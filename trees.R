## This file contains utility functions used by the Shiny app.

PROJECTION_HORIZON_YEAR <- 24
COST_BY_GENERATION_IN_BILLIONS <- c("0" = 20, "1" = 20, "2" = 16, 12)
NEW_REACTORS_BY_GENERATION <- c("1" = 1, "2" = 2, "3" = 4, 8)
NEW_PROJECT_DELAY_IN_YEARS <- 4
MAX_REACTOR_PROJECTS_PER_YEAR <- 8
NUM_TREES <- 3
STAGGER_TIME_IN_YEARS <- 1
START_DELAY_IN_YEARS <- 0

library(magrittr); library(dplyr); library(purrr); library(ggplot2)

chain <- partial(compose, .dir = "forward")

simulate_buildout <- function(
  projection_horizon_year = PROJECTION_HORIZON_YEAR,
  num_trees = NUM_TREES,
  max_reactor_projects_per_year = MAX_REACTOR_PROJECTS_PER_YEAR,
  stagger_time_in_years = STAGGER_TIME_IN_YEARS,
  start_dates = NULL,
  cost_by_generation_in_billions = COST_BY_GENERATION_IN_BILLIONS,
  new_reactors_by_generation = NEW_REACTORS_BY_GENERATION,
  new_project_delay_in_years = NEW_PROJECT_DELAY_IN_YEARS,
  start_delay_in_years = START_DELAY_IN_YEARS
) {
  build_next_generations <- function(trees, input = NULL) {
    trees %<>% mutate(tree = pmap(., build_next_generation))
    max_year <- trees %>% select(tree) %>% tidyr::unnest(tree) %$% max(year)
    if (max_year > projection_horizon_year)
      return(done(trees))
    trees
  }
  prune_excess_reactor_projects <- function(buildout) {
    buildout %>%
      group_by(year) %>%
      mutate(year_id = 1:n()) %>%
      ungroup() %>%
      filter(year_id <= max_reactor_projects_per_year) %>%
      select(- year_id)
  }
  ## TODO: define structure for and use start_dates (optional config parameter)
  trees <- tibble(
    tree_id = seq_len(num_trees),
    t_start = start_delay_in_years + stagger_time_in_years * (tree_id - 1)
  )
  while (! rlang::is_done_box(trees))
    trees %<>% build_next_generations()
  trees %<>% rlang::unbox()
  buildout <- trees %>% select(tree) %>% tidyr::unnest(tree)
  buildout %<>% prune_excess_reactor_projects()
  buildout
}

build_next_generation <- function(
  tree = NULL,
  tree_id = NA_integer_,
  cost_by_generation_in_billions = COST_BY_GENERATION_IN_BILLIONS,
  new_reactors_by_generation = NEW_REACTORS_BY_GENERATION,
  new_project_delay_in_years = NEW_PROJECT_DELAY_IN_YEARS,
  t_start = 0
) {
  if (is.null(tree))
    return(tibble(tree_id,
                  year = t_start,
                  generation = 0,
                  cost = get_by_generation(generation, cost_by_generation_in_billions)))
  curr_generation <- tree %>% slice(which.max(generation)) %>% slice(1)
  next_generation <- curr_generation %$%
    tibble(tree_id,
           year = year + new_project_delay_in_years,
           generation = generation + 1,
           cost = get_by_generation(generation, cost_by_generation_in_billions))
  next_generations <- next_generation %$% {
    list(.) %>% rep.int(get_by_generation(generation, new_reactors_by_generation)) %>% bind_rows()
  }
  bind_rows(tree, next_generations)
}

get_by_generation <- function(generation, dict) {
  if (generation %in% names(dict))
    dict[generation == names(dict)]
  else
    dict["" == names(dict)]
}

plot_buildout <- function(buildout, use_plotly = T) {
  total_cost <- buildout %$% sum(cost)
  num_trees <- buildout %$% n_distinct(tree_id)
  max_year <- buildout %$% max(year)
  buildout %<>%
    mutate(tree_id = as.character(tree_id)) %>%
    group_by(tree_id, generation) %>%
    mutate(within_generation_id = as.character(1:n())) %>%
    ungroup()
  ylab <-
    if (use_plotly)
      plotly::TeX("\\textit{Years since }t_0")
    else
      expression(paste("Years since ", t[0]))
  p <- buildout %>%
    ggplot(aes(x = 0, year, size = cost, col = tree_id)) +
    geom_point(aes(group = paste(tree_id, within_generation_id)), position = position_dodge(width = 1/8), shape = 15) +
    scale_size(range = c(3, 6)) +
    theme_minimal() +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), legend.position = "none") +
    labs(title = paste0("Tree Diagram of Simulated Buildout of ", num_trees, " plants.\nTotal Cost by Year ", max_year, ": $", prettyNum(total_cost, big.mark = ","), "B"),
         x = NULL, y = ylab)
  if (use_plotly) {
    pp <- plotly::ggplotly(p)
    plotly::config(pp, mathjax = 'cdn')
  }
  else
    p
}
