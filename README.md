# Nuclear Buildout Simulation

## Installation

1. Install R and RStudio
1. In RStudio, click `File > New Project > Version Control > Git`
1. Enter in this Repository URL: [git@github.com:arun-ramamurthy/nuclear-trees.git](git@github.com:arun-ramamurthy/nuclear-trees.git)
1. Click through the remainder of the installation wizard, and open the new `nuclear-trees` project in RStudio
1. In RStudio, go to the R console tab in the bottom-right region in the screen, and enter the following commands:
1. `install.package("renv")`
1. `renv::restore()`

## Use

1. In RStudio, click `File > Open File...` and open the file called `trees.R`
1. Modify the parameters as desired (i.e. the all-caps variables near the top of `trees.R`)
1. In the upper-right corner of the `trees.R` pane, click `Source`
1. To run the analysis, enter the following command in the RStudio R console tab: `plot_buildout(simulate_buildout())`

## Help

Please direct any questions about this tool to [Arun Ramamurthy](arun@arun.run)
