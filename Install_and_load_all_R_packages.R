options(digits = 6) 
# Install and load packages (if not already installed)
packages <- c("knitr", "dplyr", "tidyr", "foreach", "doParallel", "ggplot2", 
              "gridExtra", "reshape2", "scales", "stats4", "optimx", "skimr", 
              "e1071", "forecast", "tidyverse", "WDI")

install_if_missing <- function(packages) {
  for (package in packages) {
    # Check if the package is installed; if not, install and load it
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    } else {
      # If already installed, simply load the package
      library(package, character.only = TRUE)
    }
  }
}

# Call the function to install and load the packages
install_if_missing(packages)
