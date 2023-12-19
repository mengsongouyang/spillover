# Specify the packages of interest
packages <- c("data.table", 
              "tidyverse", 
              "readstata13", 
              "xlsx", 
              "collapse", 
              "lubridate", 
              "xtable", 
              "janitor", 
              "fixest",  # Fast fixed effects
              "modelr", 
              "stargazer")

# Load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
