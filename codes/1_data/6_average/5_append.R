# This script appends the averages of each year into one dataset

library(data.table)
library(tidyverse)
library(collapse)

message <- "Append the averages over years."
print(sprintf("Start: %s", message))

main <- function(specialty_label) {
  print(specialty_label)
  
  type_list <- c("zip", "cohort", "phy_avg")
  
  for (type in type_list) {
    print(type)
    
    if (type == "zip") {
      path <- zip_sum_path
    } else if (type == "cohort") {
      path <- cohort_sum_path
    } else if (type == "phy_avg") {
      path <- phy_avg_path
    }
    
    df <- NULL
    for (yr in yr_list) {
      print(yr)
      # Load data
      if (type == "cohort") {
        df_new <- fread(sprintf("%s/%s/C_%s.csv", path, specialty_label, yr))
      } else {
        df_new <- fread(sprintf("%s/%s/%s.csv", path, specialty_label, yr))
      }
      df_new <- df_new  %>%
        fmutate(service_yr = as.numeric(yr))
      df <- rbindlist(list(df, df_new))
    }
    fwrite(df, sprintf("%s/%s/%s_%s.csv", path, specialty_label, type, specialty_label))
  }


}

# Execute
main("psych")
main("nonpsych")

print(sprintf("Finish: %s", message))
