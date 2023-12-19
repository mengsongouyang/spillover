# This script appends the averages of each year into one dataset

library(data.table)
library(tidyverse)
library(collapse)

message <- "Append the averages over years."
print(sprintf("Start: %s", message))

main <- function(specialty_label) {
  print(specialty_label)

  df <- NULL
  for (yr in yr_list) {
    print(yr)
    # Load data
    df_new <- fread(sprintf("%s/%s/%s.csv", phy_avg_path, specialty_label, yr))
    df_new <- df_new  %>%
      fmutate(service_yr = as.numeric(yr))
    df <- rbindlist(list(df, df_new))
  }
  fwrite(df, sprintf("%s/%s/phy_avg_%s.csv", phy_avg_path, specialty_label, specialty_label))

}

# Execute
main("psych")
main("nonpsych")

print(sprintf("Finish: %s", message))
