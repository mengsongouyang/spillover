# This script cleans the school data

message <- "Clean school data"
print(sprintf("Start: %s", message))

main <- function() {
  # Load data
  school <- read.xlsx(sprintf("%s/AMA MED.SCHOOL.FILE.OCT2019.xlsx", raw_school_path), 1)
  
  # Rename variables
  colnames(school) <- c("school_code", "school_name", "city", "state", "zip", "country", "rank")
  
  # Impute the rank
  school$rank[is.na(school$rank)]      <- 125
  school$rank[school$rank == "95-124"] <- 110
  
  # Construct ranking groups
  school <- school  %>%
    mutate(school_code = as.numeric(school_code),
           rank = as.numeric(rank))  %>%
    mutate(rank_group = ifelse(rank <= 20, "001-020",
                            ifelse(rank >= 21 & rank <= 50, "021-050",
                                 ifelse(rank >= 51 & rank <= 94, "051-094",
                                        ifelse(rank == 110, "095-124", "125+")))))
  
  # Fill in missing country names and make country names consistent
  # for Canadian and Caribbean countries
  school <- school  %>% 
    mutate(country = ifelse(school_code > 6000   & school_code <= 7000 & is.na(country), "Canada", country), 
           country = ifelse(school_code >= 30801 & school_code <= 30817, "Dominican Republic",     country))
  
  
  # Separate foreign schools from American schools in the group 125+
  # and separate med schools in Canada and Caribbean from other foreign schools
  
  caribbean_country_list <- c("Antigua & Barbuda", 
                              # No Bahamas in the data
                              "Barbados", 
                              "Cuba", 
                              "Dominica", 
                              "Dominican Republic",
                              "Grenada", 
                              "Haiti", 
                              "Jamaica", 
                              "Nevis/St Kitts", "Saint Kitts & Nevis", 
                              "St Lucia", 
                              "St Vincent & the Grenadines", 
                              "Trinidad & Tobag")

  school <- school  %>%
    mutate(rank_group = ifelse(school_code > 6000 & country %in% c("Canada", caribbean_country_list), 
                               "Canada and Carribean", 
                               ifelse(school_code > 6000, "other foreign", rank_group)))
  
  
  
  # Change from strings to factors and relevel
  school <- school  %>%
    mutate(rank_group = as.factor(rank_group))
  school$rank_group <- factor(school$rank_group, c("001-020", "021-050" ,"051-094", "095-124", "125+", 
                                                   "Canada and Carribean", "other foreign"))

  # Output
  fwrite(school, sprintf("%s/school.csv", data_path))
}

# Execute
main()

print(sprintf("Finish: %s", message))
