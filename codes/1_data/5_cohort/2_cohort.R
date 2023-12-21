# This script gets the cohort of each physician

message <- "Get the cohort of each physician"
print(sprintf("Start: %s", message))

main <- function() {
  # Load data
  physician <- fread(sprintf("%s/physician.csv", physician_path))  # Cleaned physician data
  school    <- fread(sprintf("%s/school.csv",    cohort_path))     # Cleaned school data
  
  # Get the cohort of each physician
  phy_cohort <- physician  %>%
    filter(! is.na(ME_NBR))  %>%
    fselect(PROVIDER_ID, ME_NBR)  %>%
    impute_med_year()  %>%
    inner_join(school, by = "school_code")  %>%
    assign_cohort_code()
  
  # Output
  fwrite(phy_cohort,       sprintf("%s/phy_cohort.csv",     cohort_path))
}

impute_med_year <- function(data) {
  # Impute the graduation year from med school using the ME_NBR
  data <- data  %>%
    mutate(ME_NBR      = as.numeric(ME_NBR),
           school_code = floor(ME_NBR / 100000),
           last_5_dig  = ME_NBR %% 100000,
           med_year    = floor(last_5_dig / 1000),
           med_year    = ifelse(med_year <= 25, 2000 + med_year, 1900 + med_year))  %>%
    select(-last_5_dig)  %>%
    return()
}

assign_cohort_code <- function(phy_cohort) {
  # Assign a unique code for each cohort
  
  med_year_l <- quantile(phy_cohort$med_year, prob = 0.05)
  med_year_h <- quantile(phy_cohort$med_year, prob = 0.95)
  
  # 2-year bins
  if (med_year_l %% 2 == 0) {
    phy_cohort <- phy_cohort  %>%
      mutate(med_year_bin = ceiling(med_year / 2) * 2)
  } else {
    phy_cohort <- phy_cohort  %>%
      mutate(med_year_bin = floor(med_year / 2) * 2)
  }
  
  phy_cohort <- phy_cohort  %>%
    mutate(med_year_bin = ifelse(med_year <= med_year_l, med_year_l, med_year_bin), 
           med_year_bin = ifelse(med_year >= med_year_h, med_year_h, med_year_bin))
  
  phy_cohort$rank_group[phy_cohort$rank_group == "095-124"] <- "095+"
  phy_cohort$rank_group[phy_cohort$rank_group == "125+"]    <- "095+"
  
  cohort <- phy_cohort  %>% 
    fgroup_by(med_year_bin, rank_group)  %>%
    fsummarise(n_phy = fnobs(med_year_bin))  %>%
    fungroup()  %>%
    arrange(med_year_bin, rank_group)  %>%
    fmutate(cohort_code = 1:nrow(.))
  
  phy_cohort <- phy_cohort  %>%
    inner_join(cohort, by = c("med_year_bin", "rank_group"))  %>%
    fselect(PROVIDER_ID, cohort_code, med_year, rank_group)  %>%
    return()
}

# Execute
main()

print(sprintf("Finish: %s", message))
