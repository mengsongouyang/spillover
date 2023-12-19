# This script computes the sums at the (cohort, year) and the (cohort, zip, year) level

message <- "Compute the sums at the (cohort, year) and (cohort, zip, year) levels"
print(sprintf("Start: %s", message))

main <- function(specialty_label) {
  # Load data
  phy_spec   <- fread(sprintf("%s/physician.csv",  physician_path))
  phy_cohort <- fread(sprintf("%s/phy_cohort.csv", cohort_path))
  
  print(specialty_label)
  
  for (yr in yr_list) {
    print(yr)
    
    # Load data
    prov_zip        <- fread(sprintf("%s/provider/%s.csv", location_path, yr))
    phy_n_prescript <- fread(sprintf("%s/%s.csv",          phy_n_prescript_path, yr))
    
    # Keep physicians of certain specialties
    if (specialty_label == "psych") {
      phy_spec <- phy_spec  %>% filter(  PRI_SPCL_DESC %in% psychiatrist)
    } else if (specialty_label == "nonpsych") {
      phy_spec <- phy_spec  %>% filter(! PRI_SPCL_DESC %in% psychiatrist)
    }
    
    # Merge data
    physician <- merge_data(phy_n_prescript, prov_zip, phy_cohort, phy_spec, yr)
    
    # Get the sum of prescriptions at the (cohort, zip, adult) level
    cohort_zip_sum <- get_cohort_zip_sum(physician)
    
    # Get the sum of prescriptions at the (cohort, adult) level
    cohort_sum <- get_cohort_sum(physician)
    
    # Output
    fwrite(cohort_zip_sum, sprintf("%s/%s/CZ_%s.csv", cohort_sum_path, specialty_label, yr))
    fwrite(cohort_sum,     sprintf("%s/%s/C_%s.csv",  cohort_sum_path, specialty_label, yr))
  }
}

merge_data <- function(phy_n_prescript, prov_zip, phy_cohort, phy_spec, yr) {
  physician <- phy_n_prescript  %>%
    inner_join(prov_zip,   by = "PROVIDER_ID")  %>%
    inner_join(phy_cohort, by = "PROVIDER_ID")  %>% 
    filter(med_year <= as.numeric(yr))  %>%
    filter(PROVIDER_ID %in% phy_spec$PROVIDER_ID)  %>%
    return()
}

get_cohort_zip_sum <- function(physician) {
  cohort_zip_sum <- physician  %>% 
    fgroup_by(adult, cohort_code, prov_zip3)  %>%
    fsummarise(CZ_patient      = fsum(n_patient), 
               CZ_physician    = fnobs(cohort_code), 
               CZ_prescript    = fsum(n_prescript),  # `CZ` stands for cohort-zip3
               CZ_false_age    = fsum(n_false_age), 
               CZ_tricyclics   = fsum(n_tricyclics), 
               CZ_benzos       = fsum(n_benzos), 
               CZ_polypharmacy = fsum(n_polypharmacy), 
               CZ_red_flag     = fsum(n_red_flag))  %>%
    fungroup()  %>%
    return()
}

get_cohort_sum <- function(physician) {
  cohort_sum <- physician  %>% 
    fgroup_by(adult, cohort_code)  %>%
    fsummarise(C_patient      = fsum(n_patient), 
               C_physician    = fnobs(cohort_code), 
               C_prescript    = fsum(n_prescript),  # `C` stands for cohort
               C_false_age    = fsum(n_false_age), 
               C_tricyclics   = fsum(n_tricyclics), 
               C_benzos       = fsum(n_benzos), 
               C_polypharmacy = fsum(n_polypharmacy), 
               C_red_flag     = fsum(n_red_flag))  %>%
    fungroup()  %>%
    return()
}

# Execute
main("psych")
main("nonpsych")

print(sprintf("Finish: %s", message))
