# This script computes the sums at the (zip, year) level

message <- "Compute the sums at the (zip, year) level"
print(sprintf("Start: %s", message))

main <- function(specialty_label) {
  # Load data
  phy_spec   <- fread(sprintf("%s/physician.csv",  physician_path))  # Cleaned physician data
  phy_cohort <- fread(sprintf("%s/phy_cohort.csv", cohort_path))     # Cleaned cohort data
  
  print(specialty_label)
  for (yr in yr_list) {
    print(yr)
    
    # Load data
    prov_zip        <- fread(sprintf("%s/provider/%s.csv", location_path,        yr))  # Cleaned provider zip
    phy_n_prescript <- fread(sprintf("%s/%s.csv",          phy_n_prescript_path, yr))  # Cleaned num of prescriptions 
    
    # Keep physicians of certain specialties
    if (specialty_label == "psych") {
      physician_spec <- phy_spec  %>% filter(  PRI_SPCL_DESC %in% psychiatrist)
    } else if (specialty_label == "nonpsych") {
      physician_spec <- phy_spec  %>% filter(! PRI_SPCL_DESC %in% psychiatrist)
    }
    
    phy_n_prescript <- phy_n_prescript  %>% 
      inner_join(phy_cohort, by = "PROVIDER_ID")  %>%
      filter(med_year <= as.numeric(yr))
    
    # Get the sum of prescriptions within the zip code
    zip_sum <- get_zip_sum(phy_n_prescript, prov_zip, physician_spec)
    
    # Output
    fwrite(zip_sum, sprintf("%s/%s/%s.csv", zip_sum_path, specialty_label, yr))
  }
}

get_zip_sum <- function(phy_n_prescript, prov_zip, physician_spec) {
  zip_sum <- phy_n_prescript  %>% 
    inner_join(prov_zip, by = "PROVIDER_ID")  %>%
    filter(PROVIDER_ID %in% physician_spec$PROVIDER_ID)  %>%
    fgroup_by(adult, prov_zip3)  %>%
    fsummarise(zip_patient      = fsum(n_patient), 
               zip_physician    = fnobs(prov_zip3), 
               zip_prescript    = fsum(n_prescript), 
               zip_false_age    = fsum(n_false_age), 
               zip_tricyclics   = fsum(n_tricyclics), 
               zip_benzos       = fsum(n_benzos), 
               zip_polypharmacy = fsum(n_polypharmacy), 
               zip_red_flag     = fsum(n_red_flag))  %>%
    fungroup()  %>%
    return()
}


# Execute
main("psych")
main("nonpsych")

print(sprintf("Finish: %s", message))
