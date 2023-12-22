# This script merges the derived data for the analysis

message <- "Merge all datasets."
print(sprintf("Start: %s", message))

main <- function() {
  # Load data
  # First prescription of children patients
  first_kid_prescript <- fread(sprintf("%s/first_kid_prescript.csv",       first_kid_patient_path))
  # Location of children patients
  kid_pat_loc         <- fread(sprintf("%s/patient/kid_pat_loc.csv",       location_path))
  # Cleaned physician data
  physician           <- fread(sprintf("%s/physician.csv",                 physician_path))
  # Average variables
  phy_avg_psych       <- fread(sprintf("%s/psych/phy_avg_psych.csv",       phy_avg_path))
  phy_avg_nonpsych    <- fread(sprintf("%s/nonpsych/phy_avg_nonpsych.csv", phy_avg_path))
  
  
  # Merge data
  psych    <- merge_data(first_kid_prescript, kid_pat_loc, physician, phy_avg_psych,    "psych")
  nonpsych <- merge_data(first_kid_prescript, kid_pat_loc, physician, phy_avg_nonpsych, "nonpsych")
  
  # Output
  fwrite(psych,    sprintf("%s/psych.csv",    merge_path))
  fwrite(nonpsych, sprintf("%s/nonpsych.csv", merge_path))
  
}

merge_data <- function(first_kid_prescript, kid_pat_loc, physician, phy_avg, specialty_label) {
  if (specialty_label == "psych") {
    physician <- physician  %>% filter(  PRI_SPCL_DESC %in% psychiatrist)
  } else if (specialty_label == "nonpsych") {
    physician <- physician  %>% filter(! PRI_SPCL_DESC %in% psychiatrist)
  }
  
  kid_pat_loc <- kid_pat_loc  %>%
    fmutate(PATIENT_ID = as.numeric(PATIENT_ID))

  data <- first_kid_prescript  %>%
    filter(PROVIDER_ID %in% physician$PROVIDER_ID)  %>%
    fmutate(PATIENT_ID = as.numeric(PATIENT_ID))  %>%
    inner_join(kid_pat_loc, by = c("PATIENT_ID",  "service_yr"))  %>%
    inner_join(phy_avg,     by = c("PROVIDER_ID", "service_yr"))  %>%
    fselect(PATIENT_ID, service_yr, PROVIDER_ID, 
            patient_age, female, PAT_ZIP3, 
            prov_zip3, cohort_code, med_year,    rank_group, 
            false_age, tricyclics,  benzos,      polypharmacy_day, red_flag, 
            FA_zip,    tri_zip,     ben_zip,     poly_zip,         red_zip, 
            FA_cohort, tri_cohort,  ben_cohort,  poly_cohort,      red_cohort, 
            FA_spill,  tri_spill,   ben_spill,   poly_spill,       red_spill)  %>%
    filter(med_year <= service_yr)  %>%
    return()
}

# Execute
main()

print(sprintf("Finish: %s", message))
