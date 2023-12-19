# This script keeps all prescriptions (both first-time and follow-ups) of children patients.

message <- "Extract prescriptions of children patients."
print(sprintf("Start: %s", message))

main <- function(drug_type) {
  print(sprintf("Drug type: %s", drug_type))
  
  # Load list of patients who might be children patients
  young_patient <- fread(sprintf("%s/%s_young_patient.csv", first_kid_patient_path, drug_type))
  
  prescript  <- NULL
  for (yr in yr_list) {
    print(yr)
    
    # Extract prescriptions of children patients
    prescript <- get_prescript(prescript, yr, drug_type, young_patient)
    
    # Output
    fwrite(prescript, sprintf("%s/%s_all_prescript_kid.csv", first_kid_patient_path, drug_type))
  }
  
}

get_prescript <- function(prescript, yr, drug_type, young_patient) {
  # This function extract prescriptions of children patients

  raw <- read.dta13(sprintf("%s/%s/%s_FactRx%s.dta", raw_iqvia_path, drug_type, drug_type, yr))  %>%
    fmutate(PATIENT_ID = as.numeric(PATIENT_ID))
  
  young_patient <- young_patient  %>%
    fmutate(PATIENT_ID = as.numeric(PATIENT_ID))
  
  prescript_new <- raw  %>%
    inner_join(young_patient, by = "PATIENT_ID")  %>%  # Keep only young patients
    fmutate(service_yr  = floor(SVC_DT / 10000),
            patient_age = service_yr - PAT_BRTH_YR_NBR,
            adult       = (patient_age >= 18))  %>%  # To be conservative, patient_age == 18 are considered as adults. 
                                                     # By doing so, patient_age < 18 are kids for sure.
    filter(adult == F)  %>%
    fselect(PATIENT_ID, SVC_DT, CLAIM_ID, PROVIDER_ID, PRODUCT_ID, DAYS_SUPPLY_CNT, PAT_BRTH_YR_NBR, PAT_ZIP3)
  
  prescript <- rbindlist(list(prescript, prescript_new))  %>%
    return()
}

main("AD")
main("AA")

print(sprintf("Finish: %s", message))
