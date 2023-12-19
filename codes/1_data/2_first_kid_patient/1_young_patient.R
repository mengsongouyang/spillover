# This script serves as a first-step filter 
# that excludes patients who cannot be below 18 in any of their prescriptions
# within the year range available (2006-2018).

message <- "Get the list of young patients."
print(sprintf("Start: %s"), message)

main <- function(drug_type) {
  print(sprintf("Drug type: %s", drug_type))
  
  PATIENT <- read.dta13(sprintf("%s/%s/%s_Patient.dta", raw_iqvia_path, drug_type, drug_type))  #  Raw data from IQVIA on patient characteristics
  
  adoles <- PATIENT  %>%
    mutate(PAT_BRTH_YR_NBR = as.numeric(PAT_BRTH_YR_NBR))  %>%
    filter(PAT_BRTH_YR_NBR >= 1988)  %>%   # Patients born before 1988 are at least 18 yrs old by 2006
    select(PATIENT_ID, PAT_BRTH_YR_NBR)
  
  fwrite(adoles, sprintf("%s/%s_young_patient.csv", first_kid_patient_path, drug_type))
}

main("AD")
main("AA")

print(sprintf("Finish: %s"), message)
