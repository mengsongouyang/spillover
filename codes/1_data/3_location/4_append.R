# This script appends the location of children patients over years

message <- "Append kids' locations over years"
print(sprintf("Start: %s", message))

main <- function() {
  # Load data
  first_kid_patient <- fread(sprintf("%s/first_kid_patient.csv", first_kid_patient_path))
  
  # Append the patient location over years
  kid_pat_loc <- get_kid_pat_loc(first_kid_patient)
  
  # Output
  fwrite(kid_pat_loc, sprintf("%s/patient/kid_pat_loc.csv", location_path))
}

get_kid_pat_loc <- function(first_kid_patient) {
  # Append the children patients' location over years
  first_kid_patient <- first_kid_patient  %>%
    fmutate(PATIENT_ID = as.numeric(PATIENT_ID))
  
  pat_loc <- NULL
  for (yr in yr_list) {
    print(yr)
    pat_loc_new <- fread(sprintf("%s/patient/%s.csv", location_path, yr))  %>%
      fmutate(PATIENT_ID = as.numeric(PATIENT_ID))  %>%
      filter(PATIENT_ID %in% first_kid_patient$PATIENT_ID)  %>%
      fmutate(service_yr = as.numeric(yr))
    pat_loc <- rbindlist(list(pat_loc, pat_loc_new))
  }
  return(pat_loc)
}

# Execute
main()

print(sprintf("Finish: %s", message))
