# This script cleans the patient location.
# The zip3 w/ most prescriptions is viewed as the zip3 of each (patient, year).

# Setting(s)
set.seed(2023)

message <- "Get patients' locations"
print(sprintf("Start: %s", message))

main <- function() {
  # Load data
  uszips <- fread(sprintf("%s/uszips.csv", location_path))  # Cleaned list of US zip3
  
  for (yr in yr_list) {
    print(yr)
    pat_loc <- get_pat_loc(yr, uszips)
    fwrite(pat_loc, sprintf("%s/patient/%s.csv", location_path, yr))
  }
}

get_pat_loc <- function(yr, uszips) {
  # This function gets the modal location of each (patient, year)
  
  # Load raw prescription data
  AA_prescript <- read.dta13(sprintf("%s/AA/AA_FactRx%s.dta", raw_iqvia_path, yr))  %>%
    fmutate(PATIENT_ID = as.numeric(PATIENT_ID))
  AD_prescript <- read.dta13(sprintf("%s/AD/AD_FactRx%s.dta", raw_iqvia_path, yr))  %>%
    fmutate(PATIENT_ID = as.numeric(PATIENT_ID))
  
  # Append anti-anxiety and anti-depressant data
  prescript <- rbindlist(list(AA_prescript, AD_prescript))
  rm(AA_prescript, AD_prescript)  # Release memory
  
  # Summarize patient location from prescriptions
  pat_loc <- prescript  %>%
    filter(! is.na(PAT_ZIP3))  %>%
    filter(PAT_ZIP3 %in% uszips$zip3)  %>%
    fgroup_by(PATIENT_ID, PAT_ZIP3)  %>%
    fsummarise(n_prescript = fnobs(PATIENT_ID))  %>%  # Get the num of prescriptions at the (patient, zip3) level
    fungroup()  %>%
    fgroup_by(PATIENT_ID)  %>%
    fmutate(max_prescript = fmax(n_prescript))  %>%  # For each patient, get the max num of prescriptions among all zip3s where she has prescriptions
    fungroup()  %>%
    filter(n_prescript == max_prescript)  %>%  # For each patient, keep the zip3 w/ most prescriptions
    fselect(-max_prescript)  %>%
    fgroup_by(PATIENT_ID)  %>%
    fmutate(n_PAT_ZIP3 = fnobs(PAT_ZIP3))  %>%  # For each patient, get the number of zip3 w/ most prescriptions
    fungroup()
  
  rm(prescript)  # Release memory
  
  # Patients with unique modal location
  unique_pat_loc <- pat_loc  %>%
    filter(n_PAT_ZIP3 == 1)  %>%
    fselect(PATIENT_ID, PAT_ZIP3)
  
  # For patients with multiple modal locations,
  # randomly pick one location if there are ties
  multiple_pat_loc <- pat_loc  %>%
    filter(n_PAT_ZIP3 > 1)  %>%
    group_by(PATIENT_ID)  %>%
    sample_n(1)  %>%
    ungroup()  %>%
    fselect(PATIENT_ID, PAT_ZIP3)
  
  pat_loc <- rbindlist(list(unique_pat_loc, multiple_pat_loc))  %>%
    return()
}

# Execute
main()

print(sprintf("Finish: %s", message))
