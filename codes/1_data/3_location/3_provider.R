# This script cleans the provider location.
# The zip3 w/ most patients is viewed as the zip3 of each (provider, year).

# Setting(s)
set.seed(2023)

message <- "Get providers' locations"
print(sprintf("Start: %s", message))

main <- function() {
  for (yr in yr_list) {
    print(yr)
    prov_loc <- get_prov_loc(yr)
    fwrite(prov_loc, sprintf("%s/provider/%s.csv", location_path, yr))
  }
}

get_prov_loc <- function(yr) {
  # Load raw prescription data
  AA_prescript <- read.dta13(sprintf("%s/AA/AA_FactRx%s.dta", raw_iqvia_path, yr))  %>%
    fmutate(PATIENT_ID = as.numeric(PATIENT_ID))
  AD_prescript <- read.dta13(sprintf("%s/AD/AD_FactRx%s.dta", raw_iqvia_path, yr))  %>%
    fmutate(PATIENT_ID = as.numeric(PATIENT_ID))
  
  # Append anti-anxiety and anti-depressant data
  prescript <- rbindlist(list(AA_prescript, AD_prescript))
  rm(AA_prescript, AD_prescript)  # Release memory
  
  # Load cleaned patient location data
  pat_loc <- fread(sprintf("%s/patient/%s.csv", location_path, yr))
  
  # Keep (provider, PAT_ZIP3) w/ most patients
  prov_loc <- prescript  %>%
    fselect(PROVIDER_ID, PATIENT_ID, PAT_ZIP3)  %>%  # PAT_ZIP3 is the variable name for the patient's Zip3
    filter(PROVIDER_ID != 0)  %>%
    filter(! is.na(PAT_ZIP3))  %>%
    fgroup_by(PROVIDER_ID, PATIENT_ID)  %>%
    fsummarise(n_prescript = fnobs(PATIENT_ID))  %>%  # Get the num of prescriptions for each (provider, patient) pair
    fungroup()  %>%
    left_join(pat_loc, by = "PATIENT_ID")  %>%
    fgroup_by(PROVIDER_ID, PAT_ZIP3)  %>%
    fsummarise(n_prescript = fsum(n_prescript),       # Summ the num of prescriptions at the (provider, PAT_ZIP3) level
               n_patient   = fnobs(PATIENT_ID))  %>%  # Summ the num of patients      at the (provider, PAT_ZIP3) level
    fungroup()  %>%
    filter(! is.na(PAT_ZIP3))  %>%
    fgroup_by(PROVIDER_ID)  %>%
    fmutate(max_patient = fmax(n_patient))  %>%  # For each provider, get the max num of patients among all PAT_ZIP3 where she has patients
    fungroup()  %>%
    filter(n_patient == max_patient)  %>%  # For each provider, keep the PAT_ZIP3 w/ the most patients
    fselect(-max_patient)  %>%
    fgroup_by(PROVIDER_ID)  %>%
    fmutate(n_PAT_ZIP3 = fnobs(PAT_ZIP3))  %>%  # For each provider, get the number of PAT_ZIP3 w/ most patients
    fungroup()
  
  rm(prescript)  # Release memory
  
  # Providers w/ only one PAT_ZIP3 that has the most patients
  unique_prov_loc <- prov_loc  %>%
    filter(n_PAT_ZIP3 == 1)  %>%
    fselect(PROVIDER_ID, PAT_ZIP3, n_PAT_ZIP3)  %>%
    rename(prov_zip3 = PAT_ZIP3)  %>%
    fmutate(n_PAT_ZIP3_max_pat = 1)  # Patient zip3 w/ most patients
  
  # Providers w/ more than one PAT_ZIP3 that have the most patients
  multiple_prov_loc <- prov_loc  %>%
    filter(n_PAT_ZIP3 > 1)  %>%
    fgroup_by(PROVIDER_ID)  %>%
    fmutate(max_prescript = fmax(n_prescript))  %>%
    fungroup()  %>%
    filter(max_prescript == n_prescript)  %>%
    fselect(-max_prescript)  %>%
    fgroup_by(PROVIDER_ID)  %>%
    fmutate(n_PAT_ZIP3_max_pat = fnobs(PAT_ZIP3))  %>%
    fungroup()
  
  # Providers w/ only one PAT_ZIP3 that has the most prescriptions
  # among all PAT_ZIP3 that have the most patients
  unique_prov_max_prescript <- multiple_prov_loc  %>%
    filter(n_PAT_ZIP3_max_pat == 1)  %>%
    fselect(PROVIDER_ID, PAT_ZIP3, n_PAT_ZIP3, n_PAT_ZIP3_max_pat)  %>%
    rename(prov_zip3 = PAT_ZIP3)
  
  # Providers w/ more than one PAT_ZIP3 that has the most prescriptions
  # among all PAT_ZIP3 that have the most patients
  # Then randomly choose one PAT_ZIP3 as provider's zip3
  multiple_prov_max_prescript <- multiple_prov_loc  %>%
    filter(n_PAT_ZIP3_max_pat > 1)  %>%
    group_by(PROVIDER_ID)  %>%
    sample_n(1)  %>%
    ungroup()  %>%
    fselect(PROVIDER_ID, PAT_ZIP3, n_PAT_ZIP3, n_PAT_ZIP3_max_pat)  %>%
    rename(prov_zip3 = PAT_ZIP3)
  
  # Append data
  prov_loc <- rbindlist(list(unique_prov_loc, 
                             unique_prov_max_prescript, 
                             multiple_prov_max_prescript))  %>%
    return()
}

# Execute
main()

print(sprintf("Finish: %s", message))
