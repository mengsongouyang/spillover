# This script generates variables for "red-flag" prescriptions
# and keeps only the first prescription of each child patient.

message <- "Gen vars for red-flag prescriptions."
print(sprintf("Start: %s", message))

main <- function() {
  # Get patient characteristics
  AD_patient <- clean_patient("AD")
  AA_patient <- clean_patient("AA")
  patient    <- rbindlist(list(AD_patient, AA_patient))  %>% funique()
  rm(AD_patient, AA_patient)  # Release memory
  
  # Append and clean prescriptions
  prescript <- append_prescript()  %>%
    clean_prescript(patient)
  
  # Keep patient list
  patient_list <- prescript  %>% fselect(PATIENT_ID)

  # Output
  fwrite(prescript,    sprintf("%s/first_kid_prescript.csv", first_kid_patient_path))
  fwrite(patient_list, sprintf("%s/first_kid_patient.csv",   first_kid_patient_path))
}

clean_patient <- function(drug_type) {
  # This function keeps characteristics of young patients
  # Load data
  young_patient <- fread(sprintf("%s/%s_young_patient.csv",  first_kid_patient_path, drug_type))             # List of patients who might be children patients
  raw_patient   <- read.dta13(sprintf("%s/%s/%s_Patient.dta", raw_iqvia_path, drug_type, drug_type))  # Raw data from IQVIA on patient characteristics
  
  # Clean patient info
  young_patient <- young_patient  %>%
    fmutate(PATIENT_ID = as.numeric(PATIENT_ID))
  
  # Keep young patients and patient characteristics
  patient <- raw_patient  %>%
    filter(PATIENT_ID %in% young_patient$PATIENT_ID)  %>%
    fselect(PATIENT_ID, PAT_BRTH_YR_NBR, PAT_GENDER_CD)  %>%
    return()
}

append_prescript <- function() {
  # Load cleaned data on the first month of prescriptions of children patients
  AD_prescript <- fread(sprintf("%s/AD_first_mon_prescript_kid.csv", first_kid_patient_path))
  AA_prescript <- fread(sprintf("%s/AA_first_mon_prescript_kid.csv", first_kid_patient_path))
  
  # Append data
  prescript <- rbindlist(list(AD_prescript, AA_prescript))  %>%
    fgroup_by(PATIENT_ID)  %>%
    fmutate(first_appear = fmin(first_appear),  # Update the first-appear and last-appear dates after appending AD and AA prescriptions
            last_appear  = fmax(last_appear))  %>%
    fungroup()  %>%
    fmutate(first_month  = ymd(first_appear) %m+% months(1))  %>%
    filter(SVC_DT < first_month)  %>%
    fselect(-first_month)  %>%
    return()
 }

clean_prescript <- function(prescript, patient) {
  patient <- patient  %>%
    fmutate(PATIENT_ID = as.numeric(PATIENT_ID))
  
  prescript <- prescript  %>%
    filter(first_appear >= as.Date("2006-07-01"))  %>%
    fselect(-PAT_BRTH_YR_NBR)  %>%
    fmutate(PATIENT_ID = as.numeric(PATIENT_ID))  %>%
    left_join(patient, by = "PATIENT_ID")  %>%
    fmutate(service_yr       = as.numeric(format(SVC_DT, "%Y")),
            patient_age      = service_yr - PAT_BRTH_YR_NBR,
            adult            = (patient_age >= 18),
            female           = (PAT_GENDER_CD == "F"),
            false_age        = (product_age >= patient_age),
            tricyclics       = (class == "TRICYCLICS & TETRACYCLICS" | 
                                  class == "TRICYCLICS & TETRACYCLICS / BENZODIAZEPINE" | 
                                  class == "TRICYCLICS & TETRACYCLICS / CONVENTIONAL"),
            benzos           = (class == "BENZODIAZEPINES" |
                                  class == "TRICYCLICS & TETRACYCLICS / BENZODIAZEPINE"))  %>%
    filter(! (benzos == T & is.na(DAYS_SUPPLY_CNT)))  %>%
    fmutate(benzos_long_term = (benzos == T & DAYS_SUPPLY_CNT > 7))  %>%
    fselect(-benzos)  %>%
    rename(benzos = benzos_long_term)  %>%
    filter(patient_age > 0)  %>%
    fselect(PATIENT_ID, PROVIDER_ID, 
            SVC_DT, service_yr, first_appear, last_appear, 
            class, active_ingredient, product_age, 
            PAT_BRTH_YR_NBR, patient_age, female, 
            false_age, tricyclics, benzos)
  
  polypharmacy_day <- prescript  %>%
    filter(SVC_DT == first_appear)  %>%
    fgroup_by(PATIENT_ID)  %>%
    fsummarise(n_active_ingredient = fndistinct(active_ingredient))  %>%
    fungroup()  %>%
    fmutate(polypharmacy_day = (n_active_ingredient > 1))  %>%
    fselect(-n_active_ingredient)
  
  prescript <- prescript  %>%
    inner_join(polypharmacy_day,   by = "PATIENT_ID")  %>%
    filter(SVC_DT == first_appear)  %>%
    fgroup_by(PATIENT_ID, PROVIDER_ID, 
              SVC_DT, service_yr, first_appear, last_appear, 
              PAT_BRTH_YR_NBR, patient_age, female)  %>%
    fsummarise(false_age          = fmax(false_age), 
               tricyclics         = fmax(tricyclics), 
               benzos             = fmax(benzos), 
               polypharmacy_day   = fmax(polypharmacy_day))  %>%
    fmutate(red_flag = false_age + tricyclics + benzos + polypharmacy_day, 
            red_flag = ifelse(red_flag >= 1, 1, 0))  %>%
    fungroup()
  
  multiple_provider <- prescript  %>%
    fgroup_by(PATIENT_ID)  %>%
    fsummarise(n_provider = fndistinct(PROVIDER_ID))  %>%
    fungroup()  %>%
    filter(n_provider > 1)
  
  prescript <- prescript  %>%
    filter(! PATIENT_ID %in% multiple_provider$PATIENT_ID)  %>%
    filter(PROVIDER_ID != 0)  %>%
    return()
}

# Execute
main()

print(sprintf("Finish: %s", message))
