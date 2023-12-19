# This script computes the number of prescriptions of each (physician, year)

message <- "Get the num of prescriptions of each (physician, year)"
print(sprintf("Start: %s", message))

main <- function() {
  # Load data
  AA_Patient <- read.dta13(sprintf("%s/AA/AA_Patient.dta", raw_iqvia_path))  %>% fselect(PATIENT_ID, PAT_BRTH_YR_NBR)
  AD_Patient <- read.dta13(sprintf("%s/AD/AD_Patient.dta", raw_iqvia_path))  %>% fselect(PATIENT_ID, PAT_BRTH_YR_NBR)
  AA_product <- fread(sprintf("%s/AA_product.csv",         product_path))    %>% fselect(-product_name)
  AD_product <- fread(sprintf("%s/AD_product.csv",         product_path))    %>% fselect(-product_name)
  physician  <- fread(sprintf("%s/physician.csv",          physician_path))  %>% fselect(PROVIDER_ID)
  
  for (yr in yr_list) {
    print(yr)
    AA_prescript    <- load_and_clean_prescript("AA", yr, physician, AA_product, AD_product, AA_Patient, AD_Patient)
    AD_prescript    <- load_and_clean_prescript("AD", yr, physician, AA_product, AD_product, AA_Patient, AD_Patient)
    prescript       <- append_prescript(AA_prescript, AD_prescript)
    rm(AA_prescript, AD_prescript)  # Release memory
    prescript       <- identify_red_flag(prescript)
    phy_n_prescript <- summ_n_prescript(prescript)
    fwrite(phy_n_prescript, sprintf("%s/%s.csv", phy_n_prescript_path, yr))
  }
}

load_and_clean_prescript <- function(drug_type, yr, physician, AA_product, AD_product, AA_Patient, AD_Patient) {
  # Load data
  prescript <- read.dta13(sprintf("%s/%s/%s_FactRx%s.dta", raw_iqvia_path, drug_type, drug_type, yr))
  
  if (drug_type == "AA") {
    product <- AA_product
    patient <- AA_Patient
  } else if (drug_type == "AD") {
    product <- AD_product
    patient <- AD_Patient
  }

  prescript <- prescript  %>%
    fselect(PROVIDER_ID, PATIENT_ID, SVC_DT, PRODUCT_ID, DAYS_SUPPLY_CNT)  %>%
    filter(PROVIDER_ID %in% physician$PROVIDER_ID)  %>%
    fmutate(PATIENT_ID = as.numeric(PATIENT_ID))  %>%
    inner_join(product, by = "PRODUCT_ID")  %>%
    left_join (patient, by = "PATIENT_ID")  %>%
    filter(! active_ingredient %in% c("HYDROXYZINE HYDROCHLORIDE",  # HYDROXYZINE is also used for allergy
                                      "HYDROXYZINE HYDROCHLORIDE/HYDROXYZINE PAMOATE"))  %>%
    fmutate(patient_age = as.numeric(yr) - PAT_BRTH_YR_NBR)  %>%
    filter(patient_age > 0)  %>%
    fselect(PATIENT_ID, SVC_DT, PROVIDER_ID, 
            active_ingredient, class, product_age,  # Only use active_ingredient to identify the product
            patient_age, DAYS_SUPPLY_CNT)  %>%
    funique()  %>%  # Assume claims w/ same (active_ingredient, DAYS_SUPPLY_CNT) but different PRODUCT_ID and product_name are the same
    fgroup_by(PATIENT_ID, SVC_DT, PROVIDER_ID, active_ingredient, class, product_age, patient_age)  %>%
    fsummarise(DAYS_SUPPLY_CNT = fmax(DAYS_SUPPLY_CNT))  %>%  # For each (patient, provider), use the max DAYS_SUPPLY_CNT for each (active_ingredient, SVC_DT)
    fungroup()   %>%
    return()
}

append_prescript <- function(AA_prescript, AD_prescript) {
  prescript <- rbindlist(list(AA_prescript, AD_prescript))  %>%
    return()
}

identify_red_flag <- function(prescript) {
  prescript <- prescript  %>%
    fmutate(false_age        = (product_age > patient_age | product_age == 18),  # product_age == 18 is for adults
            tricyclics       = (class == "TRICYCLICS & TETRACYCLICS" | 
                                  class == "TRICYCLICS & TETRACYCLICS / BENZODIAZEPINE" | 
                                  class == "TRICYCLICS & TETRACYCLICS / CONVENTIONAL"),
            benzos           = (class == "BENZODIAZEPINES" |
                                  class == "TRICYCLICS & TETRACYCLICS / BENZODIAZEPINE"))  %>%
    filter(! (benzos == T & is.na(DAYS_SUPPLY_CNT)))  %>%
    fmutate(benzos_long_term = (benzos == T & DAYS_SUPPLY_CNT > 7))  %>%
    fselect(-benzos)  %>%
    rename(benzos = benzos_long_term)  %>%
    fselect(PROVIDER_ID, PATIENT_ID, SVC_DT, 
            class, active_ingredient, product_age, patient_age, 
            false_age, tricyclics, benzos)
  
  polypharmacy <- prescript  %>%
    fgroup_by(PROVIDER_ID, PATIENT_ID, SVC_DT)  %>%
    fsummarise(n_active_ingredient = fndistinct(active_ingredient))  %>%
    fungroup()  %>%
    fmutate(polypharmacy = (n_active_ingredient > 1))  %>%
    fselect(-n_active_ingredient)
  
  prescript <- prescript  %>%
    inner_join(polypharmacy, by = c("PATIENT_ID", "PROVIDER_ID", "SVC_DT"))  %>%
    fgroup_by(PROVIDER_ID, PATIENT_ID, SVC_DT, patient_age)  %>%
    fsummarise(false_age    = fmax(false_age), 
               tricyclics   = fmax(tricyclics), 
               benzos       = fmax(benzos), 
               polypharmacy = fmax(polypharmacy))  %>%
    fungroup()  %>%
    fmutate(red_flag = false_age + tricyclics + benzos + polypharmacy, 
            red_flag = ifelse(red_flag >= 1, 1, 0))  %>%
    return()
  
}

summ_n_prescript <- function(prescript) {
  phy_n_prescript <- prescript  %>%
    fmutate(adult = (patient_age >= 18))  %>%
    fgroup_by(PROVIDER_ID, adult)  %>%
    fsummarise(n_patient      = fnunique(PATIENT_ID), 
               n_prescript    = fnobs(PROVIDER_ID), 
               n_false_age    = fsum(false_age), 
               n_tricyclics   = fsum(tricyclics), 
               n_benzos       = fsum(benzos), 
               n_polypharmacy = fsum(polypharmacy), 
               n_red_flag     = fsum(red_flag))  %>%
    fungroup()  %>%
    return()
}

# Execute
main()

print(sprintf("Finish: %s", message))
