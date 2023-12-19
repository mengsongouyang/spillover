# This script keeps children patients' prescriptions
# in the first month since the patient appears in the dataset

message <- "Keep kids' first month of prescriptions."
print(sprintf("Start: %s", message))

main <- function(drug_type) {
  print(sprintf("Drug type: %s", drug_type))
  
  # Load data
  product       <- fread(sprintf("%s/%s_product.csv",           product_path,           drug_type))  # Cleaned drug data
  all_prescript <- fread(sprintf("%s/%s_all_prescript_kid.csv", first_kid_patient_path, drug_type))  # All prescriptions of children patients
  
  # Clean and merge data
  first_mon_prescript <- all_prescript  %>%
    fmutate(PATIENT_ID = as.numeric(PATIENT_ID))  %>%
    left_join(product, by = "PRODUCT_ID")  %>%
    filter(! active_ingredient %in% c("HYDROXYZINE HYDROCHLORIDE",  # HYDROXYZINE is also used for allergy
                                      "HYDROXYZINE HYDROCHLORIDE/HYDROXYZINE PAMOATE"))  %>%
    fselect(PATIENT_ID, SVC_DT, PROVIDER_ID, 
            active_ingredient, class, product_age, DAYS_SUPPLY_CNT, # Only use active_ingredient to identify the product; 
                                                                    # drop PRODUCT_ID, product_name, and CLAIM_ID
            PAT_ZIP3, PAT_BRTH_YR_NBR)  %>%
    funique()  %>%  # Assume claims w/ same (active_ingredient, DAYS_SUPPLY_CNT) but different PRODUCT_ID and product_name are the same
    fgroup_by(PATIENT_ID, SVC_DT, PROVIDER_ID, active_ingredient, class, product_age, PAT_ZIP3, PAT_BRTH_YR_NBR)  %>%
    fsummarise(DAYS_SUPPLY_CNT = fmax(DAYS_SUPPLY_CNT))  %>%  # For each (patient, provider), use the max DAYS_SUPPLY_CNT for each (active_ingredient, SVC_DT)
    fungroup()  %>%
    fmutate(SVC_DT = as.character(SVC_DT), 
            SVC_DT = as.Date(SVC_DT, format = "%Y%m%d"))  %>%
    fgroup_by(PATIENT_ID)  %>%
    fmutate(first_appear = fmin(SVC_DT),
            last_appear  = fmax(SVC_DT))  %>%
    fungroup()  %>%
    fmutate(first_month = ymd(first_appear) %m+% months(1))  %>%
    filter(SVC_DT < first_month)  %>%
    fselect(-first_month)
  
  # Output
  fwrite(first_mon_prescript, sprintf("%s/%s_first_mon_prescript_kid.csv", first_kid_patient_path, drug_type))
}

# Execute
main("AD")
main("AA")

print(sprintf("Start: %s", message))
