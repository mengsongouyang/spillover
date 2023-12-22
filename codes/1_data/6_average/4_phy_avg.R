# This script computes 
# (i)   the leave-physician-out zip code average
# (ii)  the out-of-zip-code cohort average (i.e. the training variable), and
# (iii) the spillover variable
# at the physician level

# Load data
phy_cohort <- fread(sprintf("%s/phy_cohort.csv", cohort_path))

message <- "Compute the averages at the physician level."
print(sprintf("Start: %s", message))

main <- function(specialty_label) {
  print(specialty_label)
  
  for (yr in yr_list) {
    print(yr)
    
    # Load data
    # Cleaned num of prescriptions
    phy_n_prescript <- fread(sprintf("%s/%s.csv",          phy_n_prescript_path, yr))
    # Provider zip
    prov_zip        <- fread(sprintf("%s/provider/%s.csv", location_path,        yr))
    # Zip sum, (cohort, zip) sum, and cohort sum
    zip_sum         <- fread(sprintf("%s/%s/%s.csv",       zip_sum_path,         specialty_label, yr))
    CZ_sum          <- fread(sprintf("%s/%s/CZ_%s.csv",    cohort_sum_path,      specialty_label, yr))
    C_sum           <- fread(sprintf("%s/%s/C_%s.csv",     cohort_sum_path,      specialty_label, yr))
    
    # Drop physicians who prescribe before graduation
    phy_n_prescript <- phy_n_prescript  %>%
      left_join(phy_cohort, by = "PROVIDER_ID")  %>%
      filter(med_year <= as.numeric(yr))  %>%
      select(-cohort_code, -med_year, -rank_group)
    
    # Keep relevant variables
    prov_zip <- prov_zip  %>%
      fselect(PROVIDER_ID, prov_zip3)
    
    # Get averages
    # Leave-physician-out zip code average
    zip_avg <- get_zip_avg(phy_n_prescript, prov_zip, zip_sum)
    # Out-of-zip-code cohort average
    cohort_avg <- get_cohort_avg(phy_n_prescript, prov_zip, phy_cohort, CZ_sum, C_sum)
    
    # Merge data
    phy_avg <- zip_avg  %>%
      full_join(cohort_avg, by = "PROVIDER_ID")  %>%
      left_join(prov_zip,   by = "PROVIDER_ID")  %>%
      left_join(phy_cohort, by = "PROVIDER_ID")  %>%
      get_spillover()  %>%
      fselect(PROVIDER_ID, prov_zip3,  cohort_code, med_year,    rank_group, 
              FA_zip,      tri_zip,    ben_zip,     poly_zip,    red_zip, 
              FA_cohort,   tri_cohort, ben_cohort,  poly_cohort, red_cohort, 
              FA_spill,    tri_spill,  ben_spill,   poly_spill,  red_spill)
    
    # Output
    fwrite(phy_avg, sprintf("%s/%s/%s.csv", phy_avg_path, specialty_label, yr))
  }
}

get_zip_avg <- function(data, prov_zip, zip_sum) {
  # Get the leave-physician-out zip code average
  data <- data  %>%
    inner_join(prov_zip, by = "PROVIDER_ID")  %>%
    inner_join(zip_sum,  by = c("prov_zip3", "adult"))  %>%
    fmutate(lo_prescript = zip_prescript - n_prescript,  # `lo` stands for leave-out
            FA_zip   = (zip_false_age    - n_false_age)    / lo_prescript, 
            tri_zip  = (zip_tricyclics   - n_tricyclics)   / lo_prescript, 
            ben_zip  = (zip_benzos       - n_benzos)       / lo_prescript, 
            poly_zip = (zip_polypharmacy - n_polypharmacy) / lo_prescript, 
            red_zip  = (zip_red_flag     - n_red_flag)     / lo_prescript)  %>%
    fselect(PROVIDER_ID, adult, FA_zip, tri_zip, ben_zip, poly_zip, red_zip)  %>%
    filter(adult == F)  %>%
    fselect(-adult)  %>%
    return()
}

get_cohort_avg <- function(data, prov_zip, phy_cohort, CZ_sum, C_sum) {
  # Get the out-of-zip-code cohort average
  data <- data  %>%
    inner_join(prov_zip,   by = "PROVIDER_ID")  %>%
    inner_join(phy_cohort, by = "PROVIDER_ID")  %>%
    inner_join(C_sum,      by = c("cohort_code", "adult"))  %>%
    inner_join(CZ_sum,     by = c("cohort_code", "prov_zip3", "adult"))  %>%
    fmutate(OOZ_prescript = C_prescript     - CZ_prescript,  # `OOZ` stands for out-of-zip-code
            FA_cohort     = (C_false_age    - CZ_false_age)    / OOZ_prescript, 
            tri_cohort    = (C_tricyclics   - CZ_tricyclics)   / OOZ_prescript, 
            ben_cohort    = (C_benzos       - CZ_benzos)       / OOZ_prescript, 
            poly_cohort   = (C_polypharmacy - CZ_polypharmacy) / OOZ_prescript, 
            red_cohort    = (C_red_flag     - CZ_red_flag)     / OOZ_prescript)  %>%
    fselect(PROVIDER_ID, adult, FA_cohort, tri_cohort, ben_cohort, poly_cohort, red_cohort)  %>%
    filter(adult == F)  %>%
    fselect(-adult)  %>%
    return()
}

get_spillover <- function(data) {
  # Get the spillover variable of each (cohort, zip)
  CZ_avg_sum <- data  %>% 
    fgroup_by(cohort_code, prov_zip3)  %>%
    fsummarise(CZ_phy  = fnobs(FA_cohort),
               CZ_FA   = fsum(FA_cohort), 
               CZ_tri  = fsum(tri_cohort), 
               CZ_ben  = fsum(ben_cohort), 
               CZ_poly = fsum(poly_cohort), 
               CZ_red  = fsum(red_cohort))  %>%
    fungroup()
  
  Z_avg_sum <- data  %>% 
    fgroup_by(prov_zip3)  %>%
    fsummarise(Z_phy  = fnobs(FA_cohort),
               Z_FA   = fsum(FA_cohort), 
               Z_tri  = fsum(tri_cohort), 
               Z_ben  = fsum(ben_cohort), 
               Z_poly = fsum(poly_cohort), 
               Z_red  = fsum(red_cohort))  %>%
    fungroup()
  
  data <- data  %>%
    inner_join(Z_avg_sum,  by = "prov_zip3")  %>%
    inner_join(CZ_avg_sum, by = c("cohort_code", "prov_zip3"))  %>%
    fmutate(spill_phy  = Z_phy   - CZ_phy,  # `spill` stands for spillover
            FA_spill   = (Z_FA   - CZ_FA)   / spill_phy, 
            tri_spill  = (Z_tri  - CZ_tri)  / spill_phy, 
            ben_spill  = (Z_ben  - CZ_ben)  / spill_phy, 
            poly_spill = (Z_poly - CZ_poly) / spill_phy, 
            red_spill  = (Z_red  - CZ_red)  / spill_phy)
  data[sapply(data, is.infinite)] <- NA
  return(data)
}
  
# Execute
main("psych")
main("nonpsych")

print(sprintf("Finish: %s", message))
