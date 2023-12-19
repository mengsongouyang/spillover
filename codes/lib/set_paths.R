# This script sets the paths

# `Codes` folder
codes_path         <- "codes"
codes_data_path    <- sprintf("%s/1_data",    codes_path) 
codes_results_path <- sprintf("%s/2_results", codes_path)

# `0_raw_data` folder
raw_path         <- "0_raw_data"
raw_iqvia_path   <- sprintf("%s/iqvia/_raw_data", raw_path)
raw_product_path <- sprintf("%s/product",         raw_path)
raw_uszips_path  <- sprintf("%s/uszips",          raw_path)
raw_school_path  <- sprintf("%s/school",          raw_path)

# `1_data` folder
data_path              <- "1_data"
product_path           <- sprintf("%s/1_product",                   data_path)
first_kid_patient_path <- sprintf("%s/2_first_kid_patient",         data_path)
location_path          <- sprintf("%s/3_location",                  data_path)
physician_path         <- sprintf("%s/4_physician",                 data_path)
cohort_path            <- sprintf("%s/5_cohort",                    data_path)
average_path           <- sprintf("%s/6_average",                   data_path)
phy_n_prescript_path   <- sprintf("%s/6_average/1_phy_n_prescript", data_path)
zip_sum_path           <- sprintf("%s/6_average/2_zip",             data_path)
cohort_sum_path        <- sprintf("%s/6_average/3_cohort",          data_path)
phy_avg_path           <- sprintf("%s/6_average/4_phy_avg",         data_path)
merge_path             <- sprintf("%s/7_merge",                     data_path)

# `2_results` folder
results_path <- "2_results"
