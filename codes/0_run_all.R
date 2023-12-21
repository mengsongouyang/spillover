# This script serves as a main script that runs all scripts.

# Setting(s)
options(warn   = -1, 
        scipen = 999)
specialty_label_list <- c("psych", "nonpsych")

# Set paths
# NB: Change the directory to the root directory before running programs
lib_path <- "codes/lib"
source(sprintf("%s/set_paths.R",  lib_path))

# Create directories to store cleaned data and results
source(sprintf("%s/create_dir.R", lib_path))

# Load packages
source(sprintf("%s/load_packages.R", lib_path))

# Load library functions
source(sprintf("%s/load_lib_fn.R", lib_path))

# Construct the dataset from raw data
source(sprintf("%s/1_product/1_merge.R",                         codes_data_path))
source(sprintf("%s/2_first_kid_patient/1_young_patient.R",       codes_data_path))
source(sprintf("%s/2_first_kid_patient/2_all_prescript.R",       codes_data_path))
source(sprintf("%s/2_first_kid_patient/3_first_mon_prescript.R", codes_data_path))
source(sprintf("%s/2_first_kid_patient/4_red_flag.R",            codes_data_path))
source(sprintf("%s/3_location/1_uszips.R",                       codes_data_path))
source(sprintf("%s/3_location/2_patient.R",                      codes_data_path))
source(sprintf("%s/3_location/3_provider.R",                     codes_data_path))
source(sprintf("%s/3_location/4_append.R",                       codes_data_path))
source(sprintf("%s/4_physician/1_physician.R",                   codes_data_path))
source(sprintf("%s/5_cohort/1_school.R",                         codes_data_path))
source(sprintf("%s/5_cohort/2_cohort.R",                         codes_data_path))
source(sprintf("%s/6_average/1_phy_n_prescript.R",               codes_data_path))
source(sprintf("%s/6_average/2_zip.R",                           codes_data_path))
source(sprintf("%s/6_average/3_cohort.R",                        codes_data_path))
source(sprintf("%s/6_average/4_phy_avg.R",                       codes_data_path))
source(sprintf("%s/6_average/5_append.R",                        codes_data_path))
source(sprintf("%s/7_merge/1_merge.R",                           codes_data_path))

# Analyze the cleaned datasets
source(sprintf("%s/1_summ.R", codes_results_path))
source(sprintf("%s/2_reg.R",  codes_results_path))
