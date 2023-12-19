# This script creates directories to store cleaned data and results

all_paths <- c(data_path, 
               product_path, 
               first_kid_patient_path, 
               location_path, 
               physician_path, 
               cohort_path, 
               average_path, 
               phy_n_prescript_path, 
               zip_sum_path, 
               cohort_sum_path, 
               phy_avg_path, 
               merge_path, 
               results_path)

paths_sep_by_specialty <- c(zip_sum_path, 
                            cohort_sum_path, 
                            phy_avg_path)

for (path in all_paths) {
  if (! dir.exists(path))
  {dir.create(path)}
}

if (! dir.exists(sprintf("%s/patient",  location_path))) {dir.create(sprintf("%s/patient",  location_path))}
if (! dir.exists(sprintf("%s/provider", location_path))) {dir.create(sprintf("%s/provider", location_path))}

for (path in paths_sep_by_specialty) {
  for (specialty_label in specialty_label_list) {
    if (! dir.exists(sprintf("%s/%s", path, specialty_label)))
    {dir.create(sprintf("%s/%s", path, specialty_label))}
  }
}
