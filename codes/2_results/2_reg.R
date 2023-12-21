# This script runs the regressions

message <- "Get Tables 2-4"
print(sprintf("Start: %s", message))

main <- function() {
  med_year_l <- 1966  # 1966 is included as an extreme year
  med_year_h <- 2014  # 2014 is excluded from the extreme year
  
  for (specialty_label in specialty_label_list) {
    print(specialty_label)
    
    data <- load_data(specialty_label)  %>%
      clean_data(med_year_l, med_year_h)  %>%
      get_residuals(specialty_label)
    
    get_red_tables(data, specialty_label)
    get_each_type_tables(data, specialty_label)
  }
}

load_data <- function(specialty_label) {
  if (specialty_label == "psych") {
    data <- fread(sprintf("%s/psych.csv",    merge_path))
  } else if (specialty_label == "nonpsych") {
    data <- fread(sprintf("%s/nonpsych.csv", merge_path))
  } 
}

clean_data <- function(data, med_year_l, med_year_h) {
  data <- data  %>% 
    filter(med_year >  med_year_l, 
           med_year <= med_year_h)  %>%
    return()
}

get_residuals <- function(data, specialty_label) {
  # Get residuals
  
  if (specialty_label == "psych") {
    specialty_name <- "Psychiatrists"
  } else if (specialty_label == "nonpsych") {
    specialty_name <- "Non-psychiatrists"
  } 
  
  # Any red-flag
  reg_res_red <- lm("red_zip ~ red_cohort + red_spill", data)
  data <- data  %>%
    add_residuals(reg_res_red)  %>%
    rename(red_zip_res = resid)
  
  dep_mean <- round(mean(reg_res_red$model$red_zip), 3)
  
  stargazer(reg_res_red, 
            title                  = sprintf("Zip Avg on Training and Spillover (%s)", specialty_name), 
            out                    = sprintf("%s/Table2_%s.tex", results_path, specialty_label), 
            dep.var.labels.include = F, 
            covariate.labels       = c("Training", "Spillover"), 
            column.labels          = c("Zip Avg"), 
            keep.stat              = c("n", "adj.rsq"), 
            omit                   = c("Constant"), 
            add.lines              = list("Mean" = c("Dep Mean", dep_mean)))
  
  # Too young
  reg_res_FA <- lm("FA_zip ~ FA_cohort + FA_spill", data)
  data <- data  %>%
    add_residuals(reg_res_FA)  %>%
    rename(FA_zip_res = resid)
  
  # Tricyclics
  reg_res_tri <- lm("tri_zip ~ tri_cohort + tri_spill", data)
  data <- data  %>%
    add_residuals(reg_res_tri)  %>%
    rename(tri_zip_res = resid)
  
  # Benzos
  reg_res_ben <- lm("ben_zip ~ ben_cohort + ben_spill", data)
  data <- data  %>%
    add_residuals(reg_res_ben)  %>%
    rename(ben_zip_res = resid)
  
  # Polypharmacy
  reg_res_poly <- lm("poly_zip ~ poly_cohort + poly_spill", data)
  data <- data  %>%
    add_residuals(reg_res_poly)  %>%
    rename(poly_zip_res = resid)  %>%
    return()
}

get_red_tables <- function(data, specialty_label) {
  if (specialty_label == "psych") {
    specialty_name <- "Psychiatrists"
  } else if (specialty_label == "nonpsych") {
    specialty_name <- "Non-psychiatrists"
  } 
  
  # This function gets Table 3
  reg1 <- feols(red_flag ~ red_zip_res + red_cohort + red_spill + patient_age + female | service_yr + PAT_ZIP3 ,              data, cluster = ~prov_zip3, notes = F)
  reg2 <- feols(red_flag ~ red_zip_res + red_cohort + red_spill + patient_age + female | service_yr + PAT_ZIP3 + PROVIDER_ID, data, cluster = ~prov_zip3, notes = F)
  reg3 <- feols(red_flag ~               red_cohort + red_spill + patient_age + female | service_yr + PAT_ZIP3 + PROVIDER_ID, data, cluster = ~prov_zip3, notes = F)

  myDict <- c("red_flag"    = "Any red-flag",
              "red_zip_res" = "Zip Residuals",
              "red_cohort"  = "Training",
              "red_spill"   = "Spillover", 
              "service_yr"  = "Year", 
              "PAT_ZIP3"    = "Patient Zip3", 
              "PROVIDER_ID" = "Physician", 
              "patient_age" = "Patient Age", 
              "femaleTRUE"  = "Female Patient", 
              "prov_zip3"   = "Physician Zip3")
  
  myOrder <- c("Zip Residuals", "Training", "Spillover")
  
  if (specialty_label == "psych") {
    panel <- "A"
  } else if (specialty_label == "nonpsych") {
    panel <- "B"
  }
  
  tab <- etable(list(reg1, reg2, reg3),
         drop      = "Const",
         dict      = myDict,
         order     = myOrder,
         file      = sprintf("%s/Table3%s.tex", results_path, panel),
         replace   = T,
         title     = sprintf("Any red-flag (%s)", specialty_name),
         style.tex = style.tex(tpt = TRUE),
         fitstat   = c("n", "my", "ar2"), 
         digits    = "r3",
         extralines = )
  print(tab)
}

get_each_type_tables <- function(data, specialty_label) {
  # This function gets Table 4
  
  if (specialty_label == "psych") {
    specialty_name <- "Psychiatrists"
  } else if (specialty_label == "nonpsych") {
    specialty_name <- "Non-psychiatrists"
  } 
  
  # Create a dataset for each type of red-flag prescription and
  # rename the variable names to generic names
  FA_data <- data  %>%
    rename(zip_res = FA_zip_res, 
           cohort  = FA_cohort, 
           spill   = FA_spill)
  
  tri_data <- data  %>%
    rename(zip_res = tri_zip_res, 
           cohort  = tri_cohort, 
           spill   = tri_spill)
  
  ben_data <- data  %>%
    rename(zip_res = ben_zip_res, 
           cohort  = ben_cohort, 
           spill   = ben_spill)
  
  poly_data <- data  %>%
    rename(zip_res = poly_zip_res, 
           cohort  = poly_cohort, 
           spill   = poly_spill)
  
  # Run regressions
  reg1 <- feols(false_age        ~ zip_res + cohort + spill + patient_age + female | service_yr + PAT_ZIP3 + PROVIDER_ID, FA_data,   cluster = ~prov_zip3, notes = F)
  reg2 <- feols(tricyclics       ~ zip_res + cohort + spill + patient_age + female | service_yr + PAT_ZIP3 + PROVIDER_ID, tri_data,  cluster = ~prov_zip3, notes = F)
  reg3 <- feols(benzos           ~ zip_res + cohort + spill + patient_age + female | service_yr + PAT_ZIP3 + PROVIDER_ID, ben_data,  cluster = ~prov_zip3, notes = F)
  reg4 <- feols(polypharmacy_day ~ zip_res + cohort + spill + patient_age + female | service_yr + PAT_ZIP3 + PROVIDER_ID, poly_data, cluster = ~prov_zip3, notes = F)
  
  # Format and output the table
  myDict <- c("false_age"        = "Too young", 
              "tricyclics"       = "Tricyclics", 
              "benzos"           = "Benzos", 
              "polypharmacy_day" = "Polypharmacy", 
              "zip_res"          = "Zip Residuals",
              "cohort"           = "Training",
              "spill"            = "Spillover", 
              "service_yr"       = "Year", 
              "PROVIDER_ID"      = "Physician", 
              "PAT_ZIP3"         = "Patient Zip3", 
              "patient_age"      = "Patient Age", 
              "femaleTRUE"       = "Female Patient", 
              "prov_zip3"        = "Physician Zip3")
  
  myOrder <- c("Zip Residuals", "Training", "Spillover")
  
  if (specialty_label == "psych") {
    panel <- "A"
  } else if (specialty_label == "nonpsych") {
    panel <- "B"
  }
  
  tab <- etable(list(reg1, reg2, reg3, reg4),
         drop      = "Const",
         dict      = myDict,
         order     = myOrder,
         file      = sprintf("%s/Table4%s.tex", results_path, panel),
         replace   = T,
         title     = sprintf("Each type of prescription (%s)", specialty_name),
         style.tex = style.tex(tpt = TRUE),
         fitstat   = c("n", "my", "ar2"), 
         digits    = "r3",
         extralines = )
  print(tab)
}

# Execute
main()

print(sprintf("Finish: %s", message))
