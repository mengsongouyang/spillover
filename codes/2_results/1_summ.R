# This script gets the summary stats

message <- "Get Table 1"
print(sprintf("Start: %s", message))

main <- function() {
  rownames_list <- c("N Phy",  "N Pat", "N Pres", "Pat/Phy", 
                     "Sh Red", "Sh Young", "Sh Tri", "Sh Ben", "Sh Poly")
  
  psych                  <- get_summ("psych")
  zip_psych              <- psych$zip
  cohort_psych           <- psych$cohort
  colnames(zip_psych)    <- c("Zip N", "Zip Mean", "Zip Std")
  colnames(cohort_psych) <- c("Cohort N", "Cohort Mean", "Cohort Std")
  
  nonpsych        <- get_summ("nonpsych")
  zip_nonpsych    <- nonpsych$zip
  cohort_nonpsych <- nonpsych$cohort
  colnames(zip_nonpsych)    <- c("Zip N", "Zip Mean", "Zip Std")
  colnames(cohort_nonpsych) <- c("Cohort N", "Cohort Mean", "Cohort Std")  
  
  psych_tab              <- cbind(zip_psych,    cohort_psych)
  nonpsych_tab           <- cbind(zip_nonpsych, cohort_nonpsych)
  rownames(psych_tab)    <- rownames_list
  rownames(nonpsych_tab) <- rownames_list
  
  # Output
  output_table(psych_tab,    "Summ Stats for Psychiatists",      digits = 3, results_path, "Table1A", T)
  output_table(nonpsych_tab, "Summ Stats for Non-psychiatrists", digits = 3, results_path, "Table1B", T)
}

get_summ <- function(specialty_label) {
  print(specialty_label)
  
  if (specialty_label == "psych") {
    specialty_name <- "Psychiatrists"
  } else if (specialty_label == "nonpsych") {
    specialty_name <- "Non-psychiatrists"
  } 
  
  data   <- load_data(specialty_label)
  zip    <- data$zip
  cohort <- data$cohort
  
  zip <- clean_data(zip)  %>%
    avg_acrs_yr()  %>%
    get_share()  %>%
    summ_acrs_group()
  
  cohort <- clean_data(cohort)  %>%
    avg_acrs_yr()  %>%
    get_share()  %>%
    summ_acrs_group()
  
  return(list(zip    = zip, 
              cohort = cohort))
}

load_data <- function(specialty_label) {
  if (specialty_label == "psych") {
    zip    <- fread(sprintf("%s/psych/zip_psych.csv",          zip_sum_path))
    cohort <- fread(sprintf("%s/psych/cohort_psych.csv",       cohort_sum_path))
  } else if (specialty_label == "nonpsych") {
    zip    <- fread(sprintf("%s/nonpsych/zip_nonpsych.csv",    zip_sum_path))
    cohort <- fread(sprintf("%s/nonpsych/cohort_nonpsych.csv", cohort_sum_path))
  } 
  return(list(zip    = zip, 
              cohort = cohort))
}

clean_data <- function(data) {
  colnames(data) <- c("adult", "group_code", 
                      "patient", "physician", "prescript", 
                      "FA", "tri", "ben", "poly", "red",
                      "service_yr")
  
  data <- data  %>%
    filter(adult == F)  %>%
    fselect(-adult)  %>%
    fmutate(pat_per_phy = patient / physician)  %>%
    return()
}

avg_acrs_yr <- function(data) {
  # Average across years
  data_avg <- data  %>%
    group_by(group_code)  %>%
    summarise_all(mean)  %>%
    ungroup()  %>%
    return()
} 

get_share <- function(data) {
  data <- data  %>%
    fmutate(red_sh   = red  / prescript, 
            FA_sh    = FA   / prescript, 
            tri_sh   = tri  / prescript, 
            ben_sh   = ben  / prescript, 
            poly_sh  = poly / prescript)  %>%
    return()
}

summ_acrs_group <- function(data) {
  # Get the mean and sd across the group
  
  mean <- data  %>%
    fselect(-group_code)  %>%
    summarise_all(mean)
  
  sd <- data  %>%
    fselect(-group_code)  %>%
    summarise_all(sd)
  
  n <- nrow(data)
  
  summ <- rbind(mean, sd)
  summ <- summ  %>%
    fselect(physician, patient, prescript, pat_per_phy, 
            red_sh, FA_sh, tri_sh, ben_sh, poly_sh)  %>%
    t()  %>%
    as.data.frame()  %>%
    mutate(n = n)
  colnames(summ) <- c("mean", "sd", "n")
  summ <- summ  %>% 
    fselect(n, mean, sd)  %>%
    fmutate(mean = round(mean, digits = 3), 
            sd   = round(sd,   digits = 3), 
            n    = round(n,    digits = 0))  %>%
    return()
}

# Execute
main()

print(sprintf("Finish: %s", message))
