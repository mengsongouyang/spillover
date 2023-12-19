# This script keeps only physicians in the provider sample

message <- "Keep only physicians in the provider sample."
print(sprintf("Start: %s", message))

main <- function() {
  # Load data
  AA_Provider <- read_dta(sprintf("%s/AA/AA_Provider.dta", raw_iqvia_path))
  AD_Provider <- read_dta(sprintf("%s/AD/AD_Provider.dta", raw_iqvia_path))
  
  # Append data
  provider  <- append_provider(AA_Provider, AD_Provider)
  
  # Keep physicians
  physician <- keep_physician(provider)
  
  # Output
  fwrite(physician, sprintf("%s/physician.csv", physician_path))
}

append_provider <- function(AA_Provider, AD_Provider) {
  provider <- rbindlist(list(AA_Provider, AD_Provider))  %>%
    fselect(PROVIDER_ID, npi, ME_NBR, PRI_SPCL_DESC)  %>%
    funique()  %>%
    return()
}

keep_physician <- function(provider) {
  # Keeps only physicians in the provider sample
  physician <- provider  %>%
    filter(! is.na(npi))  %>%
    filter(! PRI_SPCL_DESC  %in% non_physician_specialty)  %>%
    return()
}

# Execute
main()

print(sprintf("Finish: %s", message))
