# This script gets the list of 3-digit zip codes in the US
# from the American Community Survey 2014-2018

message <- "Clean uszip3 data."
print(sprintf("Start: %s", message))

main <- function() {
  # Load data
  uszips <- read.delim(sprintf("%s/uszips.txt", raw_uszips_path))
  
  # Clean data
  uszips <- clean_uszips(uszips)
  
  # Output
  fwrite(uszips, sprintf("%s/uszips.csv", location_path))
}

clean_uszips <- function(uszips) {
  uszips <- uszips  %>%
    row_to_names(row_number = 1)  %>%
    fselect(Geo_ZCTA3)  %>%
    funique()  %>%
    fmutate(Geo_ZCTA3 = as.numeric(as.character(Geo_ZCTA3)))  %>%
    rename(zip3 = Geo_ZCTA3)  %>%
    return()
}

# Execute
main()

print(sprintf("Finish: %s", message))
