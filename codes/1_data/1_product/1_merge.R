# This script merges two datasets for the drug products:
# (i) the raw anti-depressant product and anti-anxiety product datasets from IQVIA, and
# (ii) the manually filled spreadsheet with recommended age of each product.

message <- "Clean the drug products."
print(sprintf("Start: %s", message))

main <- function(drug_type) {
  print(sprintf("Drug type: %s", drug_type))
  
  # Load raw data
  # (i) Raw data from IQVIA on drug characteristics
  raw_product  <- read.dta13(sprintf("%s/%s/%s_Product.dta", raw_iqvia_path, drug_type, drug_type))
  # (ii) Manually filled spreadsheet with recommended age of each product
  product_dict <- read.xlsx(sprintf("%s/%s_product_dictionary_filled.xlsx", raw_product_path, drug_type), 1)
  
  # Clean `raw product`
  product <- raw_product  %>%
    extract_product_name()  %>%
    select(-class, -second, -third)
  
  # Harmonize product names
  if (drug_type == "AD") {
    product[product$MKTED_PROD_NM == "AMITRIPTYLINE W/PERPHENAZINE", "product_name"] <- "AMITRIPTYLINE/PERPHENAZIN"
    product[product$MKTED_PROD_NM == "SK-AMITRIPTYLINE",             "product_name"] <- "AMITRIPTYLINE"
    product[product$MKTED_PROD_NM == "TOFRANIL-PM",                  "product_name"] <- "TOFRANIL"
  } else if (drug_type == "AA") {
    product[product$MKTED_PROD_NM == "MIDAZOLAM-0.9% NACL",          "product_name"] <- "MIDAZOLAM"
    product[product$MKTED_PROD_NM == "TRANXENE-SD",                  "product_name"] <- "TRANXENE"
    product[product$MKTED_PROD_NM == "TRANXENE-T",                   "product_name"] <- "TRANXENE"
    product[product$MKTED_PROD_NM == "DI TRAN",                      "product_name"] <- "DI TRAN"
  }

  # Merge `product` and `product dictionary`
  product <- product  %>%
    left_join(product_dict, by = "product_name")  %>%
    select(PRODUCT_ID, product_name, active_ingredient, class, product_age)
  
  # Output
  fwrite(product, file = sprintf("%s/%s_product.csv", product_path, drug_type))
}

# Execute
main("AD")
main("AA")

print(sprintf("Finish: %s", message))
