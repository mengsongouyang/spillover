extract_product_name <- function (raw_product) {
  data <- raw_product  %>%
    mutate(class = USC_DESC)  %>%
    mutate(product_name = MKTED_PROD_NM)  %>%
    separate(product_name, into = c("product_name", "second", "third"), sep = "\\s")  %>%
    return()
}
