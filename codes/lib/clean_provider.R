clean_provider <- function(AD_Provider, AA_Provider) {
  provider_vars <- colnames(AD_Provider)
  Provider <- full_join(AD_Provider, AA_Provider, by = provider_vars)  %>%
    return()
}
