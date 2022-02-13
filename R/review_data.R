review_data <- function(adsl, adlb) {
  range_vars <- c("AGE", "HGTBL", "WGTBL", "DIABDUR")
  unique_vars <- c("AGEU", "HGTBLU", "WGTBLU", "DIABDURU", "TRT01P")

  # Save this to show on slides
  first_check_adsl <- check_data_adsl(adsl, range_vars, unique_vars)
  adsl[which.max(HGTBL)]
  adsl[which.min(HGTBL)]
  adsl[which.max(HGTBL), HGTBL := HGTBL / 100]
  adsl[WGTBLU == "lb", WGTBL := WGTBL * 0.453592]
  adsl[WGTBLU == "lb", WGTBLU := "kg"]
  check_data_adsl(adsl, range_vars, unique_vars)


  range_vars <- c("VISITNUM", "AVAL")
  unique_vars <- c("AVALU")
  first_check_adlb <- check_data_adlb(adlb, range_vars, unique_vars)
  list(adsl = adsl,
       adlb = adlb,
       first_check_adsl=first_check_adsl,
       first_check_adlb=first_check_adlb)
}
