check_data_adsl <- function(adsl, range_vars, unique_vars) {
  adsl <- add_any_miss(adsl, missing = T, complete = F) %>% setDT()
  missing <- adsl[any_miss_all == T]
  adsl[, map(.SD, hist), .SDcols = range_vars]
  out_range<-adsl[any_miss_all==F, map(.SD, range), .SDcols = range_vars]
  out_unique<-adsl[any_miss_all==F, map(.SD, unique), .SDcols = unique_vars]
  list(any_missing = missing,
       range=out_range,
       unique=out_unique)
}
