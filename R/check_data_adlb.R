check_data_adlb <- function(adlb, range_vars, unique_vars) {
  adlb$ANL01REA <- NULL
  adlb <- add_any_miss(adlb, missing = T, complete = F) %>% setDT()
  missing <- adlb[any_miss_all == T]
  plots <- map(unique(adlb$PARAM), function(i){
    gghistogram(adlb[PARAM==i, AVAL],title = i)
  })
  out_range<- adlb[, range(AVAL), by = PARAM]
  list(any_missing = missing,
       range=out_range,
       plots=plots)
}
