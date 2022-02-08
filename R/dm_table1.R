dm_table1 <- function(adsl, adlb, ls) {

  # Filter to desired observations
  x <- adlb[VISITNUM == 10 & PARAMCD == "C64849B" & FASFL == "Y"]

  # One dub subject in adlb visit 10
  x[, unique(SUBJID)] %>% length()
  dub <- x[duplicated(SUBJID)]$SUBJID
  x[SUBJID == dub]

  # Remove observation flagged
  x[ANL01FL == "N"]  %>% nrow()
  x <- x[ANL01FL == "Y"]
  dat <- merge(x, adsl, all = TRUE, by = "SUBJID")


  # No dups in adsl file
  adsl[, unique(SUBJID)] %>% length()==length(adsl)



  # Record the number of observations are missing values for each variable
  missing <- map(ls$var, function(var) {
    dat[is.na(get(var))]  %>% nrow()
  })
  names(missing) <- ls$var
  missing$TRT01P <- dat[is.na(TRT01P)] %>% nrow()
  missing$TRTP <- dat[is.na(TRTP)] %>% nrow()
  dat <- dat[!is.na(TRT01P)]
  missing$mismatch <- dat[TRT01P!=TRTP] %>%nrow()

  # Check for subjects who are in ADSL, but not in ABLB
  missing$not_in_adlb <- adsl[FASFL == "Y"][!SUBJID %in% x$SUBJID] %>% nrow()

  # Check for subjects who are in ABLB, but not in ADSL
  missing$not_in_adsl <- x[!SUBJID %in% adsl$SUBJID] %>% nrow()

  list(dat = dat, missing = missing)
}
