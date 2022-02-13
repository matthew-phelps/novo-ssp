dm_table1 <- function(adsl, adlb, ls) {

  # Filter to desired observations
  adlb <- adlb[VISITNUM == 10 & PARAMCD == "C64849B" & FASFL == "Y"]

  # One dublicate subject in ADLB visit 10
  adlb[, unique(SUBJID)] %>% length()
  dub <- adlb[duplicated(SUBJID)]$SUBJID
  adlb[SUBJID == dub]

  # Remove observation flagged
  adlb[ANL01FL == "N"]  %>% nrow()
  adlb <- adlb[ANL01FL == "Y"]

  # No duplicates in ADSL file
  adsl[, unique(SUBJID)] %>% length()==length(adsl)

  dat <- merge(adlb, adsl, all = TRUE, by = "SUBJID", suffixes = c("_adlb", "_adsl"))


  missing <- list()
  # Check for subjects who are in ADSL, but not in ABLB
  missing$not_in_adlb <- adsl[FASFL == "Y"][!SUBJID %in% adlb$SUBJID] %>% nrow()

  # Check for subjects who are in ABLB, but not in ADSL
  missing$not_in_adsl <- adlb[!SUBJID %in% adsl$SUBJID] %>% nrow()

  # Check that FASFL values match in the datasets. The only instances of discrepancy
  # are when there are missing values in one of the datasets.
  subject_vec <- dat[FASFL_adlb==FASFL_adsl]$SUBJID
  dat[!SUBJID %in% subject_vec]

  # Those with missing treatment TRT01P variable (the subjectid is not in the
  # baseline), are assigned a treatment value based on the ADLB dataset (i.e
  # TRTP variable)
  dat[is.na(TRT01P), TRT01P:=TRTP]


  list(dat = dat, missing = missing)
}
