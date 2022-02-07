make_table_1 <- function(adsl, adlb) {
  x <- adlb[VISITNUM == 10 & PARAMCD == "C64849B" & FASFL == "Y"]

  # Those not present in lab dataset for visit 10
  adsl[FASFL == "Y"][!SUBJID %in% x$SUBJID]

  # These are not present in Subject level dataset
  x[!SUBJID %in% adsl$SUBJID]

  # No dups in adsl file
  adsl[, unique(SUBJID)] %>% length()

  # One dub subject in adlb visit 10
  x[, unique(SUBJID)] %>% length()
  dub <- x[duplicated(SUBJID)]$SUBJID
  x[SUBJID == dub]

  dat <- merge(x, adsl, all = TRUE, by = "SUBJID")

  # Make this into a function to apply to all variables
  # Need to figure out how to rotate data
  dat[, .(
    n = .N,
    med = median(AGE,na.rm = T),
    mean = mean(AGE, na.rm = T),
    sd = sd(AGE,na.rm = T),
    min = min(AGE,na.rm = T),
    max = max(AGE,na.rm = T)
  ), by = TRTP]


  Publish::utable(TRTP ~ Q(AGE), data = dat) %>% summary()
  str(dat)
  DataExplorer::create_report(data = dat[, .(AGE, TRTP, HGTBL)], y = "TRTP")
}
