dm_fig1 <- function(adlb) {
  # Filter to selected observations
  x <- adlb[PARAMCD == "C64849B" & FASFL == "Y"]
  x <- x[ANL01FL == "Y"]

  # Check that all units are the same and there are no weird values
  x[, unique(AVALU)]
  x[, unique(PARAM)]
  x$VISITNUM  %>% unique()

  x[is.na(TRTP)]
  x[is.na(AVAL)]

  # Extract week number
  x[, week_numb := strsplit(x$AVISIT, "\\(Week |\\)") %>% sapply("[", 2)]
  x[, week_numb := as.integer(week_numb)]

  # Mean by treatment group
  x[, mean := mean(AVAL), by = .(AVISIT, TRTP)]
  dat_plot <- x[, .SD[1], by = .(TRTP, AVISIT)]

  dat_plot
}
