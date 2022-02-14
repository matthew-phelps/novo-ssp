dm_fig1 <- function(adlb) {
  # Filter to selected observations
  x <- adlb[PARAMCD == "C64849B" & FASFL == "Y"]
  x <- x[ANL01FL == "Y"]

  # Extract week number
  x[, week_numb := strsplit(x$AVISIT, "\\(Week |\\)") %>% sapply("[", 2)]
  x[, week_numb := as.integer(week_numb)]

  # Mean by treatment group
  x[, mean := mean(AVAL), by = .(AVISIT, TRTP)]
  dat_plot <- x[, .SD[1], by = .(TRTP, AVISIT)]

  dat_plot
}
