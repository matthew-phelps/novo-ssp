make_table_1 <- function(adsl, adlb) {
  # The variables to be summarized
  ls <- list(
    var = c("AGE", "HGTBL", "WGTBL", "DIABDUR", "AVAL"),
    label = c(
      "Age (yrs)",
      "Height (m)",
      "Body Weight (kg)",
      "Duration of Diabetes (yrs)",
      "HbA1c (%)"
    ),
    dig = c(1, 2, rep(1, 3)),
    dig2 = c(0, 2, rep(1, 3))
  )

  dat <- dm_table1(adsl, adlb, ls = ls)
  dat$missing

  out <-
    pmap(list(ls$var, ls$dig, ls$dig2, ls$label), function(a, b, c, d) {
      make_summary(
        dat$dat,
        var = a,
        dig = b,
        dig2 = c,
        label = d
      )
    })
  tmp <- dat$dat[, .N, by = TRT01P] %>%
    .[order(TRT01P)] %>%
    .[, .(N)] %>%
    t() %>%
    data.frame() %>%
    setDT()

  names(tmp) <-  c("V1", "V2")
  tmp <- cbind(data.table(label = "Number of Subjects"), tmp)
  tmp[, V3 := dat$dat[, .N]]

  # Construct the flextable object
  flex <- map(out, rbindlist, fill = T) %>% rbindlist()
  flex <- rbindlist(list(tmp, flex), fill = T)
  setcolorder(flex, c("label", "value"))
  names(flex)
  n_col <- ncol(flex)
  table_out <- flextable(flex)  %>%
    merge_h_range(i = 1,
                  j1 = 1,
                  j2 = 2) %>%
    merge_h_range(i = seq(2, 22, by = 5),
                  j1 = 1,
                  j2 = n_col) %>%
    width(j = 1, width = 0.1) %>%
    width(j = 2:2, width = 1.6) %>%
    width(j = 3:n_col, width = 1.4) %>%
    set_header_labels(
      label = "",
      value = "",
      V1 = "Treat A",
      V2 = "Treat B",
      V3 = "Total"
    ) %>%
    add_footer_lines(
      values = c(
        "N: Number of subjects, SD: Standard Deviation, Min: Minimum,
        Max: Maximum, yrs: Years, m: Meter, kg: Kilogram"
      )
    )

  return(list(table = table_out, missing = dat$missing))
}
