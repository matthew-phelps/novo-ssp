make_table_1 <- function(data_review) {
  # The variables to be summarized
  adsl <- data_review$adsl
  adlb <- data_review$adlb
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

  names(tmp) <-  c("a_n", "b_n")
  tmp <- cbind(data.table(label = "Number of Subjects"), tmp)
  tmp[, tot_n := dat$dat[, .N]]


  flex <- map(out, rbindlist, fill = T) %>% rbindlist()
  flex <- rbindlist(list(tmp, flex), fill = T)
  flex[,sort(names(flex))]
  setcolorder(flex, sort(names(flex)))
  setcolorder(flex, c("label", "value"))
flex
  table_out <- make_flextable(flex)

  read_docx() %>%
    body_add_flextable(value = table_out) %>%
    print(target = "output/Table 1.docx")
  return(list(table = table_out, missing = dat$missing))
}
