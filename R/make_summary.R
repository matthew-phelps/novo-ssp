make_summary  <- function(dat, var, dig, dig2, label) {
  # Stats by grouping variable
  tab_summary <- dat[!is.na(get(var)), .(
    n = .N,
    med = formatC(
      median(get(var), na.rm = T),
      digits = dig,
      format = "f"
    ),
    mean = round(mean(get(var), na.rm = T), digits = dig),
    sd = round(sd(get(var), na.rm = T), digits = dig),
    min = formatC(
      round(min(get(var), na.rm = T), digits = dig2),
      digits = dig2,
      format = "f"
    ),
    max = formatC(
      round(max(get(var), na.rm = T), digits = dig2),
      digits = dig2,
      format = "f"
    )
  ), by = TRT01P] %>%
    .[order(TRT01P )] %>%
    t()

  # Stats by total (i.e. no groping variable)
  tot <- dat[!is.na(get(var)), .(
    n = .N,
    med = formatC(
      median(get(var), na.rm = T),
      digits = dig,
      format = "f"
    ),
    mean = round(mean(get(var), na.rm = T), digits = dig),
    sd = round(sd(get(var), na.rm = T), digits = dig),
    min = formatC(
      round(min(get(var), na.rm = T), digits = dig2),
      digits = dig2,
      format = "f"
    ),
    max = formatC(
      round(max(get(var), na.rm = T), digits = dig2),
      digits = dig2,
      format = "f"
    )
  )]  %>%t()

  # Reshape and format results so it can latter be fed into flextable
  out <- data.table(
    # N = c(tab_summary["n", ], tot["n", ]),
    "Mean (SD)" = c(paste0(tab_summary['mean', ], " (", tab_summary["sd", ], ")"), paste0(tot['mean', ], " (", tot["sd", ], ")")),
    Median = c(tab_summary["med", ], tot["med", ]),
    "Min ; Max" = c(paste0(tab_summary['min', ], " ; ", tab_summary["max", ]), paste0(tot['min', ], " ; ", tot["max", ]))
  )
  m <- out %>% t() %>% as.data.frame() %>% setDT()
  m[, value := names(out)]
  setcolorder(m, "value")
  names(m) <- c("value", "a_z", "b_z", "tot_z")
  n_out <- data.table(
    N = c(tab_summary["n", ], tot["n", ]))

  tmp_N <- n_out %>% t() %>% as.data.frame() %>% setDT()
  tmp_N[, value := names(n_out)]
  setcolorder(tmp_N, "value")
  names(tmp_N) <- c("value", "a_n", "b_n", "tot_n")
  # Add a row containing only the variable. This row will be horizontally
  # merged during the flextable step.
  list(data.table(label = label), tmp_N, m)
}
