make_flextable <- function(flex){
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
}
