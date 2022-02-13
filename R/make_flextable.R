make_flextable <- function(flex){
  n_col <- ncol(flex)
  n_row <- nrow(flex)

  align_seq <- c(1, seq(3, n_row, by = 5))
  flextable(flex)  %>%
    merge_h_range(i = 1,
                  j1 = 1,
                  j2 = 2) %>%
    merge_h_range(i = seq(2, 22, by = 5),
                  j1 = 1,
                  j2 = n_col) %>%
    merge_h_range(i = align_seq,
                  j1 = 3,
                  j2 = 4) %>%
    merge_h_range(i = align_seq,
                  j1 = 5,
                  j2 = 6) %>%
    merge_h_range(i = align_seq,
                  j1 = 7,
                  j2 = 8) %>%
    width(j = 1, width = 0.2) %>%
    width(j = 2:2, width = 2) %>%
    width(j = 3:n_col, width = 1) %>%
    width(j = seq(3, 7, 2), width = .2) %>%
    set_header_labels(
      label = "",
      value = "",
      a_n="",
      b_n="",
      tot_n="",
      a_z = "Treat A",
      b_z = "Treat B",
      tot_z = "Total"
    ) %>%
    set_caption(
      "Table 1 - Baseline and diabetes characteristics - summary - full analysis set",
      autonum = NULL,
      style = "Table Caption",
      html_escape = TRUE
    ) %>%
    add_footer_lines(
      values = c(
        "N: Number of subjects, SD: Standard Deviation, Min: Minimum,
        Max: Maximum, yrs: Years, m: Meter, kg: Kilogram"
      )
    )
}
