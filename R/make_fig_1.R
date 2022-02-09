make_fig_1 <- function(adlb) {
  dat_plot <- dm_fig1(adlb)
  plot_range <- dat_plot[, range(mean)]

  # Get  even floor (by first decimal place)
  y_min <- floor(plot_range[1] * 10) / 10
  y_min <- y_min - (y_min) %% .2

  y_max <- ceiling(plot_range[2] * 10) / 10
  y_max <- y_max + (y_max) %% .2

  plot_fig1(dat_plot, y_min, y_max)
}
