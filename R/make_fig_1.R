make_fig_1 <- function(adlb) {

  dat_plot <- dm_fig1(adlb)
  plot_range <- dat_plot[,range(mean)]

  # Get  even floor (by first decimal place)
  y_min <- floor(plot_range[1]*10)/10
  y_min <- y_min-(y_min)%%.2

  y_max <- ceiling(plot_range[2]*10)/10
  y_max <- y_max+(y_max)%%.2


  # Plot
  plot <- ggplot(dat_plot) +
    geom_line(aes(
      x = week_numb,
      y = mean,
      group = TRTP,
      color = TRTP
    ), size=1) +
    geom_point(aes(
      x = week_numb,
      y = mean,
      group = TRTP,
      color = TRTP
    ), size=3)+
    geom_hline(yintercept = 7, color="black")+
    geom_hline(yintercept = 6.5, color="black")+
    geom_vline(xintercept = 0, color="black")


  plot+theme_bw()+
    labs(caption = "Figure 1 - Mean plot of HbA1c by time - full analysis set")+
    xlab("Week")+
    theme(panel.grid = element_blank(),
          plot.caption = element_text(hjust = 0,
                                      size = 14))+
    scale_color_manual(name="Treatment group", values = c("#E69F00", "#56B4E9"))+
    scale_y_continuous(name="HbA1c (%)",limits = c(y_min, y_max), breaks = seq(y_min, y_max, by=0.2))

}
