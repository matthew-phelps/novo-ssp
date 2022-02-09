plot_fig1 <- function(dat_plot,y_min,y_max){
  plot <- ggplot(dat_plot) +
    geom_line(aes(
      x = week_numb,
      y = mean,
      group = TRTP,
      color = TRTP
    ), size = 1) +
    geom_point(aes(
      x = week_numb,
      y = mean,
      group = TRTP,
      color = TRTP
    ), size = 3) +
    geom_hline(yintercept = 7, color = "black") +
    geom_hline(yintercept = 6.5, color = "black") +
    geom_vline(xintercept = 0, color = "black")


  plot + theme_bw() +
    labs(caption = "Figure 1 - Mean plot of HbA1c by time - full analysis set") +

    theme(panel.grid = element_blank(),
          plot.caption = element_text(hjust = 0,
                                      size = 14)) +
    scale_color_manual(name = "Treatment group", values = c("#E69F00", "#56B4E9")) +
    scale_y_continuous(
      name = "HbA1c (%)",
      limits = c(y_min, y_max),
      breaks = seq(y_min, y_max, by = 0.2)
    ) +
    scale_x_continuous(name = "Week", breaks = seq(0, max(dat_plot$week_numb), by =
                                                     5))
}
