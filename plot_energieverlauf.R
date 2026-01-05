plot_energieverlauf <- function(data, tag, inc) {

  output <- table[format(date, "%Y-%m-%d") == tag & aspect == -90 & slope == inc]
  output2 <- table[format(date, "%Y-%m-%d") == tag & aspect == 0 & slope == inc]
  output3 <- table[format(date, "%Y-%m-%d") == tag & aspect == 90 & slope == inc]
  output4 <- table[format(date, "%Y-%m-%d") == tag & aspect == 180 & slope == inc]
  output5 <- table[format(date, "%Y-%m-%d") == tag & aspect == 0 & slope == 0]
  
  p <- ggplot() +
    geom_line(
      data = output,
      aes(x = date, y = cum_radiation_day_MJ, color = paste0("Ost (", inc, " deg)")),
      linewidth = 1.2
    ) +
    geom_line(
      data = output2,
      aes(x = date, y = cum_radiation_day_MJ, color = paste0("Süd (", inc, " deg)")),
      linewidth = 1.2
    ) +
    geom_line(
      data = output3,
      aes(x = date, y = cum_radiation_day_MJ, color = paste0("West (", inc, " deg)")),
      linewidth = 1.2
    ) +
    geom_line(
      data = output4,
      aes(x = date, y = cum_radiation_day_MJ, color = paste0("Nord (", inc, " deg)")),
      linewidth = 1.2
    ) +
    geom_line(
      data = output5,
      aes(x = date, y = cum_radiation_day_MJ, color = paste0("Ebene")),
      color = "black",
      linewidth = 1.2,
      linetype = "dashed"
    ) +
    geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
    geom_hline(yintercept = energy_needed_MJ(), color = "red", linetype = "dotted", linewidth = 1) +
    labs(
      title = paste0("Strahlung in den Alpen - Expositionen am ", tag),
      x = "Tageszeit",
      y = "Energie (MJ/m²)",
      color = "Legende:"
    ) +
    scale_x_datetime(
      date_breaks = "1 hour",
      date_labels = "%H:%M",
      limits = c(
        as.POSIXct(paste(tag, "05:00"), tz = "Europe/Berlin"),
        as.POSIXct(paste(tag, "22:00"), tz = "Europe/Berlin")
      ),
      expand = c(0, 0)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    ) +
    scale_y_continuous(limits = c(0, 30))
  
  return(p)
}