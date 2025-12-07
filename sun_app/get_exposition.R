get_exposition <- function(angle_deg) {
  # Normiere Winkel auf 0-360°
  angle_deg <- angle_deg %% 360
  
  # Definiere Grenzen der 8 Richtungen (Mitten bei 0, 45, 90, ..., 315)
  breaks <- seq(-22.5, 360, by = 45)  # -22.5 bis 360 für korrekte Zuordnung
  labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  
  # Winkel in Faktor umwandeln
  factor_labels <- cut(angle_deg, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
  
  # NA vermeiden für 360° -> auf N setzen
  factor_labels[is.na(factor_labels)] <- "N"
  
  return(as.character(factor_labels))
}
