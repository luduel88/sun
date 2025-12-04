horizon <- function (dem, lat, lon, max_distance = 10000, step = 50, azimuths = seq(0, 360, by = 10)) {
  
  # Höhenwert am Beobachtungspunkt
  H0 <- terra::extract(dem, cbind(lon, lat))[,1]
  
  # Distanzen definieren
  distances <- seq(step, max_distance, by = step)
  
  # nur unterschiedliche Azimut
  azimuths <- unique(azimuths)
  
  # Ergebnisvektor
  angles <- numeric(length(azimuths))
  
  # --- Schleife über Azimutrichtungen ---
  for (i in seq_along(azimuths)) {
    az <- azimuths[i]
    
    # Punkte entlang des Profils berechnen
    pts <- geosphere::destPoint(c(lon, lat), az, distances)
    
    # Höhen extrahieren
    ele <- terra::extract(dem, pts)[,1]
    
    # Sichtwinkel berechnen (>= 0)
    ang <- pmax(atan((ele - H0) / distances), 0)
    
    # Maximalen Winkel (Hindernis) speichern
    angles[i] <- max(ang, na.rm = TRUE) * 180 / pi
  }

  res <- data.frame(
    azimuth = azimuths,
    angle = angles
    )
  
  return(res)
  
}
