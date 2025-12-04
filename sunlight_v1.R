# Pakete laden
library(suncalc)
library(ggplot2)
library(data.table)

# Ort Zürich
latitude <- 46.6
longitude <- 8.6

# Datum
datum <- as.Date("2025-06-21")

# Solarkonstante
solarkonstante <- 1000

# Hangparameter
slope_deg <- 90
aspect_text <- "S"

#Funktion Einfallswinkel
einfallswinkel <- function(altitude, azimuth, slope, aspect) {
  winkel <- sin(altitude) * cos(slope) + cos(altitude) * sin(slope) * cos(azimuth - aspect)
  return(winkel)
}

# Funktion Air Mass Faktor
am_faktor <- function(altitude) {
  faktor <- exp(- 0.15 * (1 / sin(altitude) - 1))
  return(faktor)
}

# Himmelsrichtung in Azimut
dir2azimuth <- function(dir) {
  dir <- toupper(dir)
  mapping <- c(S = 0, SE = -45, E = -90, NE = -135, N = 180, 
               NW = 135, W = 90, SW = 45)
  if(!dir %in% names(mapping)) stop("Unbekannte Richtung. Verwende z.B. 'S','SW','W'...")
  return(mapping[[dir]]/180*pi)
}

# Sonnenzeiten berechnen
sun_data <- getSunlightTimes(date = datum, lat = latitude, lon = longitude, tz = "Europe/Berlin")
print(sun_data)

# Sonnenposition über den Tag berechnen
zeiten <- seq(
  from = as.POSIXct(paste(datum, "00:00:00"), tz = "Europe/Berlin"),
  to   = as.POSIXct(paste(datum, "23:59:59"), tz = "Europe/Berlin"),
  by   = "15 min"
)
sonnenverlauf <- getSunlightPosition(date = zeiten, lat = latitude, lon = longitude)
sonnenverlauf$time <- zeiten
setDT(sonnenverlauf)

# Strahlung auf Ebene
sonnenverlauf[altitude > 0, strahlung := sin(altitude) * solarkonstante * am_faktor(altitude)]

# Strahlung auf Hang
aspect <- dir2azimuth(aspect_text)
slope <- slope_deg/180*pi
sonnenverlauf[altitude > 0,
              strahlung_hang := einfallswinkel(altitude, azimuth, slope, aspect)
                                * solarkonstante
                                * am_faktor(altitude)]
sonnenverlauf[strahlung_hang < 0, strahlung_hang := NA_real_]

# Plot Sonnenhöhe
ggplot(sonnenverlauf, aes(x = time, y = altitude * 180/pi)) +
  geom_line(color = "orange", linewidth = 1.2) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
  labs(
    title = paste("Sonnenverlauf in Zürich am", datum),
    x = "Uhrzeit",
    y = "Sonnenhöhe (°)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# Plot mit Beschriftung Ebene
energie <- sum(sonnenverlauf$strahlung / 6000, na.rm = TRUE)
ggplot(sonnenverlauf, aes(x = time, y = strahlung)) +
  geom_line(color = "orange", linewidth = 1.2) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
  labs(
    title = paste0("Strahlung in Zürich am ", datum, " (Ebene)"),
    x = "Uhrzeit",
    y = "Strahlung (W/m²)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  ) +
  annotate(
    "text",
    x = min(sonnenverlauf$time, na.rm = TRUE) + diff(range(sonnenverlauf$time, na.rm = TRUE)) * 0.7,
    y = max(sonnenverlauf$strahlung, na.rm = TRUE) * 0.9,
    label = paste("Gesamtenergie ≈", round(energie, 1), "kWh/m²"),
    color = "black",
    size = 5,
    hjust = 0
  )

# Plot mit Beschriftung Hang
energie <- sum(sonnenverlauf$strahlung_hang / 6000, na.rm = TRUE)
ggplot(sonnenverlauf, aes(x = time, y = strahlung_hang)) +
  geom_line(color = "orange", linewidth = 1.2) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
  labs(
    title = paste0("Strahlung in Zürich am ", datum, " (", aspect_text, "-Hang, ", slope*180/pi, " Grad)"),
    x = "Uhrzeit",
    y = "Strahlung (W/m²)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  ) +
  annotate(
    "text",
    x = min(sonnenverlauf$time, na.rm = TRUE) + diff(range(sonnenverlauf$time, na.rm = TRUE)) * 0.7,
    y = max(sonnenverlauf$strahlung_hang, na.rm = TRUE) * 0.9,
    label = paste("Gesamtenergie ≈", round(energie, 1), "kWh/m²"),
    color = "black",
    size = 5,
    hjust = 0
  )
