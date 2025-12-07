library(terra)
library(geosphere)
library(suncalc)
library(data.table)
library(ggplot2)

setwd("~/sun/sun_app")
source("horizon.R")
source("sun_position.R")

setwd("~/sun/tif")
files <- list.files(pattern = "\\.tif$", full.names = TRUE)
rasters <- lapply(files, rast)
dem <- do.call(mosaic, rasters)
plot(dem, main = "Schweizer Alpen")

# res(dem)
# ext(dem)
# crs(dem)
# minmax(dem)
# extract(dem, cbind(9.25, 46.79))
# dem[100, 150]
# plot(dem)
# plot(dem, col = terrain.colors(100))
# summary(dem)
# hist(dem)
# slope <- terrain(dem, v = "slope", unit = "degrees")
# plot(slope)
# aspect <- terrain(dem, v = "aspect", unit = "degrees")
# plot(aspect)

# Horizontwinkel an bestimmten Orten
lon <- 9.25
lat <- 46.79
sagogn <- horizon(dem, lat, lon)

lon <- 9.282
lat <- 46.787
valendas <- horizon(dem, lat, lon)

# Sonnenposition über den Tag berechnen
datum <- as.Date("2025-06-21")
sonne_jun <- sun_position(lat, lon, datum)
datum <- as.Date("2025-12-21")
sonne_dez <- sun_position(lat, lon, datum)
datum <- as.Date("2025-03-21")
sonne_mar <- sun_position(lat, lon, datum)

ggplot() +
  # Horizontwinkel
  geom_line(data = sagogn, aes(x = azimuth, y = angle), color = "blue", size = 1.2) +
  geom_line(data = valendas, aes(x = azimuth, y = angle), color = "red", size = 1.2) +
  # Sonnenhöhe
  geom_line(data = sonne_dez, aes(x = azimuth/pi*180 + 180, y = altitude/pi*180),
            color = "orange", size = 1.2) +
  geom_line(data = sonne_jun, aes(x = azimuth/pi*180 + 180, y = altitude/pi*180),
            color = "orange", size = 1.2) +
  geom_line(data = sonne_mar, aes(x = azimuth/pi*180 + 180, y = altitude/pi*180),
            color = "orange", size = 1.2) +
  labs(x = "Azimut (°)", y = "Winkel (°)", title = paste("Horizontprofil und Sonnenverlauf")) +
  ylim(0, 90) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
