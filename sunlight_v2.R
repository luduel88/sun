# Load Packages
library(suncalc)
library(ggplot2)
library(data.table)

setwd("~/R")
source("energy_needed.R")

# Location Alpen Schweiz (Andermatt)
latitude <- 46.6
longitude <- 8.6

# Time
time <- seq(
  from = as.POSIXct(paste("2025-01-01", "00:00:00"), tz = "Europe/Berlin"),
  to   = as.POSIXct(paste("2025-12-31", "23:59:59"), tz = "Europe/Berlin"),
  by   = "15 min"
)
time <- time[format(time, "%d") == "15"]

# Parameters
solarconstant <- 1000 # W/m2
emission <- 80 # W/m2

# hillside parameters
hillside <- expand.grid(aspect = seq(from = 180, to = -179.9, by = -45),
                        slope = seq(from = 0, to = 40, by = 10))
hillside <- subset(hillside, slope > 0 | aspect == 0)

# Function angle on hillside
inc_angle <- function(altitude, azimuth, slope, aspect) {
  angle <- sin(altitude) * cos(slope) + cos(altitude) * sin(slope) * cos(azimuth - aspect)
  return(angle)
}

# Function Air Mass Factor
am_factor <- function(altitude) {
  factor <- exp(- 0.15 * (1 / sin(altitude) - 1))
  return(factor)
}

# Function Aspect to azimuth
dir2azimuth <- function(dir) {
  dir <- toupper(dir)
  mapping <- c(S = 0, SE = -45, E = -90, NE = -135, N = 180, 
               NW = 135, W = 90, SW = 45)
  if(!dir %in% names(mapping)) stop("Unbekannte aspect. Verwende z.B. 'S','SW','W'...")
  return(mapping[[dir]]/180*pi)
}

# Function azimuth to Aspect
azimuth2dir <- function(azimuth) {
  mapping <- c(S = 0, SE = -45, E = -90, NE = -135, N = 180, 
               NW = 135, W = 90, SW = 45)
  dir <- sapply(azimuth, function(x) {
    idx <- which.min(abs(x - mapping))
    names(mapping)[idx]
  })
  return(dir)
}

# Deg to Rad
to_rad <- function(deg) {
  rad <- deg/180*pi
  return(rad)
}

# sunposition during day
sunposition <- getSunlightPosition(date = time, lat = latitude, lon = longitude)

# merge with hillside info
table <- merge(sunposition, hillside, by = NULL)
setDT(table)

# Horizont hinzu
horizont <- horizon(dem, lon, lat, azimuths = table[, azimuth + pi] / pi * 180) / 180 * pi
horizont$azimuth <- round(horizont$azimuth - pi, 6)
table[, azimuth := round(azimuth, 6)]
table <- table[horizont, on = .(azimuth)]

# compute radiation
table[altitude > angle,
              radiation := inc_angle(altitude, azimuth, to_rad(slope), to_rad(aspect))
              * solarconstant
              * am_factor(altitude)]
table[, radiation := radiation - emission]
table[radiation < 0 | is.na(radiation), radiation := 0]
table[, cum_radiation_day_MJ := cumsum(radiation*15*60/1000000), by = .(format(date, "%Y-%m-%d"), aspect, slope)]

# analysis
tag <- "2025-03-15"
inc <- 30
output <- table[format(date, "%Y-%m-%d") == tag & aspect == -90 & slope == inc]
output2 <- table[format(date, "%Y-%m-%d") == tag & aspect == 0 & slope == inc]
output3 <- table[format(date, "%Y-%m-%d") == tag & aspect == 90 & slope == inc]
output4 <- table[format(date, "%Y-%m-%d") == tag & aspect == 180 & slope == inc]

# Tagesverlauf
ggplot() +
  geom_line(
    data = output,
    aes(x = date, y = cum_radiation_day_MJ, color = paste0("E-hill (", inc, " deg)")),
    linewidth = 1.2
  ) +
  geom_line(
    data = output2,
    aes(x = date, y = cum_radiation_day_MJ, color = paste0("S-hill (", inc, " deg)")),
    linewidth = 1.2
  ) +
  geom_line(
    data = output3,
    aes(x = date, y = cum_radiation_day_MJ, color = paste0("W-hill (", inc, " deg)")),
    linewidth = 1.2
  ) +
  geom_line(
    data = output4,
    aes(x = date, y = cum_radiation_day_MJ, color = paste0("N-hill (", inc, " deg)")),
    linewidth = 1.2
  ) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
  geom_hline(yintercept = energy_needed_MJ(), color = "red", linetype = "dotted", linewidth = 1) +
  labs(
    title = paste0("Energy in Alpen - different hillsides on ", tag),
    x = "Time",
    y = "Energy (MJ/m²)",
    color = "Legende:"
  ) +
  scale_x_datetime(
    date_breaks = "1 hour",
    date_labels = "%H:%M",
    expand = c(0, 0)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# Jahresverlauf
zeit <- "11:00"
inc <- 30
output <- table[format(date, "%H:%M") == zeit & aspect == -90 & slope == inc]
output2 <- table[format(date, "%H:%M") == zeit & aspect == 0 & slope == inc]
output3 <- table[format(date, "%H:%M") == zeit & aspect == 90 & slope == inc]
output4 <- table[format(date, "%H:%M") == zeit & aspect == 180 & slope == inc]

ggplot() +
  geom_line(
    data = output,
    aes(x = date, y = cum_radiation_day_MJ, color = paste0("E-hill (", inc, " deg)")),
    linewidth = 1.2
  ) +
  geom_line(
    data = output2,
    aes(x = date, y = cum_radiation_day_MJ, color = paste0("S-hill (", inc, " deg)")),
    linewidth = 1.2
  ) +
  geom_line(
    data = output3,
    aes(x = date, y = cum_radiation_day_MJ, color = paste0("W-hill (", inc, " deg)")),
    linewidth = 1.2
  ) +
  geom_line(
    data = output4,
    aes(x = date, y = cum_radiation_day_MJ, color = paste0("N-hill (", inc, " deg)")),
    linewidth = 1.2
  ) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
  geom_hline(yintercept = energy_needed_MJ(), color = "red", linetype = "dotted", linewidth = 1) +
  labs(
    title = paste0("Energy in Alpen - different hillsides at ", zeit),
    x = "Time",
    y = "Energy (MJ/m²)",
    color = "Legende:"
  ) +
  scale_x_datetime(
    date_breaks = "15 day",
    date_labels = "%d.%m",
    expand = c(0, 0)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
