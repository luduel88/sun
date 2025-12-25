# Load Packages
library(suncalc)
library(ggplot2)
library(gganimate)
library(gifski)
library(data.table)
library(patchwork)

setwd("~/sun/sun_app")
source("inc_angle.R")
source("am_factor.R")
source("energy_needed.R")
source("deg_to_rad.R")
source("horizon.R")

# Location Alpen Schweiz (Andermatt)
latitude <- 46.6
longitude <- 8.6

# Parameters
solarconstant <- 1000 # W/m2
emission <- 80 # W/m2
interval <- 10 # Minuten

# Time
time <- seq(
  from = as.POSIXct(paste("2025-01-01", "00:00:00"), tz = "Europe/Berlin"),
  to   = as.POSIXct(paste("2025-12-31", "23:59:59"), tz = "Europe/Berlin"),
  by   = paste(interval, "min")
)
time <- time[format(time, "%d") == "15"]

# hillside parameters
hillside <- expand.grid(aspect = seq(from = 180, to = -179.9, by = -45),
                        slope = seq(from = 0, to = 40, by = 10))
hillside <- subset(hillside, slope > 0 | aspect == 0)

# sunposition during day
sunposition <- getSunlightPosition(date = time, lat = latitude, lon = longitude)

# merge with hillside info
table <- merge(sunposition, hillside, by = NULL)
setDT(table)

# compute radiation
table[altitude > 0,
              radiation := inc_angle(altitude, azimuth, deg_to_rad(slope), deg_to_rad(aspect))
              * solarconstant
              * am_factor(altitude)]
table[, radiation := radiation - emission]
table[radiation < 0 | is.na(radiation), radiation := 0]
table[, cum_radiation_day_MJ := cumsum(radiation*interval*60/1000000), by = .(format(date, "%Y-%m-%d"), aspect, slope)]

# Analysis

# Tagesverlauf
tag <- "2025-03-15"
inc <- 20
output <- table[format(date, "%Y-%m-%d") == tag & aspect == -90 & slope == inc]
output2 <- table[format(date, "%Y-%m-%d") == tag & aspect == 0 & slope == inc]
output3 <- table[format(date, "%Y-%m-%d") == tag & aspect == 90 & slope == inc]
output4 <- table[format(date, "%Y-%m-%d") == tag & aspect == 180 & slope == inc]

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

# Strahlungskreis im Tagesverlauf
df <- table[date >= "2025-03-15 05:00:00" &
            date <= "2025-03-15 22:00:00"]

p1 <- ggplot(df, aes(
  x = aspect,
  y = slope,
  fill = radiation
)) +
  geom_tile(color = NA) +
  coord_polar(start = pi/8) +
  scale_fill_viridis_c(
    name = "Radiation",
    option = "inferno",
    limits = c(0, 800),
    oob = scales::squish
  ) +
  scale_y_continuous(
    limits = c(0, 45),
    expand = c(0, 0)
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  transition_time(date) +
  labs(
    title = "{format(frame_time, '%Y-%m-%d %H:%M')}"
  )

p2 <- ggplot(df, aes(
  x = aspect,
  y = slope,
  fill = cum_radiation_day_MJ
)) +
  geom_tile(color = NA) +
  coord_polar(start = pi/8) +
  scale_fill_viridis_c(
    name = "Cumulative Radiation",
    option = "inferno",
    limits = c(0, 5),
    oob = scales::squish
  ) +
  scale_y_continuous(
    limits = c(0, 45),
    expand = c(0, 0)
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  transition_time(date) +
  labs(
    title = "{format(frame_time, '%Y-%m-%d %H:%M')}"
  )

animate(
  p1,
  fps = 4,
  width = 600,
  height = 600,
  renderer = gifski_renderer()
)

setwd("~/sun")
anim_save("radiation_day.gif")
