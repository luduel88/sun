sun_position <- function (latitude, longitude, datum = as.Date("2025-06-21"), interval = "15 min") {
  
  zeiten <- seq(
    from = as.POSIXct(paste(datum, "00:00:00"), tz = "Europe/Berlin"),
    to   = as.POSIXct(paste(datum, "23:59:59"), tz = "Europe/Berlin"),
    by   = interval
  )
  
  sun_pos <- getSunlightPosition(date = zeiten, lat = latitude, lon = longitude)
  
  sun_pos$time <- zeiten
  
  return(sun_pos)
  
}