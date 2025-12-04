sun_position <- function (datum, lon, lat) {
  
  zeiten <- seq(
    from = as.POSIXct(paste(datum, "00:00:00"), tz = "Europe/Berlin"),
    to   = as.POSIXct(paste(datum, "23:59:59"), tz = "Europe/Berlin"),
    by   = "15 min"
  )
  
  sun_pos <- getSunlightPosition(date = zeiten, lat = lat, lon = lon)
  
  sun_pos$time <- zeiten
  
  return(sun_pos)
  
}