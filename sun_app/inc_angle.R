# Function angle on hillside
inc_angle <- function(altitude, azimuth, slope, aspect) {
  angle <- sin(altitude) * cos(slope) + cos(altitude) * sin(slope) * cos(azimuth - aspect)
  return(angle)
}
