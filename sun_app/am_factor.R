# Function Air Mass Factor
am_factor <- function(altitude) {
  factor <- exp(- 0.15 * (1 / sin(altitude) - 1))
  return(factor)
}