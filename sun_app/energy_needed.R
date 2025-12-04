energy_needed_MJ <- function(snow_layer = 0.1,
                          snow_density = 400,
                          start_temp = -5,
                          albedo = 0.65,
                          wetness = 0.1,
                          capacity_ice = 2.1/1000,
                          melt_ice = 334/1000) {
  mass <- snow_layer * snow_density
  warm <- mass * capacity_ice * -start_temp
  melt <- mass * wetness * melt_ice
  energy <- warm + melt
  return(energy/(1 - albedo))
}

energy_needed_MJ()
