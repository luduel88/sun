library(terra)

# TIF laden
files <- list.files("data", pattern = "\\.tif$", full.names = TRUE)

rasters <- lapply(files, rast)
dem <- do.call(mosaic, rasters)
dem_small <- aggregate(dem, fact = 2, fun = mean)  # mittlerer Wert

# Optional: neues TIF speichern
writeRaster(dem_small, "data/dem_small.tif", overwrite = TRUE)
