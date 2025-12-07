library(terra)

setwd("~/sun/tif")
unlink("n46_47_e7_10.tif")

# TIF laden
files <- list.files(pattern = "\\.tif$", full.names = TRUE)

rasters <- lapply(files, rast)
dem <- do.call(mosaic, rasters)
dem_small <- aggregate(dem, fact = 2, fun = mean)

# neues kleines TIF speichern
writeRaster(dem_small, "n46_47_e7_10.tif", overwrite = TRUE)
