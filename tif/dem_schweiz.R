library(terra)
library(readxl)
library(parallel)
library(tictoc)

setwd("~/sun/tif")

# Download der TIFs
links <- read_excel("DEM_Schweiz_2x2.xlsx", col_names = FALSE)[[1]]

download_one <- function(url) {
  fname <- file.path("tif_raw", basename(url))
  if (!file.exists(fname)) {
    try(download.file(url, fname, mode = "wb", quiet = TRUE))
  }
  return(fname)
}

cl <- makeCluster(detectCores() - 1)
clusterExport(cl, c("download_one"))
clusterExport(cl, c("links"))
clusterEvalQ(cl, library(terra))

files <- parLapply(cl, links, download_one)
stopCluster(cl)

# TIFs einlesen
tifs <- list.files("tif_raw", pattern = "\\.tif$", full.names = TRUE)

# Batches bilden & verarbeiten
batches <- split(tifs, ceiling(seq_along(tifs) / 1000))

for (i in 1:length(batches)) {
  
  cat("Verarbeite Batch", i, "von", length(batches), "...\n")
  tic()
  vrtfile <- file.path("tif_processed", sprintf("batch_%03d.vrt", i))
  
  # 1. VRT für das aktuelle Paket
  vrt(batches[[i]],
      vrtfile,
      overwrite = TRUE)
  
  # 2. Virtuelles Mosaik laden
  rs <- rast(vrtfile)
  
  # 3. Zielraster (50 m) für dieses Paket definieren
  r50 <- rast(
    extent = ext(rs),
    resolution = 50,
    crs = crs(rs)
  )
  
  # 4. Resampling auf 50 m
  rs50 <- resample(rs, r50, method = "bilinear")
  # rs50 <- aggregate(rs, fact = 25, fun = mean)
  
  # 5. Ganzzahl-Höhen
  rs50_int <- round(rs50)
  
  # 6. Paket-Output speichern
  writeRaster(
    rs50_int,
    file.path("tif_processed", sprintf("dem_switzerland_50m_part_%03d.tif", i)),
    filetype = "GTiff",
    datatype = "INT2S",
    overwrite = TRUE,
    wopt = list(
      gdal = c(
        "COMPRESS=DEFLATE",
        "PREDICTOR=2",
        "TILED=YES"
      ),
      names = "elevation"
    )
  )
  toc()
}

# Batches zusammenfuegen
parts <- list.files("tif_processed", pattern="part_.*\\.tif$", full.names = TRUE)
vrtfile <- file.path("tif_processed", "swiss_50m_final.vrt")
vrt(parts, vrtfile, overwrite = TRUE)

writeRaster(
  rast(vrtfile),
  file.path("tif_processed", "dem_switzerland_50m_lv95.tif"),
  filetype = "GTiff",
  datatype = "INT2S",
  overwrite = TRUE
)

# VRT Files löschen
unlink("tif_processed/*.vrt")

# Projektion auf WGS84 statt LV95
dem_wgs84 <- project(rast(file.path("tif_processed", "dem_switzerland_50m_lv95.tif")), "EPSG:4326")

writeRaster(round(dem_wgs84),
            file.path("tif_processed", "dem_switzerland_50m_wgs84.tif"),
            filetype = "GTiff",
            datatype = "INT2S",
            overwrite = TRUE)
