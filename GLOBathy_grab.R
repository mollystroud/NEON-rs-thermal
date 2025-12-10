# GLOBathy test script

library(tidyverse)
library(sf)
library(raster)
library(terra)
library(viridis)

x_fcr <-  -79.8373 # doesn't exist in GLOBathy
y_fcr <- 37.30325

x_ccr <- -79.94997
y_ccr <- 37.3851
  
x_bvr <- -79.820864
y_bvr <- 37.314783


files <- list.files("Bathymetry_Rasters", 
                      full.names = T, recursive = T, pattern = ".tif")
possible_res <- list()
for(file in files){
    tif <- raster(file)
    is_in_raster <- ((x_fcr >= xmin(tif) && x_fcr <= xmax(tif) &&
                       y_fcr >= ymin(tif) && y_fcr <= ymax(tif)) |
                       (x_ccr >= xmin(tif) && x_ccr <= xmax(tif) &&
                       y_ccr >= ymin(tif) && y_ccr <= ymax(tif)) |
                       (x_bvr >= xmin(tif) && x_bvr <= xmax(tif) &&
                       y_bvr >= ymin(tif) && y_bvr <= ymax(tif)))
    if(is_in_raster == TRUE){
      print(paste0("MATCH!", file))
      possible_res <- append(possible_res, file)
    } else {print(file)}
  }


#####CCR
ccr_globathy <- raster("Bathymetry_Rasters/100K_200K/112001_113000/112670_bathymetry.tif")
plot(ccr_globathy)
ccr_globathy_df <- as.data.frame(ccr_globathy, xy = T)
ccr_globathy_df <- na.omit(ccr_globathy_df)
ggplot(ccr_globathy_df, aes(x = x, y = y, fill = X112670_bathymetry)) +
  geom_raster() +
  scale_fill_viridis(direction = -1) +
  theme_void() +
  labs(fill = "Depth", title = "GLOBathy")




####BVR
bvr_globathy <- raster("Bathymetry_Rasters/1000K_1100K/1059001_1060000/1059085_bathymetry.tif")
plot(bvr_globathy)
bvr_globathy_df <- as.data.frame(bvr_globathy, xy = T)
bvr_globathy_df <- na.omit(bvr_globathy_df)
ggplot(bvr_globathy_df, aes(x = x, y = y, fill = X1059085_bathymetry)) +
  geom_raster() +
  scale_fill_viridis(direction = -1) +
  theme_void() +
  labs(fill = "Depth", title = "GLOBathy")

shapefiles <- list.files("Roanoke_Bathymetry/BVR bathymetry shapefile", 
                         pattern = ".shp", full.names = T)
unwanted <- list.files("Roanoke_Bathymetry/BVR bathymetry shapefile", 
                       pattern = ".xml", full.names = T)
wanted <- base::setdiff(shapefiles, unwanted)
wanted <- wanted[2:10]
shapefile_list <- lapply(wanted, st_read)

shapefile_list <- lapply(wanted, function(x) {
  sf_obj <- st_read(x, quiet = TRUE)
  
  # Extract number at the start of the filename
  file_num <- sub("^(\\d+).*", "\\1", basename(x))
  
  sf_obj$file_num <- as.numeric(file_num)
  sf_obj
})


bvr <- do.call(rbind, shapefile_list)
bvr <- bvr |>
  arrange(file_num)
plot(bvr[4])
plot(bvr[bvr$file_num == 10,])

ggplot(bvr, aes(fill = file_num)) +
  geom_sf() +
  scale_fill_viridis(direction = -1) +
  theme_void() +
  labs(fill = "Depth")
