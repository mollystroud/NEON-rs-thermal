################################################################################
# GLOBathy test script
################################################################################
library(tidyverse)
library(sf)
library(raster)
library(terra)
library(viridis)
################################################################################
# Open up GLOBathy and loop through all files, open each and see if any of our
# reservoirs are in it
################################################################################
pts <- data.frame(
  x = c(-79.8373, -79.94997, -79.820864, -99.115, -99.253, 
        -89.472, -89.70, -82.019, -82.009, -72.055),
  y = c(37.30325, 37.3851, 37.314783, 47.160, 47.129, 
        46.21, 45.999, 29.688, 29.676, 43.375),
  site = c("fcre", "ccre", "bvre", "PRLA", "PRPO", 
           "CRAM", "LIRO", "SUGG", "BARC", "sunp")
)

files <- list.files("Bathymetry_Rasters", 
                      full.names = T, recursive = T, pattern = ".tif")

possible_res <- character()
for(file in files){
    tif <- raster(file)
    is_in_raster <- any(
      pts$x >= xmin(tif) & pts$x <= xmax(tif) &
        pts$y >= ymin(tif) & pts$y <= ymax(tif)
    )
    if(is_in_raster == TRUE){
      print(paste0("MATCH!", file))
      possible_res <- c(possible_res, file)
      #break
    } else {print(file)}
  }


################################################################################
#####available reservoir locations
ccr_globathy <- raster("Bathymetry_Rasters/100K_200K/112001_113000/112670_bathymetry.tif")
plot(ccr_globathy)

CRAM_bathy <- raster("Bathymetry_Rasters/1000K_1100K/1029001_1030000/1029915_bathymetry.tif")
plot(CRAM_bathy)

LIRO_bathy <- raster("Bathymetry_Rasters/1000K_1100K/1032001_1033000/1032416_bathymetry.tif")
plot(LIRO_bathy)

SUGG_bathy <- raster("Bathymetry_Rasters/1000K_1100K/1066001_1067000/1066600_bathymetry.tif")
plot(SUGG_bathy)

sunp_bathy <- raster("Bathymetry_Rasters/1_100K/9001_10000/9068_bathymetry.tif")
plot(sunp_bathy)

bvr_globathy <- raster("Bathymetry_Rasters/1000K_1100K/1059001_1060000/1059085_bathymetry.tif")
plot(bvr_globathy)
bvr_globathy_df <- as.data.frame(bvr_globathy, xy = T)
bvr_globathy_df <- na.omit(bvr_globathy_df)
ggplot(bvr_globathy_df, aes(x = x, y = y, fill = X1059085_bathymetry)) +
  geom_raster() +
  scale_fill_viridis(direction = -1) +
  theme_void() +
  labs(fill = "Depth", title = "GLOBathy")

# get shapefiles of our bathymetry for comparison
shapefiles <- list.files("Roanoke_Bathymetry/BVR bathymetry shapefile", 
                         pattern = ".shp", full.names = T)
unwanted <- list.files("Roanoke_Bathymetry/BVR bathymetry shapefile", 
                       pattern = ".xml", full.names = T)
wanted <- base::setdiff(shapefiles, unwanted)
wanted <- wanted[2:10]
shapefile_list <- lapply(wanted, st_read)

shapefile_list <- lapply(wanted, function(x) {
  sf_obj <- st_read(x, quiet = T)
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


################################################################################
# convert to H/A for glm files
################################################################################
bvr_globathy <- raster("Bathymetry_Rasters/1000K_1100K/1059001_1060000/1059085_bathymetry.tif")
plot(bvr_globathy)
bvr_globathy_df <- as.data.frame(bvr_globathy, xy = T)
bvr_globathy_df <- na.omit(bvr_globathy_df)
colnames(bvr_globathy_df) <- c("Longitude", "Latitude", "Elevation")

CRAM_globathy <- raster("Bathymetry_Rasters/1000K_1100K/1029001_1030000/1029915_bathymetry.tif")
plot(CRAM_globathy)
CRAM_globathy_df <- as.data.frame(CRAM_globathy, xy = T)
CRAM_globathy_df <- na.omit(CRAM_globathy_df)
colnames(CRAM_globathy_df) <- c("Longitude", "Latitude", "Elevation")

LIRO_globathy <- raster("Bathymetry_Rasters/1000K_1100K/1032001_1033000/1032416_bathymetry.tif")
plot(LIRO_globathy)
LIRO_globathy_df <- as.data.frame(LIRO_globathy, xy = T)
LIRO_globathy_df <- na.omit(LIRO_globathy_df)
colnames(LIRO_globathy_df) <- c("Longitude", "Latitude", "Elevation")

ccre_globathy <- raster("Bathymetry_Rasters/100K_200K/112001_113000/112670_bathymetry.tif")
plot(ccre_globathy)
ccre_globathy_df <- as.data.frame(ccre_globathy, xy = T)
ccre_globathy_df <- na.omit(ccre_globathy_df)
colnames(ccre_globathy_df) <- c("Longitude", "Latitude", "Elevation")

SUGG_globathy <- raster("Bathymetry_Rasters/1000K_1100K/1066001_1067000/1066600_bathymetry.tif")
plot(SUGG_globathy)
SUGG_globathy_df <- as.data.frame(SUGG_globathy, xy = T)
SUGG_globathy_df <- na.omit(SUGG_globathy_df)
colnames(SUGG_globathy_df) <- c("Longitude", "Latitude", "Elevation")

sunp_bathy <- raster("Bathymetry_Rasters/1_100K/9001_10000/9068_bathymetry.tif")
plot(sunp_bathy)
sunp_globathy_df <- as.data.frame(sunp_bathy, xy = T)
sunp_globathy_df <- na.omit(sunp_globathy_df)
colnames(sunp_globathy_df) <- c("Longitude", "Latitude", "Elevation")

library(tidyverse)
library(plotly)
library(marmap)
library(leaflet)
library(dplyr)
library(mapview)
library(webshot)
library(sp)
library(rLakeAnalyzer)
library(sf)


#yarr_bathy_df <- readr::read_csv('./bathymetry/YARR_bathy.csv') 
min_elevation <- min(sunp_globathy_df$Elevation)

sunp_globathy_df <- sunp_globathy_df |> 
  dplyr::mutate(height = Elevation - min(Elevation)) |> 
  dplyr::select(Longitude, Latitude, height) 

## USE MIN ELEVATION VALUE PLUS DEPTH (M) TO GET AHD NEEDED FOR HEIGHT / SFC AREA CURVE FOR GLM

#data_grid <- griddify(bvr_globathy_df, nlon = 75, nlat = 80) 
data_grid <- griddify(sunp_globathy_df, nlon = 391, nlat = 188) 
plot(data_grid)
#plot(bvr_globathy)
area_grid <- raster::area(data_grid, na.rm = TRUE, weights = FALSE)

#Filters out cells with no data
area_grid <- area_grid[!is.na(area_grid)] 

surface_area <- length(area_grid)*mean(area_grid) 

area_layers <- approx.bathy(Zmax = abs(max(sunp_globathy_df$height)), 
                            surface_area*1000000,
                            Zmean= mean(sunp_globathy_df$height), 
                            method = "cone",
                            zinterval = 1,
                            depths = seq(0, abs(max(sunp_globathy_df$height)), by = 1))

#convert depth back to elevation
area_layers$depths <- area_layers$depths + min_elevation

## plot the bathymetry profile
area_layers$depths <- area_layers$depths*-1 #make it so that surface (0m) is at top
# add actual elevation
# bvr 
#area_layers$depths <- area_layers$depths + 590.5
#CRAM
#area_layers$depths <- area_layers$depths + 510
#LIRO
#area_layers$depths <- area_layers$depths + 505
#ccre
#area_layers$depths <- area_layers$depths + 356.6
#SUGG
#area_layers$depths <- area_layers$depths + 30
#sunp
area_layers$depths <- area_layers$depths + 335


plot(area_layers$Area.at.z, area_layers$depths, type = 'l', 
     xlab = 'Area at Depth (m2)', ylab = 'Depth (m)', main = 'GLOBathy')

# true bathys
bvr_bathy <- data.frame(H = c(576, 576.3, 576.6, 576.9, 577.2, 577.5, 577.8, 578.1, 578.4, 578.7, 579, 579.3, 579.6, 579.9, 580.2, 580.5, 580.8, 581.1, 581.4, 581.7, 582, 582.3, 582.6, 582.9, 583.2, 583.5, 583.8, 584.1, 584.4, 584.7, 585, 585.3, 585.6, 585.9, 586.2, 586.5, 586.8, 587.1, 587.4, 587.7, 588, 588.3, 588.6, 588.9, 589.2, 589.5),
                          A = c(0, 2966.476185, 3417.098266, 3943.222265, 8201.749898, 9137.0109, 10083.516114, 18908.696911, 20482.728906, 21898.588973, 35930.572517, 38280.796164, 40097.32227, 42104.235133, 51641.882676, 53959.058794, 56286.074771, 69079.415225, 72100.143538, 74871.418261, 83344.954555, 87375.502914, 90634.540069, 94070.371758, 107069.609564, 111098.635433, 115222.000565, 132627.861799, 137640.432065, 142330.117536, 161556.612776, 167950.184421, 172986.777352, 178517.014158, 203272.260947, 210274.399692, 217393.481004, 273886.355781, 278581.881454, 282911.71991, 289953.276304, 293959.489369, 297845.964104, 301807.90306, 318261.911754, 323872.546042))
ccre_bathy <- data.frame(H = c(333.8, 334.0, 334.5, 335.0, 335.5, 336.0, 336.5, 337.0, 337.5, 338.0, 338.5, 339.0, 339.5, 340.0, 340.5, 341.0, 341.5, 342.0, 342.5, 343.0, 343.5, 344.0, 344.5, 345.0, 345.5, 346.0, 346.5, 347.0, 347.5, 348.0, 348.5, 349.0, 349.5, 350.0, 350.5, 351.0, 351.5, 352.0, 352.5, 353.0, 353.5, 354.0, 354.5, 355.0, 355.5, 356.0, 356.6),
                         A = c(832.8753, 1316.5687, 3004.3469, 5375.7605, 36432.3262, 46207.3146, 56751.0589, 150648.5232, 181127.9386, 215326.8664,271102.4653, 295618.9069, 321263.0279, 369003.5078, 394811.7282, 424212.7850, 467897.0035, 499834.3843, 536283.3268, 609473.2497, 656924.3571, 710646.2363, 834604.9706, 882252.2505, 932422.2323, 1037367.7900, 1088260.2300, 1141185.6880, 1196144.1650, 1282035.5320, 1339068.1470, 1397119.0040, 1489489.5020, 1546227.1880, 1605104.8840, 1700678.7660, 1756631.3800, 1813972.3790, 1898335.7180, 1950833.1100, 2005412.7300, 2088696.4410, 2140008.9470, 2193459.4860, 2317486.3260, 2386760.2280, 2475798.9590))
CRAM_bathy <- data.frame(H = c(489, 490.5, 491, 491.5, 492, 492.5, 493, 493.5, 494, 494.5, 495, 495.5, 496, 496.5, 497, 497.5, 498, 498.5, 499, 499.5, 500, 500.5, 501, 501.5, 502, 502.5, 503, 503.5, 504, 504.5, 505, 505.5, 506, 506.5, 507, 507.5, 508, 508.5, 509),
                          A = c(0, 10.4, 149.82, 467.7, 1019.6, 1585.01, 2146.41, 2904.4, 4194.64, 5484.74, 6890.42, 8638.92, 10279.59, 12208.11, 14125.5, 16467.17, 19193.48, 22579.01, 27198.5, 31584.1, 36997.26, 44025.53, 50479.88, 57385.03, 66752.67, 77869.89, 88977.33, 100091.49, 111853.08, 124871.75, 137328.22, 153524.9, 169216.84, 191678.41, 213789.77, 230037.27, 241422.25, 250570.32, 258924.72))
LIRO_bathy <- data.frame(H = c(492, 493, 494, 495, 496, 497, 498, 499, 500, 501, 501.4, 501.9, 504),
                         A = c(1200.896814, 4445.377, 7689.857634, 14786.16, 21882.46835, 37773.74, 53665.01806, 77701.28, 101737.5469, 143813.8, 159640.5, 175467.2354, 185890.0044))
SUGG_bathy <- data.frame(H = c(25.0, 25.9, 26.4, 26.9, 27.4, 27.9, 28.4, 29.0),
                        A = c(0, 125.6347, 53435.4847, 208192.2158, 263941.7309, 288249.2588, 298163.4627, 307362.1855))
sunp_bathy <- data.frame(H = c(299.43, 299.943, 300.443, 300.943, 301.443, 301.943, 302.443, 302.943, 303.443, 303.943, 304.443, 304.943, 305.443, 305.943, 306.443, 306.943, 307.443, 307.943, 308.443, 308.943, 309.443, 309.943, 310.443, 310.943, 311.443, 311.943, 312.443, 312.943, 313.443, 313.943, 314.443, 314.943, 315.443, 315.943, 316.443, 316.943, 317.443, 317.943, 318.443, 318.943, 319.443, 319.943, 320.443, 320.943, 321.443, 321.943, 322.443, 322.943, 323.443, 323.943, 324.443, 324.943, 325.443, 325.943, 326.443, 326.943, 327.443, 327.943, 328.443, 328.943, 329.443, 329.943, 330.443, 330.943, 331.443, 331.943, 332.343, 332.443, 332.543, 332.643, 332.743, 332.843, 332.943, 333.043, 333.143, 333.243, 333.343, 333.443, 333.543, 333.643, 333.743, 333.943),
                         A = c(1, 16.90309, 38.87712, 64.23176, 158.88908, 7254.80801, 25205.89398, 33207.81875, 41551.18602, 51853.6219, 62734.1436, 79030.41666, 105037.51731, 133941.80829, 168576.24818, 214836.63623, 269137.82616, 327112.05845, 401221.9844, 483528.22071, 575947.57823, 676930.04325, 797467.69789, 925704.7119, 1088626.8751, 1283336.99713, 1505854.39934, 1740820.93038, 1988527.32348, 2234911.89462, 2485285.59597, 2723309.89675, 2978076.71227, 3264022.97524, 3653258.97559, 4050485.06854, 4414109.50064, 4796077.1705, 5158332.45198, 5579849.98152, 6016348.72339, 6468883.43068, 6928267.27171, 7476296.00935, 8028820.97004, 8623779.45825, 9274820.72252, 9852960.63206, 10365149.7392, 10843023.87491, 11253594.96068, 11644117.28649, 12013032.38706, 12356310.56475, 12673823.35603, 12988489.66626, 13285122.06535, 13576406.32546, 13863498.61821, 14148319.13512, 14418854.84716, 14691317.51194, 14951571.07182, 15234317.57908, 15510517.51797, 15780918.00526, 16009460, 16071489, 16135107, 16203453, 16273655, 16349525, 16489737, 16603406, 16760708, 16788809, 16804233, 16826128, 16844170, 16863779, 16885473, 16934251.6))

# comp
ggplot() +
  geom_line(data = area_layers, aes(x = Area.at.z, y = depths, color = 'GLOBathy')) +
  geom_line(data = sunp_bathy, aes(x = A, y = H, color = 'Ours')) +
  theme_classic() +
  labs(x = 'Area at Depth (m2)', y = 'Depth (m)', 
       title = 'SUGG Bathymetry H/A', color = element_blank()) +
  scale_color_manual(values = c('GLOBathy' = 'darkblue', 'Ours' = 'red'))

# #Volume Calculation
# volume_m3 <- function(area_layers,h,n){
#   volume <- (1/3)*h*(area_layers[n,"Area.at.z"] + 
#                        area_layers[(n+1), "Area.at.z"] +
#                        sqrt(area_layers[n,"Area.at.z"] *
#                               area_layers[(n+1),"Area.at.z"]))
#   sum(volume)
# }
# 
# paste("Volume (m^3):", volume_m3(area_layers, h = 1, n = seq(0,abs(min(yarr_bathy_df$Elevation)),1)))

################################################################################
# reorganize GLOBathy
################################################################################
files <- list.files("Bathymetry_Rasters", 
                    full.names = T, recursive = T, pattern = ".tif")

file_index <- data.frame()
for(file in files[1274079:length(files)]){
  tif <- raster(file)
  #plot(tif)
  xmin <- round(xmin(tif), digits = 4)
  xmax <- round(xmax(tif), digits = 4)
  ymin <- round(ymin(tif), digits = 4)
  ymax <- round(ymax(tif), digits = 4)
  info <- cbind(xmin, xmax, ymin, ymax, file)
  file_index <- rbind(file_index, info)
  print(file)
}
write_csv(file_index, "Bathymetry_Rasters/file_index_16.csv")

csvs <- list.files("Bathymetry_Rasters", full.names = T, pattern = "csv")

index_file <- data.frame()
for(csv in csvs){
  data <- read_csv(csv)
  index_file <- rbind(index_file, data)
}

index_file <- index_file[!duplicated(index_file), ]
write_csv(index_file, "Bathymetry_Rasters/index_file.csv")












