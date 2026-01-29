#################################################################################
# Author: Molly Stroud
# Started 1/29/26
################################################################################

# Input below:
# 1. Your desired bounding box coordinates (and UTM zone)
# 2. The coordinates of a representative point(s) over your lake or reservoir of interest
# 3. Your start and end dates in the following format: YYYY-DD-MM

################################################################################
# INPUTS
################################################################################

# a four letter site name
# EXAMPLE: SUGG for Lake Suggs
site <- "SUGG"
# specify bounding box
bbox <- c(xmin = -82.0214,
          ymin = 29.6843,
          xmax = -82.0142,
          ymax = 29.6912)

# input UTM zone (necessary for accessing remote sensing data)
# check your UTM zone: https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#
# get your EPSG: https://epsg.io/
EPSG <- 32617
box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(bbox,crs = "EPSG:4326")), paste0("EPSG:", EPSG)))

# representative point(s) of water bodies
points_df <- data.frame(lon = c(-82.018), lat = c(29.688))
points <- st_as_sf(x = points_df,
                   coords = c("lon", "lat"),
                   crs = 4326)
points <- sf::st_transform(points, crs = EPSG)


# dates of interest 
# **START DATE MUST BE AFTER 2020-10-01**
start_date <- "2025-07-01"
end_date <- "2025-07-05"


