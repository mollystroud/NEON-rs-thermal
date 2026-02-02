#################################################################################
# Author: Molly Stroud
# Started 1/29/26
################################################################################
pacman::p_load(sf)
# Input below:
# 1. Your desired bounding box coordinates (and UTM zone)
# 2. The coordinates of a representative point(s) over your lake or reservoir of interest
# 3. Your start and end dates in the following format: YYYY-DD-MM

################################################################################
# INPUTS
################################################################################

# a four letter site name
# EXAMPLE: SUGG for Lake Suggs
site <- "mirr"
# specify bounding box
bbox <- c(xmin = -71.6967,
          ymin = 43.9411,
          xmax = -71.6887,
          ymax = 43.9467)

# input UTM zone (necessary for accessing remote sensing data)
# check your UTM zone: https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#
utm <- 19 # for zone 19
EPSG <- 32600 + utm
box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(bbox,crs = "EPSG:4326")), paste0("EPSG:", EPSG)))

# pick representative point(s) of lake
# for example, if your lake is a perfect circle, a good point would be the
# middle of the circle
points_df <- data.frame(lon = c(-71.6927), lat = c(43.9439))
points <- st_as_sf(x = points_df,
                   coords = c("lon", "lat"),
                   crs = 4326)
points <- sf::st_transform(points, crs = EPSG)


# dates over which you want to run forecasts
# **START DATE MUST BE AFTER 2020-10-01**
start_date <- "2025-10-01"
end_date <- "2025-10-10"
#end_date <- "2025-11-01"


