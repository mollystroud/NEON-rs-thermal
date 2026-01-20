################################################################################
# Run file
# Author: Molly Stroud
# Started 1/20/26
################################################################################

# This script will:
# 1. Download remote sensing data
# 2. Download meteorological data
# 3. Grab bathymetry data (OPTIIONAL)
# 4. Grab Kw factor
# 5. Create GLM file
# 6. Run FLARE

# To run this script, input below:
# 1. Your desired bounding box coordinates (and UTM zone)
# 2. The coordinates of a representative point(s) over your lake or reservoir of interest
# 3. Your start and end dates in the following format: YYYY-DD-MM


################################################################################
# INPUTS
################################################################################

# EXAMPLE: PRLA
site <- "PRLA"
# specify bounding box
bbox <- c(xmin = -99.1285, 
          ymin = 47.1564,
          xmax = -99.1116, 
          ymax = 47.1634)

# check your UTM zone: https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#
# get your EPSG: https://epsg.io/
EPSG <- 32614
box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(bbox,crs = "EPSG:4326")), paste0("EPSG:", EPSG)))

# representative point(s) of water bodies
points <- data.frame(x = c(491100, 490620), y = c(5222930, 5222770))
points <- st_as_sf(
  points,
  coords = c("x", "y"),
  crs = st_crs(box_utm)
)

# dates of interest
start_date <- "2025-06-01"
end_date <- "2025-08-01"

################################################################################
# 1. Download remote sensing data
# Warning: if you are trying to download data over a long period of time (>1yr) 
# or over a large lake, this will take a long time
################################################################################
source("get_LST.R")
data <- get_lst(bbox, 
        box_utm, 
        paste0(start_date, "T00:00:00Z"), 
        paste0(end_date, "T00:00:00Z"))
masked_data <- water_mask(data)
plot(masked_data["thermal_C"])
vals <- get_vals(points, masked_data)
## FINISH THIS UP AND WRITE FUNCTION TO PUT IN


################################################################################
# 2. Download meteorological data
# Warning: this will take a long time
################################################################################



################################################################################
# 3. Get bathymetric data (OPTIONAL!)
# If you already have existing bathymetry, skip to line XXX
################################################################################



################################################################################
# 4. Get Kw factor
################################################################################


################################################################################
# 5. Create GLM file
################################################################################



################################################################################
# 6. FLARE! (Open up combined_run.R)
################################################################################


