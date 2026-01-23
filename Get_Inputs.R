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

# a four letter site name
# EXAMPLE: PRPO
site <- "SUGG"
# specify bounding box
bbox <- c(xmin = -82.0214,
          ymin = 29.6843,
          xmax = -82.0142,
          ymax = 29.6912)

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
# START DATE MUST BE AFTER 2020-10-01
start_date <- "2025-07-01"
#end_date <- "2025-08-01"
end_date <- "2025-07-05"
################################################################################
# 1. Download remote sensing data
# Warning: if you are trying to download data over a long period of time (>>1yr) 
# or over a large lake, this will take a long time. Consider looping through
# chunks of time in this section and merging the dfs together at the end
################################################################################
source("get_LST.R")
data <- get_lst(bbox, 
        box_utm, 
        paste0(start_date, "T00:00:00Z"), 
        paste0(end_date, "T00:00:00Z"))
masked_data <- water_mask(data)
# see what it looks like!
ggplot() +
  geom_stars(data = masked_data["thermal_C"]) +
  facet_wrap(~time) +
  theme_classic() +
  scale_fill_viridis(na.value = 'transparent') +
  labs(fill = "Temperature (C)")

thermal_vals <- get_vals(points, masked_data)
output <- clean_data(thermal_vals)
# save out targets
write_csv(output, paste0('targets/', site, '/', site, '-targets-rs-test.csv'))


################################################################################
# 2. Download meteorological data
# Warning: this will take a long time
# Warning: python must be installed to run this 
################################################################################
source("get_met.R")
# download stage 2 
get_stage_2(start_date, end_date, site, bbox)
# download stage 3
get_stage_3(start_date, site, bbox)

################################################################################
# 3. Get bathymetric data (OPTIONAL!)
# If you already have existing bathymetry, skip to line 101
################################################################################
# get bathymetry from GLOBathy
source("get_bathy.R")
bathy <- find_matches(bbox)
plot(bathy)
get_ha(bathy, points)

################################################################################
# 4. Get Kw factor
# If your lake of interest is in the US, use the function get_kw_US
# If your lake of interest is outside the US, use the function get_kw_global
################################################################################


################################################################################
# 5. Create GLM file
################################################################################



################################################################################
# 6. FLARE! (Open up combined_run.R)
################################################################################


