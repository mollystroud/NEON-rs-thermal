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
site <- "PRPO"
# specify bounding box
bbox <- c(xmin = -99.2559, 
               ymin = 47.1274, 
               xmax = -99.2501, 
               ymax = 47.1317)

# check your UTM zone: https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#
# get your EPSG: https://epsg.io/
EPSG <- 32614
box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(bbox,crs = "EPSG:4326")), paste0("EPSG:", EPSG)))

# representative point(s) of water bodies
points <- data.frame(x = c(480800), y = c(5219650)) # CHANGE THIS
points <- sf::st_as_sf(
  points,
  coords = c("x", "y"),
  crs = sf::st_crs(box_utm)
)

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
get_stage_2(as.Date(start_date), as.Date(end_date), site)
data <- arrow::read_parquet('/Users/mollystroud/Desktop/postdoc/flare-rs-thermal/drivers/met/test/stage2/reference_datetime=2025-07-04/site_id=PRPO/part-0.parquet')
ggplot(data[data$variable == "air_temperature",], aes(x = datetime, 
                                                      y = prediction, 
                                                      group = parameter,
                                                      color = parameter)) +
  geom_line()

# download stage 3

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


