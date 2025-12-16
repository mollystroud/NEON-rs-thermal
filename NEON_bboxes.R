# Set up all NEON lake bboxes
# get bboxes
PRLA_bbox <- c(xmin = -99.1285, 
                      ymin = 47.1564, 
                      xmax = -99.1116, 
                      ymax = 47.1634)

PRPO_bbox <- c(xmin = -99.2559, 
                         ymin = 47.1274, 
                         xmax = -99.2501, 
                         ymax = 47.1317)

CRAM_bbox <- c(xmin = -89.4791, 
                       ymin = 46.2055, 
                       xmax = -89.4685, 
                       ymax = 46.2121)

LIRO_bbox <- c(xmin = -89.7079, 
                         ymin = 45.9941, 
                         xmax = -89.6975, 
                         ymax = 46.0011)

SUGG_bbox <- c(xmin = -82.0214, 
                    ymin = 29.6843, 
                    xmax = -82.0142, 
                    ymax = 29.6912)

BARC_bbox <- c(xmin = -82.0113, 
                    ymin = 29.6742, 
                    xmax = -82.0063, 
                    ymax = 29.6782)

sunp_bbox <- c(xmin = -72.0714, 
               ymin = 43.3212, 
               xmax = -72.0300, 
               ymax = 43.43008)

bvre_bbox <- c(xmin = -79.827088, 
             ymin = 37.311798, 
             xmax = -79.811865, 
             ymax = 37.321694)

fcre_bbox <- c(xmin = -79.840037, 
             ymin = 37.301435, 
             xmax = -79.833651, 
             ymax = 37.311487)

ccre_bbox <- c(xmin = -79.981728, 
             ymin = 37.367522, 
             xmax = -79.942552, 
             ymax = 37.407255)

# convert the bounding boxes to the correct UTM projection
# check your UTM zone: https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#
# get your EPSG: https://epsg.io/
PRLA_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(PRLA_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32614"))
PRPO_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(PRPO_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32614"))
CRAM_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(CRAM_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32616"))
LIRO_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(LIRO_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32616"))
SUGG_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(SUGG_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32617"))
BARC_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(BARC_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32617"))
sunp_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(sunp_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32618"))

bvre_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(bvre_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32617"))
fcre_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(fcre_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32617"))
ccre_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(ccre_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32617"))


# center most point(s) of water bodies, to avoid mixed pixel issues with thermal
PRLA_points <- data.frame(x = c(491100, 490620), y = c(5222930, 5222770))
PRLA_points <- st_as_sf(
  PRLA_points,
  coords = c("x", "y"),   # change to your column names
  crs = st_crs(PRLA_box_utm)    # match raster CRS!
)

PRPO_points <- data.frame(x = c(480800), y = c(5219650))
PRPO_points <- st_as_sf(
  PRPO_points,
  coords = c("x", "y"),   # change to your column names
  crs = st_crs(PRPO_box_utm)    # match raster CRS!
)

CRAM_points <- data.frame(x = c(309080, 309380), y = c(5120340, 5120340))
CRAM_points <- st_as_sf(
  CRAM_points,
  coords = c("x", "y"),   # change to your column names
  crs = st_crs(CRAM_box_utm)    # match raster CRS!
)

LIRO_points <- data.frame(x = c(290870, 290570), y = c(5097100, 5097450))
LIRO_points <- st_as_sf(
  LIRO_points,
  coords = c("x", "y"),   # change to your column names
  crs = st_crs(LIRO_box_utm)    # match raster CRS!
)

SUGG_points <- data.frame(x = c(401500), y = c(3284600))
SUGG_points <- st_as_sf(
  SUGG_points,
  coords = c("x", "y"),   # change to your column names
  crs = st_crs(SUGG_box_utm)    # match raster CRS!
)

BARC_points <- data.frame(x = c(402440), y = c(3283310))
BARC_points <- st_as_sf(
  BARC_points,
  coords = c("x", "y"),   # change to your column names
  crs = st_crs(BARC_box_utm)    # match raster CRS!
)

sunp_points <- data.frame(x = c(739000, 738500, 739000, 738900), 
                             y = c(4810000, 4808500, 4807000, 4803500))
sunp_points <- st_as_sf(
  sunp_points,
  coords = c("x", "y"),   # change to your column names
  crs = st_crs(sunp_box_utm)    # match raster CRS!
)

fcre_points <- data.frame(x = c(603010, 603050), y = c(4129520, 4129150))
fcre_points <- st_as_sf(
  fcre_points,
  coords = c("x", "y"),   # change to your column names
  crs = st_crs(fcre_box_utm)    # match raster CRS!
)
# bvr
bvre_points <- data.frame(x = c(604600), y = c(4130460))
bvre_points <- st_as_sf(
  bvre_points,
  coords = c("x", "y"),   # change to your column names
  crs = st_crs(bvre_box_utm)    # match raster CRS!
)
# ccr
ccre_points <- data.frame(x = c(592750, 593050), y = c(4137200, 4139000))
ccre_points <- st_as_sf(
  ccre_points,
  coords = c("x", "y"),   # change to your column names
  crs = st_crs(ccre_box_utm)    # match raster CRS!
)

