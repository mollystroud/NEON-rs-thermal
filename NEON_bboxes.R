# Set up all NEON lake bboxes
# get bboxes
prairielake_bbox <- c(xmin = -99.1285, 
                      ymin = 47.1564, 
                      xmax = -99.1116, 
                      ymax = 47.1634)

prairiepothole_bbox <- c(xmin = -99.2559, 
                         ymin = 47.1274, 
                         xmax = -99.2501, 
                         ymax = 47.1317)

cramptonlake_bbox <- c(xmin = -89.4791, 
                       ymin = 46.2055, 
                       xmax = -89.4685, 
                       ymax = 46.2121)

sparklinglake_bbox <- c(xmin = -89.7055, 
                       ymin = 46.0016, 
                       xmax = -89.6954, 
                       ymax = 46.0156)

littlerocklake_bbox <- c(xmin = -89.7079, 
                         ymin = 45.9941, 
                         xmax = -89.6975, 
                         ymax = 46.0011)

lakesuggs_bbox <- c(xmin = -82.0214, 
                    ymin = 29.6843, 
                    xmax = -82.0142, 
                    ymax = 29.6912)

lakebarco_bbox <- c(xmin = -82.0113, 
                    ymin = 29.6742, 
                    xmax = -82.0063, 
                    ymax = 29.6782)

# convert the bounding boxes to the correct UTM projection
# check your UTM zone: https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#
# get your EPSG: https://epsg.io/
prairielake_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(prairielake_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32614"))
prairiepothole_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(prairiepothole_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32614"))
cramptonlake_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(cramptonlake_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32616"))
sparklinglake_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(sparklinglake_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32616"))
littlerocklake_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(littlerocklake_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32616"))
lakesuggs_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(lakesuggs_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32617"))
lakebarco_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(lakebarco_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32617"))


# center most point(s) of water bodies, to avoid mixed pixel issues with thermal
lakebarco_points <- data.frame(x = c(402440), y = c(3283310))
lakebarco_points <- st_as_sf(
  lakebarco_points,
  coords = c("x", "y"),   # change to your column names
  crs = st_crs(lakebarco_box_utm)    # match raster CRS!
)

