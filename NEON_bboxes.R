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

# convert the bounding boxes to the correct UTM projection
prairielake_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(prairielake_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32617"))
prairiepothole_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(prairiepothole_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32617"))
cramptonlake_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(cramptonlake_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32617"))
sparklinglake_box_utm <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(sf::st_bbox(sparklinglake_bbox, 
                                             crs = "EPSG:4326")), "EPSG:32617"))




