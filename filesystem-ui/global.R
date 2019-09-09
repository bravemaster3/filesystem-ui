#loading all required libraries
x <- c("shiny","shinyjs","shinyTime","leaflet","dplyr","plyr","tidyr","stringr","sf","markdown","zoo","data.table","DT","leaflet.extras", "rgdal","raster", "sp")

lapply(x, require, character.only = TRUE)

#Here is the root path to location folders. This with the previous metadata are the paths to specify
path_to_space <- "/home/shiny_data/location_complex/geospatial"
path_to_locations <- "/home/shiny_data/location_complex/measurements"

#loading the shapefile of big squares
big_squares <- readOGR(paste(path_to_space,"shapefiles/big_squares/grid_4degrees_wgs84.shp", sep="/"))
big_squares <- spTransform(big_squares, CRS("+init=epsg:4326"))
global_extent <- extent(bbox(big_squares))
map_center_x <- (global_extent@xmin+global_extent@xmax)/2
map_center_y <- (global_extent@ymin+global_extent@ymax)/2

source("module.R") #this makes all functions in "module.R" available for the application
#source("D:/shiny_server/test_apps/leaflet_location_complex_v2/module.R")
#dirs big square
big_sq_dirs <- str_sub(list.dirs.depth.n(path_to_locations,1),-8)
