# code to query .laz files (raw LiDAR point cloud data) from Sonoma Veg Map for each bird count point
#
# then write a new laz file, for each point, to /generated_data/clipped_laz
#
# I ran all these, clipping to 500 m, on 10 Jan 2018 and it took ~45 min
#
# probably don'y neet to run these again for now
#
# library(plyr)
library(lidR)
library(plyr)
library(tidyverse)
library(sp)
library(rgdal)
library(leaflet)
library(sf)
library(rgeos)
library(stringr)

###
# requires running all of CountPoints_get_point_coords.R

# now working with the laz files

# set the catalog where the laz files live

mayac.catalog <- catalog("Sonoma_Veg_Map/LAZ_files")


point_clipper <- function(zpoint){
#zpoint = "10R"
# using $ here gets just the value, rather than a 1-field df that dplyr::select would create
x <- get.point.coords(zpoint, ".lcc")$easting.lcc
y <- get.point.coords(zpoint, ".lcc")$northing.lcc
# this is the radius that the laz files will be clipped to around the x and y coords
# the CA state plane units are in feet, this converts from m
rad <- 500/0.3048006096012192
# call the query function from lidR
clipped <- catalog_queries(mayac.catalog, x, y, rad)
# catalog_queries() output is a list of LAS objects, one for each Region Of Interest (ROI) identified by x and y in the _query
# since this only clips to a single point, can extract the first element and output a single LAS object instead
output_name <- paste("generated_data/clipped_laz/clipped2_", zpoint, ".laz", sep = "")
# writeLAS() apparently does not take the CRS along, so these written laz files have NA for their CRS
# but, THE COORDINATES ARE STILL IN THE STATE PLANE CRS THAT THE ORIGINAL laz FILES CAME IN
writeLAS(clipped$ROI1, output_name)
}

##------------
# couldn't firugre out how to just loop through a list of all the point names, so just calling the function for each point
# a for loop to make the code for all the function calls is down below all these calls
##------------
clipped2_1R <- point_clipper("1R") 
clipped2_2L <- point_clipper("2L") 
clipped2_3L <- point_clipper("3L") 
clipped2_4L <- point_clipper("4L") 
clipped2_5R <- point_clipper("5R") 
clipped2_6L <- point_clipper("6L")  
clipped2_7L <- point_clipper("7L")  
clipped2_8L <- point_clipper("8L")  
clipped2_9R <- point_clipper("9R") 
clipped2_10R <- point_clipper("10R") 
clipped2_11R <- point_clipper("11R") 
clipped2_12L <- point_clipper("12L") 
clipped2_13L <- point_clipper("13L") 
clipped2_14R <- point_clipper("14R") 
clipped2_15R <- point_clipper("15R") 
clipped2_16R <- point_clipper("16R")
clipped2_HOME2 <- point_clipper("HOME2") 
clipped2_HOME3 <- point_clipper("HOME3")
clipped2_MCDO10 <- point_clipper("MCDO10")
clipped2_MCDO6 <- point_clipper("MCDO6") 
clipped2_WERA1 <- point_clipper("WERA1") 
clipped2_WERA2 <- point_clipper("WERA2")
clipped2_HOME5 <- point_clipper("HOME5") 
clipped2_MCDO2 <- point_clipper("MCDO2") 
clipped2_MCDO11 <- point_clipper("MCDO11") 
clipped2_FORK10 <- point_clipper("FORK10") 
clipped2_MCDO12 <- point_clipper("MCDO12") 
clipped2_LIIN2 <- point_clipper("LIIN2") 
clipped2_LIIN3 <- point_clipper("LIIN3") 
clipped2_MIRA11 <- point_clipper("MIRA11") 
clipped2_MIRA12 <- point_clipper("MIRA12") 
clipped2_MIRA2 <- point_clipper("MIRA2") 
clipped2_BIHI2 <- point_clipper("BIHI2") 
clipped2_LIIN1 <- point_clipper("LIIN1") 
clipped2_LIIN4 <- point_clipper("LIIN4") 
clipped2_WERA3 <- point_clipper("WERA3") 
clipped2_WEBL1 <- point_clipper("WEBL1") 
clipped2_WEBL3 <- point_clipper("WEBL3")
clipped2_BIHI1 <- point_clipper("BIHI1") 
clipped2_BIHI3 <- point_clipper("BIHI3") 
clipped2_BIHI5 <- point_clipper("BIHI5") 
clipped2_BIHI6 <- point_clipper("BIHI6") 
clipped2_MIRA10 <- point_clipper("MIRA10") 
clipped2_BIHI4 <- point_clipper("BIHI4") 
clipped2_EABL2 <- point_clipper("EABL2") 
clipped2_EARA1 <- point_clipper("EARA1") 
clipped2_BEAR1 <- point_clipper("BEAR1") 
clipped2_EARA2 <- point_clipper("EARA2") 
clipped2_EARA3 <- point_clipper("EARA3") 
clipped2_HIVA1 <- point_clipper("HIVA1") 
clipped2_HIVA3 <- point_clipper("HIVA3") 
clipped2_HIVA2 <- point_clipper("HIVA2")




# -----------------------------------------------
# generate all these function calls
for(i in 1:length(birds.points.coords$point)) {
  call_list <- paste("clipped2_", birds.points.coords$point, " <- point_clipper(", "\"", birds.points.coords$point, "\"", ")", sep = "")
}
for(i in 1:length(call_list)){
  cat(call_list)
}
# -----------------------------------------------
