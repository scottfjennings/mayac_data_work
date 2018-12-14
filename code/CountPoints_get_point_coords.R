

library(lidR)
#library(plyr)
library(tidyverse)
library(sp)
library(rgdal)

birds.points.coords_maker <- function(){
  # naming, etc is a little sloppy here, but I'm wrapping this in a function to reduce the number of objects in the environment
  
# read file that has the Son Veg Map tiles for each point
birds.points.tiles <- read.csv("Sonoma_Veg_Map/bird_points_tiles.csv")


# a few steps to change the projection of the bird points to that of the laz files
# need to do this to get the correct coordinates to do the clipping
birds.points.coords <- birds.points.tiles %>% 
  dplyr::select(point, easting, northing) %>% 
  distinct()
birds.points.coords.sp <- birds.points.coords %>% 
  dplyr::select(easting, northing) %>% 
  SpatialPoints(proj4string = CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")) %>% 
  SpatialPointsDataFrame(., birds.points.coords)

# the to_meter tag appears to be a really important part here
birds.points.coords.sp.lcc <- spTransform(birds.points.coords.sp, CRS("+proj=lcc +lat_1=38.33333333333334 +lat_2=39.83333333333334 +lat_0=37.66666666666666 +lon_0=-122 +x_0=2000000 +y_0=500000.0000000002 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))

# doing data.frame() on the whole object, rather than just extracting the data slot, leads to a df that has coordinates in both utm (@data$easting and @data$northing) and in the CA state plane (@coords)
birds.points.coords.sp.lcc.df <- data.frame(birds.points.coords.sp.lcc) %>% 
  dplyr::select(point, easting.utm = easting, northing.utm = northing, easting.lcc = easting.1, northing.lcc = northing.1)
return(birds.points.coords.sp.lcc.df)
}
birds.points.coords.sp.lcc.df <- birds.points.coords_maker() 

# double check
#dplyr::filter(birds.points.coords.sp.lcc.df, point == "10R")
###################################
# now some funtions to help get coord and SVM tile info for a particular point
# get the coordinates for a particular point
get.point.coords <- function(zpoint, suff) {
  # get coordinates for a bird count point
  # in either utm (suff = ".utm")
  # or CA state plane (suff = ".lcc)
  suff <- ".lcc"
  e <- paste("easting", suff, sep = "")
  n <- paste("northing", suff, sep = "")
  coords <- filter(birds.points.coords.sp.lcc.df, point == zpoint)  %>% 
    select(point, e, n) %>% 
    distinct()
  return(coords)  
}


# run all to here in prep for using read_clipped_laz.R or read_clip_write_laz_files.R


# point.coords <- get.point.coords("10R", ".lcc")

# str(get.point.coords("10R", ".lcc")$easting.lcc)

# get the SVM tiles for the point
# get.tile.list <- function(zpoint) {
#   tiles <- filter(birds.points.tiles, point == zpoint)  
  #tiles2 <- list(paste("Sonoma_Veg_Map/LAZ_files/", tiles$Tile, ".laz", sep = ""))
#   return(tiles)
# }
# point.tiles <- get.tile.list("10R")

###
