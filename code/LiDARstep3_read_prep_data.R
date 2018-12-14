


library(lidR)
#library(plyr)
library(tidyverse)
library(sp)
library(rgdal)
library(nabor)
library(scatterplot3d)

# this is the first step for calculating derived variables from the LiDAR data
# here we:
# define the upper and lower bounds for what's considered "vegetation", 
# set the radius for the hexagons which are used in calculating horizontal diversity, 
# set the radii that we want to calculate derived metrics over

# want to have these things set in only one location, rather than in each step3x..., to avoid possibly calculating metrics with different settings

# I'm still not entirely sure what to do about the extreme values
# assigning this since it will need to be used multiple times, and might change
# if you change either of these, you need to also go into point.deriver() and change the code where the 3 lists in zsplit are being named
LowerBound = 0.5 # Lasek et al used 0.9m as the lower bound
UpperBound = 75 # and for now just removing anything taller than 75m
#################################################################################################
hex.rad = 4 # radius of hexagon for calculating horizontal diversity index
#################################################################################################


# if additional radii are needed, can redefine zrads and rerun this next ~200 lines
zrads <- c(500, 400, 300, 200, 100, 50)


#zpoint <- "6L"
#zrad = 500

#####################################################################



birds_points_coords_maker <- function(){
  # naming, etc is a little sloppy here, but I'm wrapping this in a function to reduce the number of objects in the environment
  
  # read file that has the UTMs for each point
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
birds.points.coords.sp.lcc.df <- birds_points_coords_maker() 

# double check
#dplyr::filter(birds.points.coords.sp.lcc.df, point == "10R")

###################################
# now some funtions to help get coords for a particular point
# get the coordinates for a particular point
get_point_coords <- function(zpoint, suff) {
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

############################################

norm.las <- readLAS("generated_data/norm_laz/norm_7L.laz")

las_reader <- function(zpoint){

norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
#norm.las@data$point <- zpoint
return(norm.las)
}



las_bounder_classifier <- function(norm.las){ 
  # spot-checking lidr::plot() for several points revealed that important information on low vegetation is contained in returns classified as 1. But class 1 points also have the powerlines, so only including class 1 returns below class 2 (2.13m) and above 0.5m.
  #this is consistent with the metadata for SonVegMap https://sonomaopenspace.egnyte.com/h-s/20140509/8DukhuIXgT
  norm.las.data.with.ground <- norm.las@data %>% 
    filter(Classification == 2 | Classification == 5 |(Classification ==1 & Z < 2.13 & Z > 0.5)) %>%  
    select(X, Y, Z) %>% 
    mutate(Z = Z * 0.3048006096012192,  # convert feet to meters
           Z = ifelse(Z < 0, 0, Z), # change negatives to 0 - this may end up being the wrong thing to do, but most of these are just barely negative
           VegGround = ifelse(Z < 0.5, "ground", "veg"),
           layer = ifelse(VegGround == "ground", "ground",
                          ifelse(VegGround == "veg" & Z <= 5, "shrub", "tree")))
  return(norm.las.data.with.ground)
}



derived_checker_writter <- function(derived.type, znew){
  # now check if the derived_variables file already exists, 
  outpath <- paste("generated_data/derived_variables/derived_variables_", derived.type, ".csv", sep = "")
  if(!file.exists(outpath))
    write.csv(znew, outpath, row.names = F) # if not, create it
  
  zexisting <- read.csv(outpath) # if so, read it then add the next line of data
  zexisting.new <- rbind(zexisting, znew) %>% 
    unique() 
  write.csv(zexisting.new, outpath, row.names = F)
  
}

#example of how to call funtions
#foo <- map_dfr(zrads, basic.summer2)
#derived_checker_writter("basic_summs", foo)

