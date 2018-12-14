

# calculating derived metrics of habitat structure
#

# 
# requires running CountPoints_get_point_coords.R
# which means the packages needed here will already be loaded, but here they are for reference
#
library(plyr)
#
library(tidyverse)
#
library(lidR)
library(purrr)

# first load the 2 functions point.deriver() and derived_appender(),
# then see notes below funtions for the workflow to use here

options(scipen = 999)

zpoint <- "6L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))

  point.coords <- get.point.coords(zpoint, ".lcc")  

    norm.las <- lasclipCircle(norm.las, point.coords$easting.lcc, point.coords$northing.lcc, 50/0.3048006096012192)

  #------- 
    
table(norm.las@data$Classification)  

plot(norm.las)    
    
    
    
    
    
    
    
    
  