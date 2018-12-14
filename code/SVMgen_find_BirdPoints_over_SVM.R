# find the Son Veg Map tiles that cover the 500 m buffer around all the Mayac bird count points

library(tidyverse)
library(sp)
library(rgdal)
library(sf)
library(plotKML)
library(raster)
library(rgeos)
library(leaflet)

## read in the veg map data
SonVegMaptiles <- readOGR(dsn="Sonoma_Veg_Map/Tile_Grid_for_LAS-Orthos-Contours", layer="TILE_GRID")
## set the coordinate reference system
SonVegMaptiles_m <- spTransform(SonVegMaptiles, CRS("+proj=lcc +lat_1=38.33333333333334 +lat_2=39.83333333333334 +lat_0=37.66666666666666 +lon_0=-122 +x_0=2000000 +y_0=500000.0000000002 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))


SonVegMaptiles_utm <- spTransform(SonVegMaptiles_m , CRS("+proj=utm +zone=10 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"))


##############################################
# read in file with bird point locations
mayac_spdf.sf <- st_read("generated_data/mayac_spdf.shp")
# make into spatial points dataframe
mayac_spdf <- as(mayac_spdf.sf, "Spatial")


## buffer to 500m radius around each point
mayac500utm <- gBuffer(mayac_spdf, width=500, byid=T, quadsegs=20)

## convert back to lon lat
mayac500ll <- spTransform(mayac500utm, CRS("+proj=longlat +ellps=GRS80 +datum=WGS84 +no_defs"))
## then back to simple feature
mayac500sf <- st_as_sf(mayac500ll)
## convert back to lon lat
SonVegMaptiles_ll <- spTransform(SonVegMaptiles_utm, CRS("+proj=longlat +ellps=GRS80 +datum=WGS84 +no_defs"))
## then back to simple feature
SonVegMaptilessf <- st_as_sf(SonVegMaptiles_ll)


## do overlay with sf package functions
mayacOverSVMtiles <- st_intersection(mayac500sf, SonVegMaptilessf)




## plot to double check points are still in the right place
leaf <- leaflet(height=500) %>% setView(lng=-122.8, lat=38.72, zoom=14) %>%
  addProviderTiles("Stamen.Terrain") %>% 
  addPolygons(data=mayacOverSVMtiles, popup=~paste0(point, "/", Tile))

# yup
bird_tiles <- mayacOverSVMtiles %>% 
  distinct(Tile)

write.csv(bird_tiles, "Sonoma_Veg_Map/bird_tiles.csv", row.names = F)

mayacOverSVMtiles_pointsTiles <- mayacOverSVMtiles %>% 
  as(., "Spatial") %>% 
  spTransform(., CRS("+proj=utm +zone=10 +ellps=GRS80 +datum=WGS84 +units=m +no_defs")) %>% 
  data.frame() 

mayac_spdf_coords <- data.frame(mayac_spdf) 


foo <- mayacOverSVMtiles_pointsTiles %>% 
  full_join(., mayac_spdf_coords) %>% 
  dplyr::select(everything(), easting = coords.x1, northing = coords.x2, -optional) %>% 
  write.csv(., "Sonoma_Veg_Map/bird_points_tiles.csv", row.names = F)
