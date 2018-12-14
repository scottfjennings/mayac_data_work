# combine Pine FLat and Modini bird points


library(tidyverse)
library(sp)
library(rgdal)
library(sf)
library(plotKML)
library(maptools)


# projection PFR points come in as
pfr_prj <- CRS("+proj=longlat +ellps=GRS80 +datum=WGS84 +no_defs")
# projection MIEP points come in as
miep_prj <- CRS("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs")
# final projection I want the combined sp object to be
end_prj <-  CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")


## prepare Pine Flat Rd points for binding
## read gpx file with Pine Flat Rd bird survey point locations
pfr.points <- readGPX("PineFlatRd/PineFlatStations.gpx")
## extract point ID, lon and lat
pfr.points2 <- pfr.points$waypoints %>% 
  dplyr::select(lon, lat, point = name)
# make into sp object with 
pineflat <- pfr.points2 %>% 
  dplyr::select(-point) %>% 
  SpatialPoints(., proj4string = pfr_prj)
# make spatial points data frame with point IDs
pineflat_spdf <- SpatialPointsDataFrame(pineflat, pfr.points2)
## change to UTM 
pineflat_spdf_end_prj <- spTransform(pineflat_spdf, end_prj)




## prepare MIEP points for binding
# read in MIEP shapefile as simple feature
miep_points <- st_read("MIEP/bird_point_shp/surveypoints6.shp")
# convert to spatial object
miep_spdf <- as(miep_points, "Spatial")
## change to end projection 
miep_spdf_end_prj <- spTransform(miep_spdf, end_prj)


## now final prep and binding -- need to be sure to keep the objects in the right order here
# extract and bind the coordinates
pineflat_spdf_end_prj_coords <- pineflat_spdf_end_prj@coords %>% 
  data.frame() %>% 
  dplyr::select(eastings = lon, northings = lat)
miep_spdf_end_prj_coords <- miep_spdf_end_prj@coords %>% 
  data.frame() %>% 
  select(eastings = coords.x1, northings = coords.x2)
mayac_coords <- rbind(pineflat_spdf_end_prj_coords, miep_spdf_end_prj_coords)

# extract and bind the point IDs
pineflat_spdf_end_prj_pointIDs <- pineflat_spdf_end_prj@data %>% 
  data.frame() %>% 
  dplyr::select(point)
miep_spdf_end_prj_pointIDs <- miep_spdf_end_prj@data %>% 
  data.frame() %>% 
  dplyr::select(point = stationid) %>% 
  mutate(point = as.character(point))
mayac_pointIDs <- rbind(pineflat_spdf_end_prj_pointIDs, miep_spdf_end_prj_pointIDs)

# make coords back into sp object
mayac_coords_sp <- SpatialPoints(mayac_coords, end_prj)
# then add point IDs to make SPDF
mayac_coords_spdf <- SpatialPointsDataFrame(mayac_coords_sp, mayac_pointIDs)

# double check everything looks right
test <- cbind(mayac_coords_spdf@data, mayac_coords_spdf@coords)
pfr.test <- cbind(dplyr::select(pineflat_spdf_end_prj@data, point), pineflat_spdf_end_prj@coords)

filter(test, point == "3L")
filter(pfr.test, point == "3L")
filter(miep_points, stationid == "BIHI5") %>% 
  dplyr::select(stationid, geometry)


writeOGR(mayac_coords_spdf, dsn = "generated_data", layer = mayac_coords_spdf, driver="ESRI Shapefile")


mayac_spdf.sf <- st_as_sf(mayac_coords_spdf)

st_write(mayac_spdf.sf, "generated_data/mayac_spdf.shp")
