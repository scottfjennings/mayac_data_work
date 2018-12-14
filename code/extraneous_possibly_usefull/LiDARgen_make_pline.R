
# this makes a 15 m buffer around each of 3 separate lines for the powerlines running through the study area
# then merges them into a single polygon
# output is a sf

library(sf)
library(sp)
library(tidyverse)
library(rgeos)

pline_1 <- st_read("generated_data/mayac_powerlines/mayac-powerline-1.kml")
pline_2 <- st_read("generated_data/mayac_powerlines/mayac-powerline-2.kml")
pline_3 <- st_read("generated_data/mayac_powerlines/mayac-powerline-3.kml")
pline_4 <- st_read("generated_data/mayac_powerlines/mayac-powerline-4.kml")

pliner.lat <- function(pline) {
  pline_df <- pline[2:nrow(pline),] %>% 
    as(., "Spatial") %>% 
    data.frame() %>% 
    select(lon = coords.x1, lat = coords.x2) %>% 
    arrange(lat) 
  pline_line <- SpatialLines(list(Lines(list(Line(pline_df)), "id")), proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  return(pline_line)
}
pline1_l <- pliner.lat(pline_1)
pline2_l <- pliner.lat(pline_2)

#---
pliner.lon <- function(pline) {
  pline_df <- pline[2:nrow(pline),] %>% 
    as(., "Spatial") %>% 
    data.frame() %>% 
    select(lon = coords.x1, lat = coords.x2) %>% 
    arrange(lon) 
  pline_line <- SpatialLines(list(Lines(list(Line(pline_df)), "id")), proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  return(pline_line)
}
pline3_l <- pliner.lon(pline_3)
pline4_l <- pliner.lon(pline_4)



trans_trans_buff <- function(pline_l) {
  pline_l_utm <- spTransform(pline_l, CRS = "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
  pline_l_utm_sf <- st_as_sf(pline_l_utm)
  pline_l_utm_sf_buff <- st_buffer(pline_l_utm_sf, 15)
  return(pline_l_utm_sf_buff)
}
pline1_l_utm_sf_buff <- trans_trans_buff(pline1_l)
pline2_l_utm_sf_buff <- trans_trans_buff(pline2_l)
pline3_l_utm_sf_buff <- trans_trans_buff(pline3_l)
pline4_l_utm_sf_buff <- trans_trans_buff(pline4_l)



pline <- st_union(st_union(pline1_l_utm_sf_buff, pline2_l_utm_sf_buff),
                  st_union(pline3_l_utm_sf_buff, pline4_l_utm_sf_buff))
plot(pline)

# pline is now a polygon representing all the area covered by the powerlines
