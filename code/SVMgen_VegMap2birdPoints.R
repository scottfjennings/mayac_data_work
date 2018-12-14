

## code to read in data from the Sonoma Veg Map and the Pine Flat Rd point count points, then extract veg data for the points



library(rgdal)
library(plotKML)
library(tidyverse)
library(sp)
library(leaflet)
library(raster)
library(rgeos)
library(sf)
library(rgbif)


## read in the veg map data
SonVegMap <- readOGR(dsn="Sonoma_Veg_Map", layer="Sonoma_Veg_Map_5_1")
## set the coordinate reference system
SonVegMapddtrans <- spTransform(SonVegMap, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

## convert to simple feature
SonVegMap_sf <- st_as_sf(SonVegMapddtrans)
##############################################
## read gpx file with Pine Flat Rd bird survey point locations
pfr.points <- readGPX("C:/Users/scott.jennings/Documents/FieldSiteInfo/PineFlatRd/PineFlatStations.gpx")
## extract lon and lat
pfr.points2 <- pfr.points$waypoints %>% 
  dplyr::select(lon, lat)


prj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
pineflat <- SpatialPoints(pfr.points2, proj4string = prj)


## change to UTM for easiest buffering in meters
pineflat.utm <- spTransform(pineflat, CRS("+proj=utm +zone=10 +ellps=WGS84"))

## buffer to 50m radius around each point
pineflat50utm <- gBuffer(pineflat.utm, width=50, byid=T, quadsegs=20)

## convert back to lon lat
pineflat50dd <- spTransform(pineflat50utm, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
## and make into a Spatial Polygons Data Frame
pineflat50dddf <- SpatialPolygonsDataFrame(pineflat50dd, data=pfr.points2)

pineflat50_sf <- st_as_sf(pineflat50dddf)

pineflat50_sf <- pineflat50_sf %>% 
  mutate(point = 16:1)

## plot to double check points are still in the right place
leaf <- leaflet(height=500) %>% setView(lng=-122.8, lat=38.72, zoom=14) %>%
  addProviderTiles("Stamen.Terrain") %>% 
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", group = "Satellite image")%>%
  addCircleMarkers(data=pineflat, radius=1) %>% 
  addPolygons(data=pineflat50dddf)



points.veg <- over(pineflat50dd, SonVegMapddtrans, returnList = TRUE)

## do overlay with sf package functions
int <- st_intersection(pineflat50_sf, SonVegMap_sf)

int_geo <- st_geometry(int)

int <- int %>% 
  mutate(area = st_area(.) %>% as.numeric(),
         proportion = area/7853.982)

st_write(int, "PineFlatRd_plus_SonVegMap.shp")

##################################################
## and again for 400m radius

## buffer to 400m radius around each point
pineflat400utm <- gBuffer(pineflat.utm, width=400, byid=T, quadsegs=20)

## convert back to lon lat
pineflat400dd <- spTransform(pineflat400utm, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
## and make into a Spatial Polygons Data Frame
pineflat400dddf <- SpatialPolygonsDataFrame(pineflat400dd, data=pfr.points2)

pineflat400_sf <- st_as_sf(pineflat400dddf)

pineflat400_sf <- pineflat400_sf %>% 
  mutate(point = 16:1)

## plot to double check points are still in the right place
leaf <- leaflet(height=500) %>% setView(lng=-122.8, lat=38.72, zoom=14) %>%
  addProviderTiles("Stamen.Terrain") %>% 
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", group = "Satellite image")%>%
  addCircleMarkers(data=pineflat, radius=1) %>% 
  addPolygons(data=pineflat400dddf)



points.veg400 <- over(pineflat400dd, SonVegMapddtrans, returnList = TRUE)

## do overlay with sf package functions
int400 <- st_intersection(pineflat400_sf, SonVegMap_sf)

int_geo400 <- st_geometry(int400)

int400 <- int400 %>% 
  mutate(area = st_area(.) %>% as.numeric(),
         proportion = area/502654.8)

st_write(int400, "PineFlatRd400_plus_SonVegMap.shp")


pfr_svm <- st_read("generated_data/PineFlatRd_plus_SonVegMap.shp")


## get elevation for each point
## pretty hackish for now: copied elevetions over by hand from the .kmz (in GE, opened the properties for each point, then scrolled down to 'altitude')
pfr_elevs <- data.frame(
point = 1:16,
altitude = c(60, 95, 136, 136, 200, 275, 368, 439, 522, 645, 658, 634, 631, 774, 889, 956)
)





### exploring the SonVegMap data
##plot geometry at a specific point
int_geo <- int %>%
  filter(point == 16) %>% 
  st_geometry() %>% 
  plot()

mapclass_perpoint_all <- PFR_SVM %>% 
     data.frame() %>% 
     dplyr::select(point, MAP_CLASS, proportion) %>% 
     group_by(point, MAP_CLASS) %>% 
     summarize(proportion = sum(proportion)) %>% 
     group_by(point) %>% 
     summarise(n())



# summarizing by MAPP_CLASS
by_point <- PFR_SVM %>% 
  data.frame() %>% 
  dplyr::select(point, MAP_CLASS, proportion) %>% 
  group_by(point, MAP_CLASS) %>% 
  summarize(proportion = sum(proportion)) %>% 
  filter(proportion > 0.1) 

%>% 
  spread(key = MAP_CLASS, value = proportion) %>% 
  mutate_all(funs(coalesce(., 0))) 

%>% 
  group_by(point) %>% 
  summarise(n())



dev.off()

ggplot(data = by_point)+
  geom_col(aes(x = point, y = proportion, fill = MAP_CLASS))

## summarizing by LF_FOREST
LF_FOREST_by_point <- PFR_SVM %>% 
  data.frame() %>% 
  dplyr::select(point, LF_FOREST, proportion) %>% 
  group_by(point, LF_FOREST) %>% 
  summarize(proportion = sum(proportion)) 

ggplot(data = LF_FOREST_by_point)+
  geom_col(aes(x = point, y = proportion, fill = LF_FOREST))

## SonVegMap variables to include

TREE_HT_MN
TREE_HT_SD
ABS_COVER






###################################################################################


#### MIEP POINTS
# read at simple feature
miep_points <- st_read("MIEP/bird_point_shp/surveypoints6.shp")
# convert to sp
miep_points_sp <- as(miep_points, "Spatial")
# set CRS to UTM
miep_points_utm <- spTransform(miep_points_sp, CRS("+proj=utm +zone=10 +ellps=WGS84"))
## buffer to 50m radius around each point
miep_points_50utm <- gBuffer(miep_points_utm, width=50, byid=T, quadsegs=20)
## convert back to lon lat
miep_points_50dd <- spTransform(miep_points_50utm, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
## then back to simple feature
miep_points_50sf <- st_as_sf(miep_points_50dd)


## do overlay with sf package functions
miep_int <- st_intersection(miep_points_50sf, SonVegMap_sf)


miep_int <- miep_int %>% 
  mutate(area = st_area(.) %>% as.numeric(),
         proportion = area/7853.982) %>% 
  dplyr::select(point = stationid, everything(), -dateestab, -status, -notes, -locationno, -newxcoord, -newycoord, -OID_COPY)

# write and read the generated shp
st_write(miep_int, "generated_data/MIEP_plus_SonVegMap.shp")

#################################################################
## and now 400m radius
# read at simple feature
miep_points <- st_read("MIEP/bird_point_shp/surveypoints6.shp")
# convert to sp
miep_points_sp <- as(miep_points, "Spatial")
# set CRS to UTM
miep_points_utm <- spTransform(miep_points_sp, CRS("+proj=utm +zone=10 +ellps=WGS84"))

## buffer to 400m radius around each point
miep_points_400utm <- gBuffer(miep_points_utm, width=400, byid=T, quadsegs=20)
## convert back to lon lat
miep_points_400dd <- spTransform(miep_points_400utm, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
## then back to simple feature
miep_points_400sf <- st_as_sf(miep_points_400dd)


## do overlay with sf package functions
miep_int400 <- st_intersection(miep_points_400sf, SonVegMap_sf)


miep_int400 <- miep_int400 %>% 
  mutate(area = st_area(.) %>% as.numeric(),
         proportion = area/502654.8) %>% 
  dplyr::select(point = stationid, everything(), -dateestab, -status, -notes, -locationno, -newxcoord, -newycoord, -OID_COPY)

# write and read the generated shp
st_write(miep_int400, "generated_data/MIEP400_plus_SonVegMap.shp")




miep_svm <- st_read("generated_data/MIEP_plus_SonVegMap.shp")


# summarizing by MAP_CLASS
miep_svm_by_point <- miep_svm %>% 
  data.frame() %>% 
  dplyr::select(point, MAP_CLASS, proportion) %>% 
  group_by(point, MAP_CLASS) %>% 
  summarize(proportion = sum(proportion))  %>% 
  filter(proportion > 0.1) 

%>% 
  summarise(n())



%>% 
  spread(key = MAP_CLASS, value = proportion) %>% 
  mutate_all(funs(coalesce(., 0))) 

%>% 
  group_by(point) %>% 
  summarise(n())

ggplot(data = miep_svm_by_point)+
  geom_col(aes(x = point, y = proportion, fill = MAP_CLASS))+
  theme(axis.text.x=element_text(angle=90,hjust=1))



