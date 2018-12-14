library(tidyverse)
library(sp)
library(rgdal)
library(sf)
library(rgeos)
library(raster)
# ------------------


study_area <- st_read("generated_data/mayac_study_area.kml")

study_area_sp <- study_area %>%  
  st_zm(.) %>% 
  as(., "Spatial") 

study_area_sp_lcc <- spTransform(study_area_sp, CRS("+proj=lcc +lat_1=38.33333333333334 +lat_2=39.83333333333334 +lat_0=37.66666666666666 +lon_0=-122 +x_0=2000000 +y_0=500000.0000000001 +ellps=GRS80 +units=us-ft +no_defs"))

# these DTM rasters are already in the same State Plane projection as the laz files, so no need to reproject

# the clipped laz files have their CRS = NA
sausal_hydroDTM <- raster("Sonoma_Veg_Map/SVM_derived_products/Sausal Creek_HYDROFLATTENED_BARE_EARTH.tif")
mcdonnell_hydroDTM <- raster("Sonoma_Veg_Map/SVM_derived_products/McDonnell Creek_HYDROFLATTENED_BARE_EARTH.tif")
big_sulphur_hydroDTM <- raster("Sonoma_Veg_Map/SVM_derived_products/Upper Big Sulphur Creek_HYDROFLATTENED_BARE_EARTH.tif")
little_sulphur_hydroDTM <- raster("Sonoma_Veg_Map/SVM_derived_products/Upper Little Sulfur Creek_HYDROFLATTENED_BARE_EARTH.tif")
gird_creek_hydroDTM <- raster("Sonoma_Veg_Map/SVM_derived_products/Gird Creek_HYDROFLATTENED_BARE_EARTH.tif")



sausal_in_study_area <- mask(sausal_hydroDTM, study_area_sp_lcc)
mcdonnell_in_study_area <- mask(mcdonnell_hydroDTM, study_area_sp_lcc)
big_sulphur_in_study_area <- mask(big_sulphur_hydroDTM, study_area_sp_lcc)
little_sulphur_in_study_area <- mask(little_sulphur_hydroDTM, study_area_sp_lcc)
gird_in_study_area <- mask(gird_creek_hydroDTM, study_area_sp_lcc)

sausal_in_study_area_trim <- trim(sausal_in_study_area)
mcdonnell_in_study_area_trim <- trim(mcdonnell_in_study_area)
big_sulphur_in_study_area_trim <- trim(big_sulphur_in_study_area)
little_sulphur_in_study_area_trim <- trim(little_sulphur_in_study_area)
gird_in_study_area_trim <- trim(gird_in_study_area)


mayac_hydroDTM <- merge(sausal_in_study_area_trim, 
                        merge(mcdonnell_in_study_area_trim,
                              merge(big_sulphur_in_study_area_trim,
                                    merge(little_sulphur_in_study_area_trim, gird_in_study_area_trim))))


writeRaster(mayac_hydroDTM, "generated_data/mayac_hydroDTM.tif", overwrite=TRUE)
