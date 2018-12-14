


# lidR doesn't seem to include a funtion to change the projection of a LAS object
# this function changes the object to sp, re-projects it, then changes it back to a LAS
# for some reason the CRS isn't carried along by either as.spatial() or LAS()

norm.las <- readLAS("generated_data/norm_laz/norm_7L.laz")

las <- readLAS("generated_data/clipped_laz/clipped2_7L.laz")

LAS_reproject <- function(oldprojLAS, new.CRS = CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "), ...) {

  oldproj_sp <- as.spatial(oldprojLAS)

proj4string(oldproj_sp) <- CRS("+proj=lcc +lat_1=38.33333333333334 +lat_2=39.83333333333334 +lat_0=37.66666666666666 +lon_0=-122 +x_0=2000000 +y_0=500000.0000000002 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs")

newproj_sp <- spTransform(oldproj_sp, new.CRS)

newproj_sp_df <- data.frame(newproj_sp)


reproj_LAS <- LAS(newproj_sp_df)

reproj_LAS@header@PHB$`X offset` <- 0
reproj_LAS@header@PHB$`Y offset` <- 0
reproj_LAS@header@PHB$`Z offset` <- 0
reproj_LAS@crs <- CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

return(reproj_LAS)
}

