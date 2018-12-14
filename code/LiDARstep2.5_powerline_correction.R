


# some of the points have a big powerline running through them, 
# which impacts the derived variables calculated from the LiDAR data.
#
# I traced the line of the powerlines (3 different segments) in Gaia GPS desktop
# now my plan is to try and make a polygon from these lines, 
# clip/overlay the transformed laz files for the points that need it,
# within the powerline polygon, remove all points higher than 3 m (to keep shrub returns)
# then re-join the powerline with the non-powerline objects

library(lidR)
# v 1.3.1



# taking pline from make_pline.R
pline_geom <- st_geometry(pline)
pline_geom_matr <- pline_geom[[1]][1][[1]]


pline_geom_matr_x <- pline_geom_matr[,1]
pline_geom_matr_y <- pline_geom_matr[,2]



#norm.las <- readLAS("generated_data/norm_laz/norm_7L.laz")

pliner <- function(zpoint, in_filt_m = 3, out_filt_m = 100) {
  #zpoint <- "13L"
  #in_filt_m <- 3
  #out_filt_m <- 100
  
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))

norm.las <- norm.las %>% 
  lasfilter(Z < (out_filt_m / 0.3048006096012192))
  
norm.las_sp_lcc <- as.spatial(norm.las)

proj4string(norm.las_sp_lcc) <- CRS("+proj=lcc +lat_1=38.33333333333334 +lat_2=39.83333333333334 +lat_0=37.66666666666666 +lon_0=-122 +x_0=2000000 +y_0=500000.0000000002 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs")

norm.las_sp_utm <- spTransform(norm.las_sp_lcc, CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

norm.las_df_utm <- data.frame(norm.las_sp_utm)


norm.las_utm <- LAS(norm.las_df_utm)

norm.las_utm@header@PHB$`X offset` <- 0
norm.las_utm@header@PHB$`Y offset` <- 0
norm.las_utm@header@PHB$`Z offset` <- 0
norm.las_utm@crs <- CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

clipped_in <- lasclipPolygon(norm.las_utm, pline_geom_matr_x, pline_geom_matr_y)

clipped_out <- lasclipPolygon(norm.las_utm, pline_geom_matr_x, pline_geom_matr_y, inside = F)


clipped_in_filt <- clipped_in %>% 
  lasfilter(Z < (in_filt_m / 0.3048006096012192))


clipped_out_filt <- clipped_out %>% 
  lasfilter(Z < (out_filt_m / 0.3048006096012192))


bound_disk <- rbind(clipped_in_filt@data, clipped_out_filt@data) %>% 
  mutate(Z = ifelse(Z < 0, 0, Z))

new_las <- LAS(bound_disk, header = clipped_out@header)
new_las@crs <- CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

output_name <- paste("generated_data/norm_laz/norm_", zpoint, "_noPline.laz", sep = "")

writeLAS(new_las, output_name)

}



# points needing powerline data removed (w/in 500 m of pline)
pliner("7L") 
pliner("8L")
pliner("WERA2")
pliner("10R")
pliner("12L")
pliner("13L")
pliner("15R")
pliner("16R")

readLAS("generated_data/norm_laz/norm_BIHI1.laz") %>% 
  lasfilter(Z < (100 / 0.3048006096012192)) %>% 
  plot()

readLAS("generated_data/norm_laz/norm_16R_noPline.laz") %>% 
  plot()

readLAS("generated_data/norm_laz/norm_14R.laz") %>%  
  plot()
