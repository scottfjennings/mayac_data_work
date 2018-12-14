the R code files in this folder are grouped by which data set they largely operate on, reflected by the first portion of the file name (before the first "_"):

Birds - the actual bird count data
CountPoints - the location data for the count points
          -> the get_point_coords function is used in many of the LiDAR files
LiDAR - the raw LiDAR point cloud data
    LiDARgen - general functions and prep for working with the LiDAR data
    LiDARstepX - ordered steps for actually preparing the raw LiDAR data for the analysis
PineFlatVeg - the ground-collected vegetation data at the PFR count points
SVM - Sonoma Veg Map, the derived data, classifications, etc that the SVM folks generated. These likely won't be used.