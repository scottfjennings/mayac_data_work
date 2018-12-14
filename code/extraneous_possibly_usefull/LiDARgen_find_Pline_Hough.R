# trying to see if I can detect powerlines in the LiDAR data using the Hough transform
# based on this example: https://stats.stackexchange.com/questions/109819/detect-line-in-scatter

library(lidR)
library(tidyverse)
library(sp)

las <- readLAS("generated_data/norm_laz/norm_16R.laz") %>% 
  lasfilter(Z < (100 / 0.3048006096012192), Z > (10 / 0.3048006096012192)) 

las.sp <- LAS_reproject(las)

las_data <- las.sp@data %>% 
  select(x = X, y = Y)

#############################
# Hough transform:
#   for each point find slopes and intercepts that go through the line
#############################

# first, set up a grid of intercepts to cycle through
delta_y <- max(las_data$y) - min(las_data$y)
y_intercepts <- seq(min(las_data$y) - delta_y, max(las_data$y) + delta_y, delta_y/50) # the intercept grid for the lines to consider

# a function that takes a point and a grid of intercepts and returns a data frame of slope-intercept pairs
compute_slopes_and_intercepts <- function(x,y,intercepts) {
  data.frame(intercept=intercepts,
             slope=(y-intercepts) / x)
}

# apply the function above to all points
all_slopes_and_intercepts.list <- apply(las_data, 1, function(point) compute_slopes_and_intercepts(point['x'],point['y'],y_intercepts))
# bind together all resulting data frames 
all_slopes_and_intercepts <- do.call(rbind,all_slopes_and_intercepts.list)



# plot the slope-intercept representation
plot(all_slopes_and_intercepts$intercept, all_slopes_and_intercepts$slope, pch=19,col=rgb(50,50,50,2,maxColorValue=255))
# circle the true value
slope <- (end$y - start$y) / (end$x - start$x)
intercept <- start$y - start$x * slope
points(intercept, slope, col='red', cex = 4)


#################################
# Make a best guess. Bin the data according to a fixed grid and count the number of slope-intercept pairs in each bin
slope_intercepts = all_slopes_and_intercepts
bin_width.slope= (max(slope_intercepts$slope) - min(slope_intercepts$slope))/50
bin_width.intercept= (max(slope_intercepts$intercept) - min(slope_intercepts$intercept))/50
slope_intercepts$slope.cut <- cut(slope_intercepts$slope,seq(min(slope_intercepts$slope), max(slope_intercepts$slope), by=bin_width.slope))
slope_intercepts$intercept.cut <- cut(slope_intercepts$intercept,seq(min(slope_intercepts$intercept), max(slope_intercepts$intercept), by=bin_width.intercept))
accumulator <- aggregate(slope ~ slope.cut + intercept.cut, data=slope_intercepts, length)
head(accumulator[order(-accumulator$slope),]) # the best guesses
(best.grid_cell <- accumulator[which.max(accumulator$slope),c('slope.cut','intercept.cut')]) # the best guess

# as the best guess take the mean of slope and intercept in the best grid cell
best.slope_intercepts <- slope_intercepts[slope_intercepts$slope.cut == best.grid_cell$slope.cut & slope_intercepts$intercept.cut == best.grid_cell$intercept.cut,]
(best.guess <- colMeans(best.slope_intercepts[,1:2],na.rm = TRUE))
points(best.guess['intercept'], best.guess['slope'], col='blue', cex = 4)