

## This is the "statistical method" from https://pdal.io/stages/filters.outlier.html#filters-outlier adapted for R

library(nabor)
library(lidR)

zpoint <- "7L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))

# norm.las.reproj <- LAS_reproject(norm.las) # I can't figure out where LAS_reproject() came from. Google can't find it. saving for now in case I find where I made this function.

norm.las.reproj@data$Z <- ifelse(norm.las.reproj@data$Z < 0, 0,
                                 norm.las.reproj@data$Z)

outlier_filter <- function(las, mean.k, multiplier){
  
  # mean.k is the number of nearest neighbors to calculate mean distance to nearest neighbor
  # multiplier is the value used to determin the threshold value (relative to the global mean nearest neighbor) for outlier/not outlier

las.data <- las@data
mean.k1 <- mean.k +1
foo <- knn(las.data, k = mean.k1)

foo.dist <- data.frame(foo$nn.dists[,2:mean.k1])

foo.mean.dists <- rowMeans(foo.dist)

global.mean.dist <- mean(foo.mean.dists)

global.sd.dist <- sd(foo.mean.dists)

threshold <- global.mean.dist + multiplier*global.sd.dist

outlier <- foo.mean.dists < threshold

new.las.data <- cbind(las.data, outlier)

filt.new.las.data <- new.las.data %>% 
  filter(outlier == FALSE)

filt.las <- LAS(filt.new.las.data)
return(filt.las)
}

filt.las.5_2.2 <- outlier_filter(norm.las, 5, 2.2)

filt.las.5_2 <- outlier_filter(norm.las, 5, 2)

filt.las.10_2 <- outlier_filter(norm.las, 10, 2)

filt.las.20_2 <- outlier_filter(norm.las, 20, 2)

filt.las.30_2 <- outlier_filter(norm.las, 30, 2)

filt.las.30_3 <- outlier_filter(norm.las, 30, 3)

filt.las.10_3 <- outlier_filter(norm.las, 10, 3)

##

foof <- filter(norm.las.reproj@data, Z > 100, Z < 200)

norm.las.reproj@data$Z <- ifelse(norm.las.reproj@data$Z < 200, 0,
                                 norm.las.reproj@data$Z)


zoof <- norm.las.reproj@data

zoofer <- zoof %>% 
  filter(Z > 200, Z < 300)

las.zoofer <- LAS(zoofer)
