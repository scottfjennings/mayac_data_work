

# calculating derived metrics of habitat structure
# this script calculates the quantile heights for each of the different veg layers

# 
# requires running CountPoints_get_point_coords.R
# which means the packages needed here will already be loaded, but here they are for reference
#library(plyr)
#library(tidyverse)
#library(lidR)
library(purrr)

# first load the 2 functions point.deriver_quant_hts() and derived_appender(),
# then see notes below funtions for the workflow to use here

options(scipen = 999)

# load the function, then go down below it for more settings and the call
# this one is a bit overcomplicated with the number of metrics calculated
point.deriver_quant_hts <- function(zrad = NULL) {
  # for a circle around a given point with radius = zrad (must be <= 500)
  # if you set zrad to 500, the clipping step is skipped since that is already the extent of the normalized laz's
  # input norm.las is a normalized LAs object saved at generated_data/norm_laz
  
  point.coords <- get.point.coords(zpoint, ".lcc")  
  #zrad = 500
  #if(!is.null(zrad))  # allows leaving zrad blank, decided its better to be explicit about the desired radius
  if(zrad < 500)
    norm.las <- lasclipCircle(norm.las, point.coords$easting.lcc, point.coords$northing.lcc, zrad/0.3048006096012192)
  if (zrad > 500) 
    stop("Only radii <= 500 allowed")
  #------- 
  
  norm.las.data.with.ground <- norm.las@data %>% 
    select(X, Y, Z) %>% 
    mutate(Z = Z * 0.3048006096012192,  # convert feet to meters
           Z = ifelse(Z < 0, 0, Z)) %>%  # change negatives to 0 - this may end up being the wrong thing to do, but most of these are just barely negative
    filter(Z < upper.bound) 
  
  norm.las.data.veg <- norm.las.data.with.ground  %>% 
    filter( Z > lower.bound) 
  # count the totals
  num.returns <- nrow(norm.las.data.with.ground)
  num.returns.veg <- nrow(norm.las.data.veg)
  #-------
  # veg proportion
  allveg_per_cov <- 100 * (num.returns.veg/num.returns) 
  ground_per_cov <- 100 * (num.returns - num.returns.veg)/num.returns
  #-------
  
  las.summary <- norm.las.data.veg %>% 
    summarise(mean.ht = mean(Z),
              cv.ht = sd(Z)/mean(Z))
  #-------
  # split the data into the standard veg layers, then summarize by layer
  zbreaks <- c(lower.bound, 0.5, 5, upper.bound)
  zsplit <- split(norm.las.data.veg, cut(norm.las.data.veg$Z, zbreaks, include.lowest=TRUE))
  forb_layer <- zsplit[1]$`[0.01,0.5]`
forb_quants <- quantile(forb_layer$Z, probs = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)) %>% 
  data.frame() %>% 
  mutate(quant = seq(10, 100, by = 10),
         layer = "forb")
colnames(forb_quants)[1] <- "quant_ht"


shrub_layer <- zsplit[2]$`(0.5,5]`
  shrub_quants <- quantile(shrub_layer$Z, probs = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)) %>% 
    data.frame() %>% 
    mutate(quant = seq(10, 100, by = 10),
           layer = "shrub")
  colnames(shrub_quants)[1] <- "quant_ht"
  
  
  tree_layer <- zsplit[3]$`(5,75]`
  tree_quants <- quantile(tree_layer$Z, probs = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)) %>% 
    data.frame() %>% 
    mutate(quant = seq(10, 100, by = 10),
           layer = "tree")
  colnames(tree_quants)[1] <- "quant_ht"
  
  all_quants <- bind_rows(tree_quants, shrub_quants, forb_quants) 
  
  
  all_quants_wide <- all_quants %>%
    mutate(quant_layer = paste("quant", quant, layer, sep = "_")) %>% 
    select(-quant, -layer) %>% 
    spread(quant_layer, quant_ht) %>% 
    mutate(point = zpoint, radius = zrad)
 

  
  #------
  # now check if the derived_variables file already exists, 
  
  outpath <- "generated_data/derived_variables/derived_variables_quant_hts.csv"
  if(!file.exists(outpath))
    write.csv(all_quants_wide, outpath, row.names = F) # if not, create it
  
  zall_quants_wide <- read.csv(outpath) # if so, read it then add the next line of data
  all_quants_wide.added <- rbind(zall_quants_wide, all_quants_wide) %>% 
    unique() 
  write.csv(all_quants_wide.added, outpath, row.names = F)
  
}

#################################################################################################
# I'm still not entirely sure what to do about the extreme values
# assigning this since it will need to be used multiple times, and might change
# if you change either of these, you need to also go into point.deriver_quant_hts() and change the code where the 3 lists in zsplit are being named
lower.bound = 0.01 # Lasek et al used 0.9m as the lower bound
upper.bound = 75 # and for now just removing anything taller than 50m
#################################################################################################


# if additional radii are needed, can redefine zrads and rerun this next ~200 lines
zrads <- c(500, 400, 300, 200, 100, 50)

zpoint <- "1R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)     

zpoint <- "2L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)     

zpoint <- "3L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)     

zpoint <- "4L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "5R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "6L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "7L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "8L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "9R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "10R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "11R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "12L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "13L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "14R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "15R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "16R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "HOME2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "HOME3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "MCDO10"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "MCDO6"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "WERA1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "WERA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "HOME5"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "MCDO2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "MCDO11"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "FORK10"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "MCDO12"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "LIIN2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "LIIN3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "MIRA11"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "MIRA12"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "MIRA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "BIHI2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "LIIN1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "LIIN4"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "WERA3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "WEBL1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "WEBL3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "BIHI1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "BIHI3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "BIHI5"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "BIHI6"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "MIRA10"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "BIHI4"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "EABL2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "EARA1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "BEAR1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "EARA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "EARA3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "HIVA1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "HIVA3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

zpoint <- "HIVA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver_quant_hts)

