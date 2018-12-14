

# calculating derived metrics of habitat structure
#

# 
# requires running CountPoints_get_point_coords.R
# which means the packages needed here will already be loaded, but here they are for reference
#library(plyr)
#library(tidyverse)
#library(lidR)
library(purrr)

# first load the 2 functions point.deriver() and derived_appender(),
# then see notes below funtions for the workflow to use here

options(scipen = 999)

# load the function, then go down below it for more settings and the call
# this one is a bit overcomplicated with the number of metrics calculated
point.deriver.div.ind <- function(zrad, n.breaks) {

  #zpoint = "1R"
  #zrad = 500
  #norm.las <- las_reader(zpoint)
  point.coords <- get_point_coords(zpoint, ".lcc")
  
  norm.las <- lasclipCircle(norm.las, point.coords$easting.lcc, point.coords$northing.lcc, zrad/0.3048006096012192)
  
  norm.las.data.with.ground <- las_bounder_classifier(norm.las)
  
  

#-------
# div.ind is the Shannon diversity index, following Macarthur and Macarthur (1961) and others following them.
# norm.div.ind is the normalized Shannon diversity index, following Pretzsch (2009). (normalized by the number of bins)
# norm.div.ind is also the same as lidR::entropy, except that this (using layer_rel_dens from above) allows having the bottom bound != 0, following Lesak et al (2011)



veg <- norm.las.data.with.ground  %>% 
  filter( Z > LowerBound) 

num.breaks = n.breaks

zbreaks <- seq(LowerBound, max(veg$Z), by = ((max(veg$Z) - LowerBound)/num.breaks))
zlayer = seq(10, 100, by = 10)

veg.bins <- cut(veg$Z, zbreaks, include.lowest=TRUE, labels = zlayer)

foof <- cbind(veg, veg.bins)


num.returns.veg <- nrow(veg)

  div.ind <- foof %>% 
    group_by(veg.bins) %>%
    summarize(NumRet = n()) %>%
    mutate(RelDens = NumRet/num.returns.veg,
           prop.l.prop = RelDens * log(RelDens)) %>%
    summarise(div.ind = -1 * (sum(prop.l.prop))) %>% 
    mutate(norm.div.ind = div.ind/log(num.breaks),
           point = zpoint,
           CircleRadius = zrad) %>% 
    rename(!!paste("div.ind.b", num.breaks, sep = "") := div.ind,
           !!paste("norm.div.ind.b", num.breaks, sep = "") := norm.div.ind)

return(div.ind)
}

##############################################################################

zpoint <- "1R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "2L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     


zpoint <- "3L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "4L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "5R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "6L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "7L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "8L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "9R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "10R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "11R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "12L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "13L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "14R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "15R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "16R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "HOME2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "HOME3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "MCDO10"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "MCDO6"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "WERA1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "WERA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "HOME5"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "MCDO2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "MCDO11"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "FORK10"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "MCDO12"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "LIIN2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "LIIN3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "MIRA11"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "MIRA12"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "MIRA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "BIHI2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "LIIN1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "LIIN4"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "WERA3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "WEBL1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "WEBL3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "BIHI1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "BIHI3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "BIHI5"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "BIHI6"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "MIRA10"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "BIHI4"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "EABL2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "EARA1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "BEAR1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "EARA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "EARA3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "HIVA1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "HIVA3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     

zpoint <- "HIVA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map2_dfr(zrads, 10, point.deriver.div.ind)
derived_checker_writter("div.ind.b10", foo)     



#################################################################################################
#################################################################################################

# adding a horizontal diversity index to the above derived variables
#-------
myMetrics = function(z)
{
  metrics = list(
    mean.ht   = mean(z),
    max.ht   = max(z),
    num.ret = length(z)
  )
  return(metrics)
}
horiz.diversitizer <- function(zrad = NULL) {
  # for a circle around a given point with radius = zrad (must be <= 500)
  # if you set zrad to 500, the clipping step is skipped since that is already the extent of the normalized laz's
  # input norm.las is a normalized LAs object saved at generated_data/norm_laz
  point.coords <- get_point_coords(zpoint, ".lcc")
  
  norm.las <- lasclipCircle(norm.las, point.coords$easting.lcc, point.coords$northing.lcc, zrad/0.3048006096012192)
  
  norm.las.data.with.ground <- las_bounder_classifier(norm.las)
  
#---  
# all veg  
hex.summary.all <- norm.las %>% 
  lasfilter(Z > lower.bound) %>% 
  grid_hexametrics(myMetrics(Z), hex.rad)

tot.num.ret.num.hex.all <- hex.summary.all %>% 
  summarize(tot.num.ret = sum(num.ret),
            tot.num.hex = length(num.ret))

div.ind.all <- hex.summary.all %>% 
  mutate(rel_dens = num.ret/tot.num.ret.num.hex.all$tot.num.ret,
         prop.l.prop = rel_dens * log(rel_dens)) %>% 
  summarise(horiz_div_ind_all = -1 * (sum(prop.l.prop))) %>% 
  mutate(norm_horiz_div_ind_all = horiz_div_ind_all/log(tot.num.ret.num.hex.all$tot.num.hex))
#---  
# shrub
hex.summary.shrub <- norm.las %>% 
  lasfilter(Z > 0.5 & Z < 5) %>% 
  grid_hexametrics(myMetrics(Z), hex.rad)

tot.num.ret.num.hex.shrub <- hex.summary.shrub %>% 
  summarize(tot.num.ret = sum(num.ret),
            tot.num.hex = length(num.ret))

div.ind.shrub <- hex.summary.shrub %>% 
  mutate(rel_dens = num.ret/tot.num.ret.num.hex.shrub$tot.num.ret,
         prop.l.prop = rel_dens * log(rel_dens)) %>% 
  summarise(horiz_div_ind_shrub = -1 * (sum(prop.l.prop))) %>% 
  mutate(norm_horiz_div_ind_shrub = horiz_div_ind_shrub/log(tot.num.ret.num.hex.shrub$tot.num.hex))
#---  
# tree
hex.summary.tree <- norm.las %>% 
  lasfilter(Z >= 5) %>% 
  grid_hexametrics(myMetrics(Z), hex.rad)

tot.num.ret.num.hex.tree <- hex.summary.tree %>% 
  summarize(tot.num.ret = sum(num.ret),
            tot.num.hex = length(num.ret))

div.ind.tree <- hex.summary.tree %>% 
  mutate(rel_dens = num.ret/tot.num.ret.num.hex.tree$tot.num.ret,
         prop.l.prop = rel_dens * log(rel_dens)) %>% 
  summarise(horiz_div_ind_tree = -1 * (sum(prop.l.prop))) %>% 
  mutate(norm_horiz_div_ind_tree = horiz_div_ind_tree/log(tot.num.ret.num.hex.tree$tot.num.hex))
#---
horiz.div.ind <- bind_cols(div.ind.all, div.ind.shrub, div.ind.tree)
horiz.div.ind <- horiz.div.ind %>% 
  mutate(point = zpoint,
         zrad = zrad,
         hex.rad = hex_rad)
  #------

  outpath <- "generated_data/derived_variables/derived_variables_horiz_div.csv"
  if(!file.exists(outpath))
    write.csv(horiz.div.ind, outpath, row.names = F) # if not, create it
  
  h.div.ind <- read.csv(outpath) # if so, read it then add the next line of data
  h.div.ind.added <- rbind(h.div.ind, horiz.div.ind) %>% 
    unique() 
  write.csv(h.div.ind.added, outpath, row.names = F)
  
}

#################################################################################################
# I'm still not entirely sure what to do about the extreme values
# assigning this since it will need to be used multiple times, and might change
# if you change either of these, you need to also go into horiz.diversitizer() and change the code where the 3 lists in zsplit are being named
lower.bound = 0.01 # Lasek et al used 0.9m as the lower bound
upper.bound = 75 # and for now just removing anything taller than 50m
#################################################################################################


# if additional radii are needed, can redefine zrads and rerun this next ~200 lines
hex.rad = 2
zrads <- c(500, 400, 300, 200, 100, 50)

zpoint <- "1R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)     

zpoint <- "2L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)     

zpoint <- "3L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)     

zpoint <- "4L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "5R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "6L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "7L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "8L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "9R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "10R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "11R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "12L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "13L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "14R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "15R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "16R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "HOME2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "HOME3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "MCDO10"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "MCDO6"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "WERA1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "WERA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "HOME5"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "MCDO2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "MCDO11"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "FORK10"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "MCDO12"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "LIIN2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "LIIN3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "MIRA11"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "MIRA12"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "MIRA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "BIHI2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "LIIN1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "LIIN4"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "WERA3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "WEBL1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "WEBL3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "BIHI1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "BIHI3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "BIHI5"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "BIHI6"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "MIRA10"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "BIHI4"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "EABL2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "EARA1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "BEAR1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "EARA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "EARA3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "HIVA1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "HIVA3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)

zpoint <- "HIVA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, horiz.diversitizer)






#################################################################################################
#################################################################################################

####
# playing with the diversity index

foo <- data.frame(plot = c("A", "B", "C", "D"),
                  forb = c(39, 20, 1, 59.98),
                  shrub = c(20, 20, 20, 0.01),
                  tree = c(1, 20, 39, 0.01))


foo <- foo %>% 
  mutate(tot = forb + shrub + tree,
         forb_prop = forb / tot,
         shrub_prop = shrub / tot,
         tree_prop = tree / tot,
         log_forb_prop = log(forb_prop),
         log_shrub_prop = log(shrub_prop),
         log_tree_prop = log(tree_prop),
         div_ind = -1 * ((forb_prop *log_forb_prop) +
                           (shrub_prop *log_shrub_prop) +
                           (tree_prop *log_tree_prop)))




#################################################################################################
#################################################################################################

# following Lesak et al (2011) (http://www.sciencedirect.com/science/article/pii/S0034425711001271) as a rough guide


# load the function, then go down below it for more settings and the call
# this one is a bit overcomplicated with the number of metrics calculated
point.deriver_Lesak <- function(norm.las, zrad = NULL) {
  # function to calculate all the derived variables from Lesak et al (2011) 
  # for a circle around a given point with radius = zrad (must be <500)
  # if you set zrad to 500, the clipping step is set since that is already the extent of the normalized laz's
  # input norm.las is a normalized LAs object saved at generated_data/norm_laz
  
  #if(!is.null(zrad))  # allows leaving zrad blank, decided its better to be implicit about the desired radius
  if(zrad < 500)
    norm.las <- lasclipCircle(norm.las, point.coords$easting.lcc, point.coords$northing.lcc, zrad/0.3048006096012192)
  if (zrad > 500) 
    stop("Only radii <= 500 allowed")
  #------- 
  
  norm.las.data.with.ground <- norm.las@data %>% 
    mutate(Z = Z * 0.3048006096012192,
           Z = ifelse(Z < 0, 0, Z)) %>% 
    filter(Z < upper.bound) 
  
  norm.las.data.veg <- norm.las.data.with.ground  %>% 
    filter( Z > lower.bound) 
  
  num.returns <- nrow(norm.las.data.with.ground)
  num.returns.veg <- nrow(norm.las.data.veg)
  #-------
  
  p_veg <- num.returns.veg/num.returns
  #-------
  
  las.summary <- norm.las.data.veg %>% 
    summarise(mean.ht = mean(Z),
              cv.ht = sd(Z)/mean(Z))
  #-------
  
  z.quants <- quantile(norm.las.data.veg$Z, probs = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1))
  z.quants.df <- data.frame(unlist(z.quants, use.names=FALSE)) %>% 
    mutate(point = zpoint,
           quant.level = paste("h_", seq(10, 100, by = 10), sep = "")) 
  names(z.quants.df)[1] <- "quant.val"
  z.quants.df.wide <- z.quants.df %>% 
    spread(quant.level, quant.val)
  #-------
  
  dens.proper <- function(zprop){
    zbase.prop <- (max(norm.las.data.veg$Z) - lower.bound) * 0.1
    dens.prop.lo.div <- lower.bound + (zbase.prop * zprop)
    dens.prop.hi.div <- lower.bound + (zbase.prop * (zprop +1))
    num.prop <- norm.las.data.veg %>% 
      filter(Z > dens.prop.lo.div, Z <= dens.prop.hi.div) %>%
      summarise(dens.prop = n())
    dens.prop <- num.prop/num.returns.veg
    return(dens.prop)
  }
  
  znums <- 0:9
  dens.props <- ldply(znums, dens.proper)
  dens.props$prop <- c("P_10", "P_20", "P_30", "P_40", "P_50", "P_60", "P_70", "P_80", "P_90", "P_100")
  
  #-------
  # this is the normalized Shannon diversity index, following Pretzsch (2009)
  # it is also the same as lidR::entropy, except that this (using dens.props from above) allows having the bottom bound != 0, which was what Lesak et al (2011) did
  div.ind <- dens.props %>% 
    mutate(l.prop = log(dens.prop),
           prop.l.prop = dens.prop * l.prop) %>% 
    summarise(div.ind = -1 * (sum(prop.l.prop)/log(nrow(dens.props))))
  #-------
  
  dens.props.wide <- dens.props %>% 
    mutate(point = zpoint) %>% 
    spread(prop, dens.prop)
  #-------
  
  point.derived <- cbind(full_join(z.quants.df.wide, dens.props.wide), div.ind, las.summary, p_veg)
  
  point.derived <- point.derived %>% 
    mutate(P_under = P_10 + P_20,
           P_mid = P_30 + P_40 + P_50 + P_60,
           P_can = P_70 + P_80 + P_90 + P_100,
           P_u_m = P_under/P_mid,
           P_u_c = P_under/P_can,
           P_m_c = P_mid/P_can) %>% 
    dplyr::select(point, h_10, h_20, h_30, h_40, h_50, h_60, h_70, h_80, h_90, h_100, P_10, P_20, P_30, P_40, P_50, P_60, P_70, P_80, P_90, P_100, div.ind, mean.ht, cv.ht, p_veg, P_under, P_mid, P_can, P_u_m, P_u_c, P_m_c)
  
  outpath <- "generated_data/derived_variables/derived_variables_ptiles.csv"
  if(!file.exists(outpath))
    write.csv(point.derived, outpath, row.names = F)
  
  derived <- read.csv(outpath)
  point.derived.added <- rbind(derived, point.derived) %>% 
    unique() 
  write.csv(point.derived.added, outpath, row.names = F)
}



#################################################################################################
#################################################################################################
# build the saved file with derived variables for each point
# will write the file for the first time if it doesn't already exist
appender <- function(zrad = NULL, point.derived){
  if(is.null(zrad))
    zrad <- 500 # here this is only used for generating outpath, not for clipping
  
  #outpath <- paste("generated_data/derived_variables/derived_variables_", zrad, "m.csv", sep = "")
  outpath <- "generated_data/derived_variables/derived_variables.csv"
  if(!file.exists(outpath))
    write.csv(point.derived, outpath, row.names = F)
  
  derived <- read.csv(outpath)
  point.derived.added <- rbind(derived, point.derived) %>% 
    unique() 
  write.csv(point.derived.added, outpath, row.names = F)
}

#################################################################################################
#################################################################################################
# bundle the 2 functions
deriver_appender <- function(norm.las, zrad) {
  point.derived <- point.deriver(norm.las, zrad)
  appender(zrad, point.derived)
}

#################################################################################################
#################################################################################################
# I'm still not entirely sure what to do about the extreme values
# assigning this since it will need to be used multiple times, and might change
lower.bound = 0.1 # Lasek et al used 0.9m as the lower bound
upper.bound = 50 # and for now just removing anything taller than 50m
#################################################################################################
#################################################################################################




# now to the workflow
# specify which point you want to work with 
# for each point, you can edit the zpoint assignment then run everything from 143 - 159 

zpoint <- "13L_noPline"
zpoint <- "13L"

##########
# then start calling functions
# only need to do these 2 once for each point
# reading the file in takes a little time so don't want to repeat it for each radius circle for a given point
point.coords <- get.point.coords(zpoint, ".lcc")
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))


# then call for each desired radius (with this bundle, need to actually )
deriver_appender(norm.las, 500)
deriver_appender(norm.las, 400)
deriver_appender(norm.las, 300)
deriver_appender(norm.las, 200)
deriver_appender(norm.las, 100)
deriver_appender(norm.las, 50)
