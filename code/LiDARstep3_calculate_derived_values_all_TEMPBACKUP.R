

# calculating derived metrics of habitat structure
#

# 
# requires running CountPoints_get_point_coords.R
# which means the packages needed here will already be loaded, but here they are for reference
#library(plyr)
#library(tidyverse)
#library(lidR)
library(purrr)
library(hexbin)

# first load the 2 functions point.deriver() and derived_appender(),
# then see notes below funtions for the workflow to use here

options(scipen = 999)


zrad = 500
# load the function, then go down below it for more settings and the call
# this one is a bit overcomplicated with the number of metrics calculated
point.deriver <- function(zrad = NULL) {
  # for a circle around a given point with radius = zrad (must be <= 500)
  # if you set zrad to 500, the clipping step is skipped since that is already the extent of the normalized laz's
  # input norm.las is a normalized LAs object saved at generated_data/norm_laz
  
  point.coords <- get.point.coords(zpoint, ".lcc")  
  
  #if(!is.null(zrad))  # allows leaving zrad blank, decided its better to be explicit about the desired radius
  if(zrad < 500)
    norm.las <- lasclipCircle(norm.las, point.coords$easting.lcc, point.coords$northing.lcc, zrad/0.3048006096012192)
  if (zrad > 500) 
    stop("Only radii <= 500 allowed")
  #------- 
  # some cleaning  
  norm.las.m <- norm.las@data %>% 
    select(X, Y, Z) %>% 
    mutate(Z = Z * 0.3048006096012192,  # convert feet to meters
           Z = ifelse(Z < 0, 0, Z)) %>%  # change negatives to 0 - this may end up being the wrong thing to do, but most of these are just barely negative
    filter(Z < upper.bound) 
  # just the 'ground' returns  
  norm.las.ground <- norm.las.m %>% 
    filter(Z < lower.veg)
  # just the 'veg' returns and filtering possible outlier heights
  norm.las.veg <- norm.las.m %>% 
    filter(Z > lower.veg) %>% 
    filter(Z < quantile(Z, probs = c(.99)))
  # get a height to further filter possible outlier heights   
  upper.veg = quantile(norm.las.veg$Z, probs = c(.99))
  
  
  #  norm.las.data.veg <- norm.las.data.m  %>% 
  filter( Z > lower.veg) 
  # count the totals
  num.returns <- nrow(norm.las.data.m)
  num.returns.veg <- nrow(norm.las.data.veg)
  #-------
  # calculate percent covers
  allveg_per_cov <- 100 * (num.returns.veg/num.returns) 
  ground_per_cov <- 100 * (num.returns - num.returns.veg)/num.returns
  #-------
  # classify veg returns
  norm.las.data.veg <- norm.las.data.veg %>%
    mutate(layer = ifelse(Z <= 5, "shrub", "tree"))
  #   
  layer_summs <- norm.las.data.veg %>% 
    group_by(layer) %>% 
    summarise(nreturns = n(),
              #max_layer_ht = max(Z),
              mean_layer_ht = mean(Z),
              med_layer_ht = median(Z),
              quant99 = quantile(Z, probs = c(.99)),
              quant95 = quantile(Z, probs = c(.95)),
              quant90 = quantile(Z, probs = c(.9)),
              #quant75 = quantile(Z, probs = c(.75)),
              #quant25 = quantile(Z, probs = c(.25)),
              VDR = 1-((quant99 - med_layer_ht)/quant99)) %>% 
    mutate(point = zpoint)
  
  layer_summs_wide <- layer_summs %>%
    gather(varb, val, -point, -layer) %>% 
    mutate(newkey = paste(layer, varb, sep = "_")) %>% 
    select(-layer, -varb) %>% 
    spread(newkey, val)
  
  
  summs <- full_join(all_summs, layer_summs_wide)
  
  #-------
  # calculate relative density and percent cover of each veg layer
  
  summs <- summs %>% 
    mutate(tree_per_cov = 100 * (tree_nreturns/num.returns),
           tree_rel_dens = tree_nreturns/(num.returns.veg),
           shrub_per_cov = 100 * (shrub_nreturns/(num.returns - tree_nreturns)),
           shrub_rel_dens = shrub_nreturns/(num.returns.veg)) %>% 
    select(point, starts_with("all"), starts_with("shrub"), starts_with("tree"))
  
  
  
  #-------
  # div.ind is the Shannon diversity index, following Macarthur and Macarthur (1961) and others following them.
  # norm.div.ind is the normalized Shannon diversity index, following Pretzsch (2009). (normalized by the number of bins)
  # norm.div.ind is also the same as lidR::entropy, except that this (using layer_rel_dens from above) allows having the bottom bound != 0, following Lesak et al (2011)
  
  # calculate the relative density of returns in each of X (defined below by znums) equal-sized bins between top and bottom of veg  
  dens.proper <- function(zprop){
    zbase.prop <- (upper.veg - lower.veg) * 0.1
    dens.prop.lo.div <- lower.veg + (zbase.prop * zprop)
    dens.prop.hi.div <- lower.veg + (zbase.prop * (zprop +1))
    num.prop <- norm.las.data.veg %>% 
      filter(Z > dens.prop.lo.div, Z <= dens.prop.hi.div) %>%
      dplyr::summarise(dens.prop = n())
    dens.prop <- num.prop/num.returns.veg
    return(dens.prop)
  }
  
  znums <- 0:9
  dens.props <- map_df(znums, dens.proper)
  dens.props$prop <- c("P_10", "P_20", "P_30", "P_40", "P_50", "P_60", "P_70", "P_80", "P_90", "P_100")
  
  
  # calculate Shannon diversity index based on the above-generated bins
  div.ind <- dens.props %>% 
    mutate(prop.l.prop = dens.prop * log(dens.prop)) %>% 
    summarise(div.ind = -1 * (sum(prop.l.prop))) %>% 
    mutate(norm.div.ind = div.ind/log(10))
  #-----------------------------------------------------------------
  # now horizontal veg structure
  
  # some metrics to calculate for each hexagon
  myMetrics = function(z)
  {
    metrics = list(
      mean.ht   = mean(z),
      max.ht   = max(z),
      num.ret = length(z)
    )
    return(metrics)
  }
  
  # horizontal diversity index for all veg  
  hex.summary.all <- norm.las %>% 
    lasfilter(Z > lower.veg) %>% 
    grid_hexametrics(myMetrics(Z), hex.rad)
  
  horiz.num.ret.num.hex.all <- hex.summary.all %>% 
    summarize(tot.num.ret.all = sum(num.ret), # total number of returns
              tot.num.hex.all = length(num.ret)) # number of hex's; number of groups for diversity index
  
  horiz.div.ind.all <- hex.summary.all %>% 
    mutate(rel_dens = num.ret/tot.num.ret.num.hex.all$tot.num.ret.all,
           prop.l.prop = rel_dens * log(rel_dens)) %>% 
    summarise(horiz_div_ind_all = -1 * (sum(prop.l.prop))) %>% 
    mutate(norm_horiz_div_ind_all = horiz_div_ind_all/log(tot.num.ret.num.hex.all$tot.num.hex))
  
  horiz.ret.div.ind.all <- cbind(tot.num.ret.num.hex.all, horiz.div.ind.all)
  #---  
  # horizontal diversity index for shrub layer
  hex.summary.shrub <- norm.las %>% 
    lasfilter(Z > 0.5 & Z < 5) %>% 
    grid_hexametrics(myMetrics(Z), hex.rad)
  
  horiz.num.ret.num.hex.shrub <- hex.summary.shrub %>% 
    summarize(tot.num.ret.shrub = sum(num.ret),
              tot.num.hex.shrub = length(num.ret))
  
  horiz.div.ind.shrub <- hex.summary.shrub %>% 
    mutate(rel_dens = num.ret/tot.num.ret.num.hex.shrub$tot.num.ret.shrub,
           prop.l.prop = rel_dens * log(rel_dens)) %>% 
    summarise(horiz_div_ind_shrub = -1 * (sum(prop.l.prop))) %>% 
    mutate(norm_horiz_div_ind_shrub = horiz_div_ind_shrub/log(tot.num.ret.num.hex.shrub$tot.num.hex.shrub))
  
  tot.ret.div.ind.shrub <- cbind(tot.num.ret.num.hex.shrub, div.ind.shrub)
  
  #---  
  ## horizontal diversity index for tree layer
  hex.summary.tree <- norm.las %>% 
    lasfilter(Z >= 5) %>% 
    grid_hexametrics(myMetrics(Z), hex.rad)
  
  tot.num.ret.num.hex.tree <- hex.summary.tree %>% 
    summarize(tot.num.ret.tree = sum(num.ret),
              tot.num.hex.tree = length(num.ret))
  
  div.ind.tree <- hex.summary.tree %>% 
    mutate(rel_dens = num.ret/tot.num.ret.num.hex.tree$tot.num.ret.tree,
           prop.l.prop = rel_dens * log(rel_dens)) %>% 
    summarise(horiz_div_ind_tree = -1 * (sum(prop.l.prop))) %>% 
    mutate(norm_horiz_div_ind_tree = horiz_div_ind_tree/log(tot.num.ret.num.hex.tree$tot.num.hex.tree))
  
  tot.ret.div.ind.tree <- cbind(tot.num.ret.num.hex.tree, div.ind.tree)
  #---
  
  horiz.div.ind <- bind_cols(tot.ret.div.ind.all, tot.ret.div.ind.shrub, tot.ret.div.ind.tree)
  horiz.div.ind <- horiz.div.ind %>% 
    mutate(#point = zpoint,
      #zrad = zrad,
      hex.rad = hex.rad)
  
  #-------
  
  point.derived <- cbind(zrad, num.returns, allveg_per_cov, ground_per_cov, summs, div.ind, horiz.div.ind) %>% 
    select(point, radius = zrad, everything())
  
  #------
  # now check if the derived_variables file already exists, 
  outpath <- "generated_data/derived_variables/derived_variables_USETHISONE.csv"
  if(!file.exists(outpath))
    write.csv(point.derived, outpath, row.names = F) # if not, create it
  
  derived <- read.csv(outpath) # if so, read it then add the next line of data
  point.derived.added <- rbind(derived, point.derived) %>% 
    unique() 
  write.csv(point.derived.added, outpath, row.names = F)
  
}

file.checker.writer <- function(zfile.name){
  outpath <- paste("generated_data/derived_variables/", zfile.name, ".csv")
  if(!file.exists(outpath))
    write.csv(point.derived, outpath, row.names = F) # if not, create it
  
  derived <- read.csv(outpath) # if so, read it then add the next line of data
  point.derived.added <- rbind(derived, point.derived) %>% 
    unique() 
  write.csv(point.derived.added, outpath, row.names = F)
  
}

#################################################################################################
# I'm still not entirely sure what to do about the extreme values
# assigning this since it will need to be used multiple times, and might change
# if you change either of these, you need to also go into point.deriver() and change the code where the 3 lists in zsplit are being named
lower.veg = 0.5 # Lasek et al used 0.9m as the lower bound
upper.bound = 75 # and for now just removing anything taller than 50m
hex.rad = 4 # radius of hexagon for calculating horizontal diversity index
#################################################################################################


# if additional radii are needed, can redefine zrads and rerun this next ~200 lines
zrads <- c(500, 400, 300, 200, 100, 50)

zpoint <- "1R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)     

zpoint <- "2L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)     

zpoint <- "3L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)     

zpoint <- "4L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "5R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "6L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "7L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "8L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "9R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "10R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "11R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "12L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "13L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "14R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "15R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "16R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "HOME2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "HOME3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "MCDO10"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "MCDO6"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "WERA1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "WERA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "HOME5"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "MCDO2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "MCDO11"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "FORK10"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "MCDO12"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "LIIN2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "LIIN3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "MIRA11"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "MIRA12"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "MIRA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "BIHI2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "LIIN1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "LIIN4"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "WERA3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "WEBL1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "WEBL3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "BIHI1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "BIHI3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "BIHI5"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "BIHI6"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "MIRA10"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "BIHI4"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "EABL2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "EARA1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "BEAR1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "EARA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "EARA3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "HIVA1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "HIVA3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

zpoint <- "HIVA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
map(zrads, point.deriver)

