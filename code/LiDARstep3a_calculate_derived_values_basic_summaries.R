

# calculating derived metrics of habitat structure
#

# 
# requires running LiDARstep3_read_prep_data.R
# which means the packages needed here will already be loaded, but here they are for reference
#library(plyr)
#library(tidyverse)
#library(lidR)
library(purrr)

# LiDARstep3_read_prep_data.R also has these functions: get_point_coords(), las_reader() and point_prepper().

options(scipen = 999)


#########################


# 1st version, works fine, but see notes for 2nd version

basic_summer <- function(zrad){
  
  point.coords <- get_point_coords(zpoint, ".lcc")
  
  norm.las <- lasclipCircle(norm.las, point.coords$easting.lcc, point.coords$northing.lcc, zrad/0.3048006096012192)
  
  norm.las.data.with.ground <- las_bounder_classifier(norm.las)
  
  
  num.returns <- norm.las.data.with.ground %>% 
    summarise(NumReturns = n()) %>% 
    mutate(point = zpoint,
           CircleRadius = zrad) 
  
  veg.per.cov <- norm.las.data.with.ground %>% 
    filter(VegGround == "veg") %>% 
    summarise(NumVegReturns = n()) %>% 
    summarise(VegPerCov =  NumVegReturns/num.returns$NumReturns)
  
  ground.per.cov <- norm.las.data.with.ground %>% 
    filter(VegGround == "ground") %>% 
    summarise(NumGroundReturns = n()) %>% 
    summarise(GroundPerCov = NumGroundReturns/num.returns$NumReturns)
  
  all.veg.summ <- norm.las.data.with.ground %>% 
    filter(VegGround == "veg") %>% 
    summarise(nVegReturns = n(),
              MaxVegHt = max(Z),
              MeanVegHt = mean(Z),
              MedVegHt = median(Z))
  
  shrub.summ <- norm.las.data.with.ground  %>% 
    filter(VegGround == "veg", layer == "shrub") %>%
    summarise(nShrubReturns = n(),
              MaxShrubHt = max(Z),
              MeanShrubHt = mean(Z),
              MedShrubHt = median(Z))
  
  tree.summ <- norm.las.data.with.ground  %>% 
    filter(VegGround == "veg", layer == "tree") %>%
    summarise(nTreeReturns = n(),
              MaxTreeHt = max(Z),
              MeanTreeHt = mean(Z),
              MedTreeHt = median(Z))
  
  
  basic.summs <- cbind(num.returns, veg.per.cov, ground.per.cov, all.veg.summ, shrub.summ, tree.summ) %>% 
    select(point, CircleRadius, everything())
  
  #------
  # now check if the derived_variables file already exists, 
  outpath <- "generated_data/derived_variables/derived_variables_basic_summs.csv"
  if(!file.exists(outpath))
    write.csv(basic.summs, outpath, row.names = F) # if not, create it
  
  basic.summed <- read.csv(outpath) # if so, read it then add the next line of data
  basic.summed.added <- rbind(basic.summed, basic.summs) %>% 
    unique() 
  write.csv(basic.summed.added, outpath, row.names = F)
  
}

map(zrads, basic_summer)     


zrad = 500
##########################################################################


# this version has the writing to disk as a separate step, which is done only once for each point. This seems to be much quicker than the 1st version.
basic_summer2 <- function(zrad){
  
  point.coords <- get_point_coords(zpoint, ".lcc")
  
  norm.las <- lasclipCircle(norm.las, point.coords$easting.lcc, point.coords$northing.lcc, zrad/0.3048006096012192)
  
  norm.las.data.with.ground <- las_bounder_classifier(norm.las)
  
  
  num.returns <- norm.las.data.with.ground %>% 
    summarise(NumReturns = n()) %>% 
    mutate(point = zpoint,
           CircleRadius = zrad) 
  
  num.veg.returns <- norm.las.data.with.ground %>% 
    filter(VegGround == "veg") %>% 
    summarise(NumVegReturns = n()) 
  
  veg.per.cov <- num.veg.returns %>% 
    summarise(VegPerCov =  NumVegReturns/num.returns$NumReturns)
  
  ground.per.cov <- norm.las.data.with.ground %>% 
    filter(VegGround == "ground") %>% 
    summarise(NumGroundReturns = n()) %>% 
    summarise(GroundPerCov = NumGroundReturns/num.returns$NumReturns)
  
  
  all.veg.summ <- norm.las.data.with.ground %>% 
    filter(VegGround == "veg") %>% 
    summarise(nVegReturns = n(),
              VegMaxHt = max(Z),
              VegMeanHt = mean(Z),
              VegMedHt = median(Z))
  
  shrub.summ <- norm.las.data.with.ground  %>% 
    filter(VegGround == "veg", layer == "shrub") %>%
    summarise(nShrubReturns = n(),
              ShrubMaxHt = max(Z),
              ShrubMeanHt = mean(Z),
              ShrubMedHt = median(Z),
              ShrubPerCov = nShrubReturns/num.veg.returns$NumVegReturns)
  
  tree.summ <- norm.las.data.with.ground  %>% 
    filter(VegGround == "veg", layer == "tree") %>%
    summarise(nTreeReturns = n(),
              TreeMaxHt = max(Z),
              TreeMeanHt = mean(Z),
              TreeMedHt = median(Z),
              TreePerCov = nTreeReturns/num.veg.returns$NumVegReturns)
  
  
  basic.summs <- cbind(num.returns, veg.per.cov, ground.per.cov, all.veg.summ, shrub.summ, tree.summ) %>% 
    select(point, CircleRadius, everything())
  
}


#######################################################


zpoint <- "1R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "2L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "3L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "4L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "5R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "6L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "7L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "8L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "9R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "10R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "11R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "12L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "13L"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "14R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "15R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "16R"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "HOME2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "HOME3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "MCDO10"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "MCDO6"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "WERA1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "WERA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "HOME5"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "MCDO2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "MCDO11"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "FORK10"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "MCDO12"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "LIIN2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "LIIN3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "MIRA11"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "MIRA12"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "MIRA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "BIHI2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "LIIN1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "LIIN4"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "WERA3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "WEBL1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "WEBL3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "BIHI1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "BIHI3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "BIHI5"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "BIHI6"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "MIRA10"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "BIHI4"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "EABL2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "EARA1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "BEAR1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "EARA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "EARA3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "HIVA1"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "HIVA3"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     

zpoint <- "HIVA2"
norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
foo <- map_dfr(zrads, basic_summer2)
derived_checker_writter("basic_summs", foo)     


##-----
# test to make sure we got everything
shoo <- read.csv("generated_data/derived_variables/derived_variables_basic_summs.csv")
table(shoo$point)
length(unique(shoo$point))


