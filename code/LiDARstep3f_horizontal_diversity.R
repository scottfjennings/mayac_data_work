

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
  
  point.coords <- get.point.coords(zpoint, ".lcc")  
  
  
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
    filter( Z > lower.veg) 
  # count the totals
  
  #---  
  # all veg  
  hex.summary.all <- norm.las %>% 
    lasfilter(Z > lower.veg) %>% 
    grid_hexametrics(myMetrics(Z, Y), hex.rad)
  
  tot.num.ret.num.hex.all <- hex.summary.all %>% 
    summarize(tot.num.ret.all = sum(num.ret), # total number of returns
              tot.num.hex.all = length(num.ret)) # number of hex's; number of groups for diversity index
  
  div.ind.all <- hex.summary.all %>% 
    mutate(rel_dens = num.ret/tot.num.ret.num.hex.all$tot.num.ret.all,
           prop.l.prop = rel_dens * log(rel_dens)) %>% 
    summarise(horiz_div_ind_all = -1 * (sum(prop.l.prop))) %>% 
    mutate(norm_horiz_div_ind_all = horiz_div_ind_all/log(tot.num.ret.num.hex.all$tot.num.hex))
  
  tot.ret.div.ind.all <- cbind(tot.num.ret.num.hex.all, div.ind.all)
  #---  
  # shrub
  hex.summary.shrub <- norm.las %>% 
    lasfilter(Z > 0.5 & Z < 5) %>% 
    grid_hexametrics(myMetrics(Z), hex.rad)
  
  tot.num.ret.num.hex.shrub <- hex.summary.shrub %>% 
    summarize(tot.num.ret.shrub = sum(num.ret),
              tot.num.hex.shrub = length(num.ret))
  
  div.ind.shrub <- hex.summary.shrub %>% 
    mutate(rel_dens = num.ret/tot.num.ret.num.hex.shrub$tot.num.ret.shrub,
           prop.l.prop = rel_dens * log(rel_dens)) %>% 
    summarise(horiz_div_ind_shrub = -1 * (sum(prop.l.prop))) %>% 
    mutate(norm_horiz_div_ind_shrub = horiz_div_ind_shrub/log(tot.num.ret.num.hex.shrub$tot.num.hex.shrub))
  
  tot.ret.div.ind.shrub <- cbind(tot.num.ret.num.hex.shrub, div.ind.shrub)
  
  #---  
  # tree
  hex.summary.tree <- norm.las %>% 
    lasfilter(Z >= 5) %>% 
    grid_hexametrics(myMetrics(Z, Y), hex.rad)
  
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
    mutate(point = zpoint,
           zrad = zrad,
           hex.rad = hex.rad)
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
lower.veg = 0.5 # Lasek et al used 0.9m as the lower bound
upper.bound = 75 # and for now just removing anything taller than 50m
#################################################################################################


# if additional radii are needed, can redefine zrads and rerun this next ~200 lines
hex.rad = 4
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






