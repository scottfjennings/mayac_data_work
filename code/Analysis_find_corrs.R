
library(tidyverse)
library(purrr)
library(stringr)



# checking a species' global model for highly correlated variables 


hi_corr_checker <- function(spp_vars, hi.corr.lim){
var_cors <- read.csv("generated_data/derived_variables/derived_variables_correlations.csv")
var_cors_lim <- var_cors %>% 
  filter(abs_r >= hi.corr.lim)

spp_global <- paste(spp_vars, collapse=" + ")


seeker <- function(ind.num){
grep(var_cors_lim[ind.num,1], spp_global) & grep(var_cors_lim[ind.num,2], spp_global) 
}

ind.nums <- 1:nrow(var_cors_lim)

foo <- map(ind.nums, seeker)


foo2 <- foo %>% 
  tibble::enframe(value = "hi.corr") %>% 
  mutate(hi.corr = as.character(hi.corr))

foof <- cbind(var_cors_lim, foo2) %>% 
  filter(hi.corr == TRUE) %>% 
  select(firstName, secondName, r, abs_r)

return(foof)
}



# these are the variables hypothesized to be important for predicting spp abundance
# can make a named vector of the variables
spp_vars <- c("shrub_per_cov", "shrub_rel_dens", "shrub_med_layer_ht", "horiz_div_ind_shrub", "forb_per_cov", "forb_rel_dens", "forb_med_layer_ht")
# or input them right into the function
spto_corr <- hi_corr_checker(c("shrub_per_cov", "shrub_rel_dens", "shrub_med_layer_ht", "horiz_div_ind_shrub", "forb_per_cov", "forb_rel_dens", "forb_med_layer_ht"), 0.6)


spto_cors <- cor(spto_vars)



#----------------------------------------------

derived <- read.csv("generated_data/derived_variables/derived_variables_bottom_forb_0.01_shrub_0.5.csv")
derived_horiz_div <- read.csv("generated_data/derived_variables/derived_variables_horiz_div.csv") %>% 
  rename(radius = zrad)
derived_variables_quant_hts <- read.csv("generated_data/derived_variables/derived_variables_quant_hts.csv")


all_derived <- full_join(full_join(derived, derived_horiz_div), derived_variables_quant_hts)
all_derived$tree_shrub_axis <- all_derived$tree_rel_dens/all_derived$shrub_rel_dens

spp_vars <- c("shrub_rel_dens", 
              "shrub_per_cov",
              "tree_rel_dens",
              "tree_per_cov",
              "tree_shrub_axis")

spp_sub <- all_derived %>% 
  select(., one_of(spp_vars))
foo <- cor(spp_sub)


spp_sub_corr <- all_derived %>% 
  select(radius, one_of(spp_vars)) %>% 
  group_by(radius) %>% 
  cor()



library(scatterplot3d)
scatterplot3d(all_derived$forb_rel_dens, all_derived$shrub_rel_dens, all_derived$tree_rel_dens)

library(rgl)
X = all_derived$forb_rel_dens
Y = all_derived$shrub_rel_dens
Z = all_derived$tree_rel_dens
plot3d(all_derived$forb_rel_dens, all_derived$shrub_rel_dens, all_derived$tree_rel_dens, type="p", col="red", xlab="forb", ylab="shrub", zlab="tree", 
       size=5, lwd=15, box=F)
