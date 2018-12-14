

library(plyr)
library(tidyverse)
library(lidR)
library(purrr)
library(moments)
library(gridExtra)

options(scipen = 999)


############

# load the function, then go down below it for more settings and the call
# this one is a bit overcomplicated with the number of metrics calculated
zacfer <- function(zpoint) {
  
  # input norm.las is a normalized LAs object saved at generated_data/norm_laz
  
  
  norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))  
  
  norm.las.data.veg <- norm.las@data %>% 
    select(Z) %>% 
    mutate(Z = Z * 0.3048006096012192,  # convert feet to meters
           Z = ifelse(Z < 0, 0, Z)) %>%  # change negatives to 0 - this may end up being the wrong thing to do, but most of these are just barely negative
    filter(Z < upper.bound, Z > lower.bound) 
  
  zacf <- acf(norm.las.data.veg$Z)
  
  
  return(zacf) 
}

#################################################################################################
# I'm still not entirely sure what to do about the extreme values
# assigning this since it will need to be used multiple times, and might change
# if you change either of these, you need to also go into point.deriver_quant_hts() and change the code where the 3 lists in zsplit are being named
lower.bound = 0.5 # Lasek et al used 0.9m as the lower bound
upper.bound = 75 # and for now just removing anything taller than 50m
#################################################################################################


sk_1R <- zacfer("1R")
sk_2L <- zacfer("2L")
sk_3L <- zacfer("3L")
sk_4L <- zacfer("4L")
grid.arrange(sk_1R, sk_7L, sk_10R, sk_11R, sk_16R, ncol = 2)
ggsave(sk_a)
sk_5R <- zacfer("5R")
sk_6L <- zacfer("6L")
sk_7L <- zacfer("7L")
sk_8L <- zacfer("8L")
sk_9R <- zacfer("9R")
sk_10R <- zacfer("10R")
sk_11R <- zacfer("11R")
sk_12L <- zacfer("12L")
sk_13L <- zacfer("13L")
sk_14R <- zacfer("14R")
sk_15R <- zacfer("15R")
sk_16R <- zacfer("16R")
sk_HOME2 <- zacfer("HOME2")
sk_HOME3 <- zacfer("HOME3")
sk_MCDO10 <- zacfer("MCDO10")
sk_MCDO6 <- zacfer("MCDO6")
sk_WERA1 <- zacfer("WERA1")
sk_WERA2 <- zacfer("WERA2")
sk_HOME5 <- zacfer("HOME5")
sk_MCDO2 <- zacfer("MCDO2")
sk_MCDO11 <- zacfer("MCDO11")
sk_FORK10 <- zacfer("FORK10")
sk_MCDO12 <- zacfer("MCDO12")
sk_LIIN2 <- zacfer("LIIN2")
sk_LIIN3 <- zacfer("LIIN3")
sk_MIRA11 <- zacfer("MIRA11")
sk_MIRA12 <- zacfer("MIRA12")
sk_MIRA2 <- zacfer("MIRA2")
sk_BIHI2 <- zacfer("BIHI2")
sk_LIIN1 <- zacfer("LIIN1")
sk_LIIN4 <- zacfer("LIIN4")
sk_WERA3 <- zacfer("WERA3")
sk_WEBL1 <- zacfer("WEBL1")
sk_WEBL3 <- zacfer("WEBL3")
sk_BIHI1 <- zacfer("BIHI1")
sk_BIHI3 <- zacfer("BIHI3")
sk_BIHI5 <- zacfer("BIHI5")
sk_BIHI6 <- zacfer("BIHI6")
sk_MIRA10 <- zacfer("MIRA10")
sk_BIHI4 <- zacfer("BIHI4")
sk_EABL2 <- zacfer("EABL2")
sk_EARA1 <- zacfer("EARA1")
sk_BEAR1 <- zacfer("BEAR1")
sk_EARA2 <- zacfer("EARA2")
sk_EARA3 <- zacfer("EARA3")
sk_HIVA1 <- zacfer("HIVA1")
sk_HIVA3 <- zacfer("HIVA3")
sk_HIVA2 <- zacfer("HIVA2")



goo <- grid.arrange(sk_1R, sk_2L, sk_3L, sk_4L, ncol = 2)
