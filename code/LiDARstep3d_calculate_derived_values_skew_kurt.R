

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
skew_kurt_plotter <- function(zpoint) {

  # input norm.las is a normalized LAs object saved at generated_data/norm_laz
  norm.las <- readLAS(paste("generated_data/norm_laz/norm_", zpoint, ".laz", sep = ""))
  zrad <- 500
  
  point.coords <- get_point_coords(zpoint, ".lcc")
  
  norm.las <- lasclipCircle(norm.las, point.coords$easting.lcc, point.coords$northing.lcc, zrad/0.3048006096012192)
  
  norm.las.data.with.ground <- las_bounder_classifier(norm.las)
  norm.las.data.veg <- norm.las.data.with.ground %>% 
    filter(VegGround == "veg")

skew <- skewness(norm.las.data.veg$Z)
kurt <- kurtosis(norm.las.data.veg$Z)
zlabel = paste("skewness = ", round(skew, 3), "; ", "kurtosis = ", round(kurt, 3), sep = "")

  # count the totals
 foo <-  
   ggplot(data = norm.las.data.veg, aes(x = Z)) +
    geom_density() +
    ggtitle(paste("Point: ", zpoint, "; ", zlabel, sep = "")) +
    xlab("height above ground (m)") +
   xlim(0, 15) +
   ylim(0, 0.5) +
   coord_flip()
 
 return(foo) 
}

#################################################################################################


sk_1R <- skew_kurt_plotter("1R")
sk_2L <- skew_kurt_plotter("2L")
sk_3L <- skew_kurt_plotter("3L")
sk_4L <- skew_kurt_plotter("4L")
grid.arrange(sk_1R, sk_7L, sk_10R, sk_11R, sk_16R, ncol = 2)
ggsave(sk_a)
sk_5R <- skew_kurt_plotter("5R")
sk_6L <- skew_kurt_plotter("6L")
sk_7L <- skew_kurt_plotter("7L")
sk_8L <- skew_kurt_plotter("8L")
sk_9R <- skew_kurt_plotter("9R")
sk_10R <- skew_kurt_plotter("10R")
sk_11R <- skew_kurt_plotter("11R")
sk_12L <- skew_kurt_plotter("12L")
sk_13L <- skew_kurt_plotter("13L")
sk_14R <- skew_kurt_plotter("14R")
sk_15R <- skew_kurt_plotter("15R")
sk_16R <- skew_kurt_plotter("16R")
sk_HOME2 <- skew_kurt_plotter("HOME2")
sk_HOME3 <- skew_kurt_plotter("HOME3")
sk_MCDO10 <- skew_kurt_plotter("MCDO10")
sk_MCDO6 <- skew_kurt_plotter("MCDO6")
sk_WERA1 <- skew_kurt_plotter("WERA1")
sk_WERA2 <- skew_kurt_plotter("WERA2")
sk_HOME5 <- skew_kurt_plotter("HOME5")
sk_MCDO2 <- skew_kurt_plotter("MCDO2")
sk_MCDO11 <- skew_kurt_plotter("MCDO11")
sk_FORK10 <- skew_kurt_plotter("FORK10")
sk_MCDO12 <- skew_kurt_plotter("MCDO12")
sk_LIIN2 <- skew_kurt_plotter("LIIN2")
sk_LIIN3 <- skew_kurt_plotter("LIIN3")
sk_MIRA11 <- skew_kurt_plotter("MIRA11")
sk_MIRA12 <- skew_kurt_plotter("MIRA12")
sk_MIRA2 <- skew_kurt_plotter("MIRA2")
sk_BIHI2 <- skew_kurt_plotter("BIHI2")
sk_LIIN1 <- skew_kurt_plotter("LIIN1")
sk_LIIN4 <- skew_kurt_plotter("LIIN4")
sk_WERA3 <- skew_kurt_plotter("WERA3")
sk_WEBL1 <- skew_kurt_plotter("WEBL1")
sk_WEBL3 <- skew_kurt_plotter("WEBL3")
sk_BIHI1 <- skew_kurt_plotter("BIHI1")
sk_BIHI3 <- skew_kurt_plotter("BIHI3")
sk_BIHI5 <- skew_kurt_plotter("BIHI5")
sk_BIHI6 <- skew_kurt_plotter("BIHI6")
sk_MIRA10 <- skew_kurt_plotter("MIRA10")
sk_BIHI4 <- skew_kurt_plotter("BIHI4")
sk_EABL2 <- skew_kurt_plotter("EABL2")
sk_EARA1 <- skew_kurt_plotter("EARA1")
sk_BEAR1 <- skew_kurt_plotter("BEAR1")
sk_EARA2 <- skew_kurt_plotter("EARA2")
sk_EARA3 <- skew_kurt_plotter("EARA3")
sk_HIVA1 <- skew_kurt_plotter("HIVA1")
sk_HIVA3 <- skew_kurt_plotter("HIVA3")
sk_HIVA2 <- skew_kurt_plotter("HIVA2")



goo <- grid.arrange(sk_1R, sk_2L, sk_3L, sk_4L, ncol = 2)
