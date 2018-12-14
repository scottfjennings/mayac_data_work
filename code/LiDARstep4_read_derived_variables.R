


##
library(tidyverse)
library(psych)
library(data.table)
library(tidyr)

deriv.varbs.500 <- read.csv("generated_data/derived_variables/derived_variables_500m.csv")
deriv.varbs.400 <- read.csv("generated_data/derived_variables/derived_variables_400m.csv")
deriv.varbs.300 <- read.csv("generated_data/derived_variables/derived_variables_300m.csv")
deriv.varbs.200 <- read.csv("generated_data/derived_variables/derived_variables_200m.csv")
deriv.varbs.100 <- read.csv("generated_data/derived_variables/derived_variables_100m.csv")
deriv.varbs.50 <- read.csv("generated_data/derived_variables/derived_variables_50m.csv")



foo <- deriv.varbs.500 %>% 
  dplyr::select(-point) 

foo.too <- corr.test(foo)$ci %>% 
  dplyr::select(r)

foo.too <- setDT(foo.too, keep.rownames = TRUE)[]

foo.too <- foo.too %>% 
  dplyr::arrange(-abs(r))

foo.too.50 <- foo.too %>% 
  dplyr::filter(abs(r) >.5)


foo.too.50 <- foo.too.50 %>% 
  separate(rn, c("var1", "var2"), "-")








