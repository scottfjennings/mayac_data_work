
library(tidyverse)
library(stringr)
library(psych)
library(data.table)

pfr_birds <- read.csv("PineFlatRd/Pine_Flat_Bird_Data.csv")


pfr_birds_678_50 <- pfr_birds %>% 
  filter(Distance.Bin.ID == "L50", Point == 6 | Point == 7 | Point == 8) %>% 
  group_by(Point, Spp) %>% 
  summarise(meanSp = mean(Count))


pfr_birds_678_50_spp <- pfr_birds %>% 
  filter(Distance.Bin.ID == "L50", Point == 6 | Point == 7 | Point == 8) %>% 
  distinct(Spp)


#################################
# basic trends

pfr