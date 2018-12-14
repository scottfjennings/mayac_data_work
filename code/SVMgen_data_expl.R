


library(rgdal)
library(plotKML)
library(tidyverse)
library(sp)
library(leaflet)
library(raster)
library(rgeos)
library(sf)
library(rgbif)
library(stringr)
options(scipen = 99)


################################################
### MERGE THE 2 VEG OBJECTS

pfr_svm <- st_read("generated_data/PineFlatRd_plus_SonVegMap.shp")

miep_svm <- st_read("generated_data/MIEP_plus_SonVegMap.shp")


pfr_svm <- pfr_svm %>% 
  dplyr::select(everything(), -OID_COPY) %>% 
  mutate(point = paste("PFR", as.character(point), sep = ""))
miep_svm <- miep_svm %>% 
  dplyr::select(lon = xcoord, lat = ycoord, point, MAP_CLASS, LF_FOREST, proportion, everything())

mayac_svm <- rbind(miep_svm, pfr_svm)

### combine/sum proportions where there are more than 1 polygon of the same MAP_CLASS in a circle
mayac_svm_classlumped <- mayac_svm %>% 
  data.frame() %>% 
  group_by(point, MAP_CLASS, Abbrv, LF_FOREST) %>% 
  summarise(lumped.proportion = sum(proportion))

#---
mayac_svm_classlumped %>% 
  data.frame() %>% 
  dplyr::select(point, MAP_CLASS, LF_FOREST, lumped.proportion) %>% 
  mutate(lumped.proportion = round(lumped.proportion, 3))%>% 
  write.csv("generated_data/mayac_svm.csv", row.names = F)


###########################
## for 400m radius
pfr_svm400 <- st_read("generated_data/PineFlatRd400_plus_SonVegMap.shp")

miep_svm400 <- st_read("generated_data/MIEP400_plus_SonVegMap.shp")


pfr_svm400 <- pfr_svm400 %>% 
  dplyr::select(everything(), -OID_COPY) %>% 
  mutate(point = paste("PFR", as.character(point), sep = ""))
miep_svm400 <- miep_svm400 %>% 
  dplyr::select(lon = xcoord, lat = ycoord, point, MAP_CLASS, LF_FOREST, proportion, everything())

mayac_svm400 <- rbind(miep_svm400, pfr_svm400)

foof <- mayac_svm400 %>% 
  data.frame() %>% 
  distinct(MAP_CLASS, Abbrv)

### combine/sum proportions where there are more than 1 polygon of the same MAP_CLASS in a circle
mayac_svm400_classlumped <- mayac_svm400 %>% 
  data.frame() %>% 
  group_by(point, MAP_CLASS, Abbrv, LF_FOREST) %>% 
  summarise(lumped.proportion = sum(proportion))

#---
mayac_svm400_classlumped %>% 
  data.frame() %>% 
  dplyr::select(point, MAP_CLASS, LF_FOREST, lumped.proportion) %>% 
  mutate(lumped.proportion = round(lumped.proportion, 3))%>% 
  write.csv("generated_data/mayac_svm400.csv", row.names = F)



###################################
## data exploration

###################################
## comparing alliance percentages between the pine flat rd veg surveys and the SonVegMap

alliance_comp <- mayac_svm400_classlumped %>% 
  filter(grepl("PFR", point)) %>% 
  full_join(., pf_alliance_comp) %>% 
  mutate(lumped.proportion = round(100*lumped.proportion, 1)) %>% 
  dplyr::select(point, Abbrv, SVM.percov = lumped.proportion, PFR.percov = per.cov) %>% 
  filter(Abbrv != "Barren")

alliance_comp$SVM.percov = ifelse(is.na(alliance_comp$SVM.percov), 0, alliance_comp$SVM.percov)
alliance_comp$PFR.percov = ifelse(is.na(alliance_comp$PFR.percov), 0, alliance_comp$PFR.percov)

alliance_comp_long <- alliance_comp %>% 
  gather(data_type, per_cov, -point, -MAP_CLASS, -Abbrv) %>% 
  full_join(., perc.assessed)


alliance_comp_long$data_type <- ifelse(alliance_comp_long$data_type == "SVM.percov", "SonVegMap", "ACR_veg")

alliance_comp_long$perc.assesed <- (ifelse(alliance_comp_long$data_type == "SonVegMap", 100, alliance_comp_long$perc.assesed)/100)

alliance_comp_long$per_cov_cor <-   alliance_comp_long$per_cov * alliance_comp_long$perc.assesed
  
ggplot(alliance_comp_long, aes(x = data_type), group = point)+
  geom_col(aes(y = per_cov_cor, fill = Abbrv), position = position_stack(reverse = TRUE)) +
  facet_wrap(~point, ncol = 4)+
  theme(legend.text = element_text(size=8)) +
  theme(legend.title = element_blank()) +
  xlab("")+
  ylab("Percent cover")


perc.assessed <- pf.hab %>% 
  dplyr::select(point = station, perc.assesed = perc.of.400m.assesed) %>% 
  mutate(point = paste("PFR", point, sep = ""))







## looking at how representative vegetation in the 50 m radius circles is of the criteria for each MAP_CLASS 

native_forest <- mayac_svm %>%
  filter(LIFEFORM == "Native Forest") %>% 
  data.frame() %>% 
  dplyr::select(lon, lat, point, MAP_CLASS, LF_FOREST, proportion, TREE_HT_MN, TREE_HT_MX, TREE_HT_SD, ABS_COVER, HDW_COV_LO, HDW_COV_HI, CON_COV_LO, CON_COV_HI) %>% 
  group_by(MAP_CLASS) %>% 
  summarise(mean.TREE_HT_MN = mean(TREE_HT_MN),
            max.TREE_HT_MN = max(TREE_HT_MN),
            min.TREE_HT_MN = min(TREE_HT_MN),
            
            mean.TREE_HT_MX = mean(TREE_HT_MX),
            max.TREE_HT_MX = max(TREE_HT_MX),
            min.TREE_HT_MX = min(TREE_HT_MX),
            
            mean.TREE_HT_SD = mean(TREE_HT_SD), 
            max.TREE_HT_SD = max(TREE_HT_SD), 
            min.TREE_HT_SD = min(TREE_HT_SD), 
            
            mean.ABS_COVER = mean(ABS_COVER), 
            max.ABS_COVER = max(ABS_COVER), 
            min.ABS_COVER = min(ABS_COVER), 
            
            mean.HDW_COV_LO = mean(HDW_COV_LO), 
            max.HDW_COV_LO = max(HDW_COV_LO), 
            min.HDW_COV_LO = min(HDW_COV_LO), 
            
            mean.HDW_COV_HI = mean(HDW_COV_HI), 
            max.HDW_COV_HI = max(HDW_COV_HI), 
            min.HDW_COV_HI = min(HDW_COV_HI), 
            
            mean.CON_COV_LO = mean(CON_COV_LO), 
            max.CON_COV_LO = max(CON_COV_LO), 
            min.CON_COV_LO = min(CON_COV_LO), 
            
            mean.CON_COV_HI = mean(CON_COV_HI),
            max.CON_COV_HI = max(CON_COV_HI),
            min.CON_COV_HI = min(CON_COV_HI))




###################################

mayac_svm_exp <- mayac_svm_classlumped %>% 
  data.frame() %>% 
  dplyr::select(point, Abbrv, lumped.proportion) %>% 
  complete(point, Abbrv) %>% 
  mutate(lumped.proportion = ifelse(is.na(lumped.proportion), 0, lumped.proportion),
         lumped.proportion = round(lumped.proportion, 3),
         log.prop.5 = log(lumped.proportion + 0.5),
         log.prop1 = log(lumped.proportion + 1),
         asin.prop = asinTransform(lumped.proportion),
         sqrt.prop = sqrt(lumped.proportion))

mayac_svm_exp_long <- mayac_svm_exp %>% 
  gather(trans, value, -point, -Abbrv)

ggplot(mayac_svm_exp_long, aes(trans, value))+
  geom_boxplot() +
  facet_wrap(~Abbrv, ncol = 6)+
  theme(axis.text.x=element_text(angle=90,hjust=1)) 

ggplot(mayac_svm_exp_long, aes(trans, value))+
  geom_point() +
  geom_jitter() +
  facet_wrap(~Abbrv, ncol = 6)+
  theme(axis.text.x=element_text(angle=90,hjust=1)) 

ggplot(mayac_svm_exp_long, aes(trans, value))+
  geom_violin() +
  geom_point(size = 0) +
  #geom_jitter() +
  facet_wrap(~Abbrv, ncol = 6)+
  coord_flip()


mayac_svm_exp_summ <- mayac_svm_exp %>% 
  group_by(Abbrv) %>% 
  summarise(mean.prop = mean(lumped.proportion),
            min.prop = min(lumped.proportion),
            max.prop = max(lumped.proportion),
            
            mean.asin.prop = mean(asin.prop),
            min.asin.propp = min(asin.prop),
            max.asin.prop = max(asin.prop))



ggplot(mayac_svm_exp, aes(Abbrv, lumped.proportion))+
  geom_boxplot()

ggplot(mayac_svm_exp, aes(lumped.proportion))+
  #geom_point() +
  ylim(-0.3, 1) +
  coord_flip() +
  geom_boxplot() +
  geom_boxplot(aes(log.prop.1)) +
  geom_boxplot(aes(log.prop.0001)) +
  facet_wrap(~Abbrv, ncol = 6)

ggplot(mayac_svm_exp, aes(asin.prop))+
  geom_bar(width = .5) +
  facet_wrap(~Abbrv, ncol = 6)


foo <- mayac_svm %>% 
  data.frame() %>% 
  distinct(point, MAP_CLASS) %>% 
  arrange(MAP_CLASS)
write.csv(foo, "generated_data/mayac_mapclass_lfforest.csv")



## assign new values for REL_COV, which is a 5-level factor variable for the ratio of softwood/hardwood cover
30-70%S/30-70%H  <- 
70-90%S/10-30%H
90-100%S/0-10%H
10-30%S/70-90%H
0-10%S/90-100%H

rel.cov <- read_csv("level, soft.low, soft.high, hard.low, hard.high
  3, 30, 70, 30, 70 
  4, 70, 90, 10, 30
  5, 90, 100, 0, 10
  2, 10, 30, 70, 90
  1, 0, 10, 90, 100") 

rel.cov <- rel.cov %>% 
  mutate(low.high = soft.low/hard.high,
         high.low = soft.high/hard.low,
         mean.soft = ((soft.low + soft.high)/2),
         mean.hard = ((hard.low + hard.high)/2),
         means = mean.soft/mean.hard,
         ind = mean.soft/5)

mean(rel.cov$soft.low, rel.cov$soft.high)

# looking at proportion of 50m radius circle covered by different LF_FOREST and MAP_CLASS types
mapclass_types <- mayac_svm_classlumped %>% 
  data.frame() %>% 
  #filter(lumped.proportion >= 0.1)%>% 
  group_by(MAP_CLASS) %>% 
  summarise(max_prop = round(max(lumped.proportion), 3),
            mean_prop = round(mean(lumped.proportion), 3),
            min_prop = round(min(lumped.proportion), 3),
            npoints = n())


ggplot(mayac_svm, aes(point, proportion))+
  geom_col(aes(fill = MAP_CLASS), position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme(legend.text = element_text(size=8)) +
  theme(legend.title = element_blank()) +
  xlab("")+
  ylab("")


mayac_svm_prop0.1 <- mayac_svm %>% 
  filter(proportion >= 0.1)

mayac_svm_prop0.15_types <- data.frame(unique(mayac_svm_prop0.15$LF_FOREST))




mayac_svm_fulltable <- expand.grid(point = mayac_svm$point,
                                   LF_FOREST = mayac_svm$LF_FOREST) %>% 
  unique()



mayac_svm_fulltable_filled <- full_join(mayac_svm, mayac_svm_fulltable)


mayac_svm_fulltable_filled <- mayac_svm_fulltable_filled %>% 
  data.frame() %>% 
  dplyr::select(point, LF_FOREST, proportion) %>% 
  mutate(proportion = ifelse(is.na(proportion), 0, proportion))





mayac_svm_by_point <- mayac_svm %>% 
  data.frame() %>% 
  dplyr::select(point, MAP_CLASS, proportion) %>% 
  group_by(point, MAP_CLASS) %>% 
  summarize(proportion = sum(proportion))  


mayac_svm_by_point %>% 
  ungroup() %>% 
  #filter(proportion > 0.25) %>% 
  dplyr::select(MAP_CLASS) %>% 
  distinct() %>% 
  nrow()



mayac_svm_LFFORESTcont <- mayac_svm %>% 
  data.frame() %>% 
  dplyr::select(LF_FOREST, TREE_HT_MN:MN_STD, -ends_with("_AC")) %>% 
  gather(type, value, -LF_FOREST) %>% 
  filter(value>=0)  %>%
  distinct(LF_FOREST, type) %>% 
  mutate(value = "Y") %>% 
  spread(LF_FOREST, value)

mayac_svm_LFFORESTcont[is.na(mayac_svm_LFFORESTcont)] <- "N"
write_csv(mayac_svm_LFFORESTcont, "generated_data/mayac_svm_LFFORESTcont.csv")


mayac_svm_MAP_CLASScont <- mayac_svm %>% 
  data.frame() %>% 
  dplyr::select(MAP_CLASS, TREE_HT_MN:MN_STD, -ends_with("_AC")) %>% 
  gather(type, value, -MAP_CLASS) %>% 
  filter(value>=0)  %>%
  distinct(MAP_CLASS, type) %>% 
  mutate(value = "Y") %>% 
  spread(MAP_CLASS, value)


mayac_svm_MAP_CLASS_LFFOREST <- mayac_svm %>% 
  data.frame() %>% 
  dplyr::select(MAP_CLASS, LF_FOREST) %>% 
  distinct()


mayac_svm %>% 
  data.frame() %>% 
  dplyr::select(LF_FOREST) %>% 
  distinct()

############################################################

## looking at covariance of veg map variables


svm4spreading <- mayac_svm %>% 
  data.frame() %>% 
  dplyr::select(point, MAP_CLASS, proportion) %>% 
  group_by(point, MAP_CLASS) %>% 
  summarise(proportion = sum(proportion)) %>% 
  spread(MAP_CLASS, proportion)
svm4spreading[is.na(svm4spreading)] <- 0
mayac_mapclass_cor <- svm4spreading %>% 
  ungroup() %>% 
  dplyr::select(-point) %>% 
  cor() %>% 
  data.frame()
write_csv(mayac_mapclass_cor, "mayac_mapclass_cor.csv")

znames <- names(mayac_mapclass_cor)

mayac_mapclass_cor_long <- mayac_mapclass_cor %>% 
  mutate(MAP_CLASS = znames) %>% 
  dplyr::select(MAP_CLASS, everything()) %>% 
  gather(MAP_CLASS2, zcor, -MAP_CLASS) %>% 
  filter(!MAP_CLASS==MAP_CLASS2) %>% 
  arrange(-abs(zcor)) %>% 
  mutate(indx = 1:nrow(.)) %>% 
  filter(indx %% 2 == 1, abs(zcor) >= 0.2) %>% 
  dplyr::select(-indx)

mayac_mapclass_cor_long2 <- mayac_mapclass_cor_long[which(mayac_mapclass_cor_long$indx %% 2 != 0)]




zoof <- cor(mayac_svm$MAP_CLASS, mayac_svm$REL_COV)


#########################################################
## now the tabular form of some data on habitat alliances from vol 2 of the Veg Map methods

svm_tabular <- read.csv("generated_data/SonVegVol2_tabular.csv")


svm_tabular <- svm_tabular %>% 
  mutate(Mean = as.character(Mean),
         Mean = gsub("º", "", Mean),
         Mean = gsub("%", "", Mean),
         Cover.Range = as.character(Cover.Range),
         Cover.Range = gsub("º", "", Cover.Range),
         Cover.Range = gsub("%", "", Cover.Range)
         )



svm_tabular_wideMean <- svm_tabular %>% 
  dplyr::select(X, X.1, Mean, Cover.Range) %>% 
  #mutate(X.1 = paste(X.1, ".Mean", sep = "")) %>% 
  gather(val.type, value, -X, -X.1) %>% 
  mutate(type = paste(X.1, val.type, sep = ".")) %>% 
  dplyr::select(-X.1, -val.type) %>% 
  spread(type, value) %>% 
  dplyr::select(MAP_CLASS = X, 
                TotMean = "Total vegetation.Mean",  
                HardMean = Hardwood.Mean,
                ConMean = Conifer.Mean,  
                UnderstMean = "Regenerating/understory tree.Mean", 
                ShrubMean = Shrub.Mean, 
                HerbMean = Herb.Mean,  
                LittMean =  "Litter cover.Mean",
                groundMean = "Bare ground cover.Mean", 
                LRockMean = "Large rock cover.Mean",  
                SRockMean = "Small rock cover.Mean", 
                SlopeMean = Slope.Mean,
                
                TotRange = "Total vegetation.Cover.Range", 
                HardRange = Hardwood.Cover.Range,
                ConRange = Conifer.Cover.Range,  
                UnderstRange = "Regenerating/understory tree.Cover.Range", 
                ShrubRange = Shrub.Cover.Range,
                HerbRange = Herb.Cover.Range, 
                LittRange = "Litter cover.Cover.Range", 
                LRockRange =  "Large rock cover.Cover.Range", 
                SRockRange = "Small rock cover.Cover.Range",
                SlopeRange = Slope.Cover.Range)


svm_tabular_wideMean$MAP_CLASSnames <- c("Bigleaf maple forest",
                                         "Chamise chaparral",
                                         "Madrone forest",
                                         "Hoary, common, and Stanford manzanita chaparral",
                                         "Wedge leaf ceanothus chaparral, Buck brush chaparral",
                                         "Hairy leaf ceanothus chaparral",
                                         "Blue blossom chaparral",
                                         "California yerba santa – silver bush lupine scrub",
                                         "Baltic and Mexican rush marshes",
                                         "Knobcone pine forest",
                                         "Fremont cottonwood forest",
                                         "Douglas fir forest",
                                         "Mixed oak forest",
                                         "Coast live oak woodland",
                                         "Canyon live oak forest",
                                         "Leather oak chaparral",
                                         "Oregon white oak woodland",
                                         "Valley oak woodland",
                                         "Interior live oak chaparral",
                                         "Interior live oak woodland",
                                         "Himalayan blackberry brambles",
                                         "California bay forest")

svm_tabular_wideMean <- svm_tabular_wideMean %>% 
  full_join(., mayac_svm_mapclassabbrv) %>% 
  dplyr::select(Abbrv, MAP_CLASS, MAP_CLASSnames, everything()) 
  


mayac_svm_mapclassabbrv <- mayac_svm %>% 
  data.frame() %>% 
  distinct(MAP_CLASS, Abbrv, LF_FOREST) %>% 
  mutate(Abbrv = gsub(" ", "", Abbrv, fixed = TRUE),
         Abbrv = gsub(".", "", Abbrv, fixed = TRUE))



write.csv(svm_tabular_wideMean, "generated_data/svm_tabular_wide.csv", row.names = F)



separate(Location, c('Latitude', 'Longitude'), sep=",")


