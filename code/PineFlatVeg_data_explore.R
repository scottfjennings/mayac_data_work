
library(tidyverse)
library(lubridate)
library(stringr)
#


######################################
## habitat data
######################################
## read in habitat data
pf.hab= read.csv("PineFlatRd/Pine_Flat_Veg_Data.csv")


pf.hab.use <- pf.hab %>% 
  select(station, max.tree.height.m, med.tree.height.m, max.shrub.height.m, med.shrub.height.m, max.ground_cover.height.m, med.ground_cover.height.m)


pf.long.PtInt.avs <- pf.hab %>% 
  gather(key = "variable", value="value", -station) %>% 
  filter(grepl("PtInt", variable)) %>% 
  mutate(value=as.numeric(value),
         variable=substring(variable, 2))%>% 
  group_by(station, variable) %>%
  summarise(point.mean = 100*(mean(value/10))) %>% 
  spread(key="variable", value="point.mean") %>% 
  select(station, tall.dead.wood=PtInt.0.5_3m.wood, short.dead.wood=PtInt.0_0.5m.wood, bare.ground.cov=PtInt.bare, canopy.cov=PtInt.canopy, duff.cov=PtInt.duff, ground.veg.cov=PtInt.ground, litter.cov=PtInt.litter, shrub.cov=PtInt.shrub)


## extract dominant species columns
dom.spp <- pf.hab %>% 
select(station, contains("dom"))




pf.long <- pf.hab %>% 
  select(-Comments, -X) %>% 
  gather(key = "variable", value="value", -station) %>% 
  filter(!grepl("ptqt", variable)) %>% 
  arrange(station)




pf.long.PtInt.avs <- pf.long %>% filter(grepl("PtInt", variable)) %>% 
  group_by(station, variable.2) %>%
  mutate(value=as.numeric(value)) %>% 
  summarise(point.mean = 100*(mean(value/10)))





pf.long <- within(pf.long, {
  variable<-as.character(variable)
  direction <- as.factor(ifelse(substr(variable, 1, 1)=="N", "N",
                         ifelse(substr(variable, 1, 1)=="E", "E",
                         ifelse(substr(variable, 1, 1)=="S", "S",
                         ifelse(substr(variable, 1, 1)=="W", "W","All")))))
  veg.type <- as.factor(ifelse(grepl("shrub", variable),"shrub",
                        ifelse(grepl("tree", variable),"tree",
                        ifelse(grepl("logs", variable),"logs",
                        ifelse(grepl("snags", variable),"snags",
                        ifelse(grepl("canopy", variable),"canopy",
                        ifelse(grepl("bare", variable),"bare",
                        ifelse(grepl("litter", variable),"litter",
                        ifelse(grepl("duff", variable),"duff",
                        ifelse(grepl("0_0.5m.wood", variable),"low.dead.wood",
                        ifelse(grepl("0.5_3m.wood", variable),"hi.dead.wood",
                        ifelse(grepl("sappling", variable),"sappling",
                        ifelse(grepl("ground", variable),"ground","")))))))))))))
  data.type <- as.factor(ifelse(grepl("distance", variable),"distance",
                         ifelse(grepl("height", variable),"height",
                         ifelse(grepl("dbh", variable),"dbh",
                         ifelse(grepl("cover", variable),"cover",
                         ifelse(grepl("PtInt", variable),"cover",
                         ifelse(grepl("species", variable),"species", "")))))))
  quart.inter <- as.factor(ifelse(grepl("PtInt", variable),"intercept",
                           ifelse(grepl("ptqt", variable),"quarter", "")))
  radius <- ifelse(grepl("alliance", variable) | grepl("400m", variable), 400, 50)
  variable.2 <- as.factor(ifelse(direction=="All", variable, substring(variable, 2)))
})





## select fields that have numeric data
nums <- sapply(pf.hab, is.numeric)
pf.hab.num <- pf.hab[ , nums]






#calculate corelations between numeric variables
pf.hab_cor <- as.matrix(cor(pf.hab.num))
pf.hab_cor_melt <- melt(pf.hab_cor)

## make data long
pf.num.long <- gather(pf.hab.num, key = "variable", value="value", -station)


## make some new fields to classify data types, etc
pf.num.long <- within(pf.num.long, {
  variable<-as.character(variable)
  direction <- as.factor(ifelse(substr(variable, 1, 1)=="N", "N",
                         ifelse(substr(variable, 1, 1)=="E", "E",
                         ifelse(substr(variable, 1, 1)=="S", "S",
                         ifelse(substr(variable, 1, 1)=="W", "W","All")))))
  veg.type <- as.factor(ifelse(grepl("shrub", variable),"shrub",
                        ifelse(grepl("tree", variable),"tree",
                        ifelse(grepl("logs", variable),"logs",
                        ifelse(grepl("snags", variable),"snags",
                        ifelse(grepl("canopy", variable),"canopy",
                        ifelse(grepl("bare", variable),"bare",
                        ifelse(grepl("litter", variable),"litter",
                        ifelse(grepl("duff", variable),"duff",
                        ifelse(grepl("0_0.5m.wood", variable),"low.dead.wood",
                        ifelse(grepl("0.5_3m.wood", variable),"hi.dead.wood",
                        ifelse(grepl("sappling", variable),"sappling",
                        ifelse(grepl("ground", variable),"ground","")))))))))))))
  data.type <- as.factor(ifelse(grepl("distance", variable),"distance",
                         ifelse(grepl("height", variable),"height",
                         ifelse(grepl("dbh", variable),"dbh",
                         ifelse(grepl("cover", variable),"cover",
                         ifelse(grepl("PtInt", variable),"cover",
                         ifelse(grepl("species", variable),"species", "")))))))
  quart.inter <- as.factor(ifelse(grepl("PtInt", variable),"intercept",
                           ifelse(grepl("ptqt", variable),"quarter", "")))
  radius <- ifelse(grepl("alliance", variable) | grepl("400m", variable), 400, 50)
  variable.2 <- as.factor(ifelse(direction=="All", variable, substring(variable, 2)))
})

## calculate mean values of habitat variables measured in the 4 directions
summarized.hab <- pf.num.long %>% 
  filter(quart.inter=="intercept") %>% 
  group_by(station, veg.type) %>% 
  summarise(per.cover = (mean(value)),
            sd.per.cover = (sd(value))) 



pf.num.long %>% 
  filter(data.type=="cover", direction=="All") %>% 
  arrange(station)


#### extract non-numeric fields and make long
pf.hab.notnum <- pf.hab[ , !nums]
pf.notnum.long <- pf.hab.notnum %>% 
  mutate(station = 1:16) %>% 
  select(station, everything(), -Comments, -X) %>% 
  gather(key = "variable", value="value", -station) 

pf.notnum.long <- pf.notnum.long %>% 
  filter(grepl("dom", variable)) %>% 
  select(station, variable, spp.code=value) %>% 
  mutate(veg.type = as.factor(ifelse(grepl("tree", variable), "tree",
                              ifelse(grepl("shrub", variable), "shrub",
                              ifelse(grepl("ground", variable), "ground", "")))),
         dom.order = str_sub(variable, 2, 2))



dom.covs <- pf.num.long %>% 
  filter(grepl("dom", variable)) %>% 
  select(station, variable, value, veg.type) %>% 
  mutate(dom.order = str_sub(variable, 2, 2))

pf.dom.covs <- full_join(pf.notnum.long, dom.covs)


#############################
## extract just alliance columns to compare to SVM

pf.hab_alliances <- pf.hab %>% 
  dplyr::select(point = station, contains("alliance")) %>% 
  gather(alliance, per.cov, -point) %>% 
  mutate(alliance = gsub("\\.", " ", alliance),
         alliance = gsub("z ", "", alliance),
         alliance = gsub("grass NNAN", "NNAN", alliance))
  
  
pf.alliances <- read.csv("PineFlatRd/PFR_alliances.csv") %>% 
  dplyr::select(Abbrv, alliance = PFR_abbrv, name)%>% 
  mutate(alliance = gsub("\\/", "_", alliance))
  
  
  
 pf_alliance_comp <- full_join(pf.hab_alliances, pf.alliances) %>% 
   mutate(point = paste("PFR", point, sep = "")) %>% 
   filter(per.cov > 0)
  
  
  