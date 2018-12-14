
library(tidyverse)
library(lubridate)
library(stringr)
library(gridExtra)
#



######################################
## habitat data
######################################
## read in habitat data
pf.hab= read.csv("PineFlatRd/Pine_Flat_Veg_Data.csv")

# generate percent covers from point intercept data
field_per_covs <- pf.hab %>% 
  gather(key = "variable", value="value", -station) %>% 
  filter(grepl("PtInt", variable)) %>% 
  mutate(value = as.numeric(value),
         variable = substring(variable, 2)) %>% 
  group_by(station, variable) %>%
  summarise(point.mean = 100*(mean(value/10))) %>% 
  spread(key="variable", value="point.mean") %>% 
  select(point = station, Ground_PerCov = PtInt.bare, Tree_PerCov = PtInt.canopy, Shrub_PerCov = PtInt.shrub) %>% 
  mutate(ztype = "field") %>% 
  ungroup()

field_hts <- pf.hab %>%
  select(point = station, Tree_MaxHt = max.tree.height.m, Tree_MedHt = med.tree.height.m, Shrub_MaxHt = max.shrub.height.m, Shrub_MedHt = med.shrub.height.m)%>% 
  mutate(ztype = "field") %>% 
  ungroup()

field_veg <- full_join(field_per_covs, field_hts)

lidar<- read.csv("generated_data/derived_variables/derived_variables_basic_summs.csv") %>% 
  arrange(point) %>% 
  full_join(mayac.points)
  
lidar_pfr <- lidar %>%
  filter(CircleRadius == 50, Study == "PFR") %>% 
  select(point, Ground_PerCov = GroundPerCov, Shrub_PerCov = ShrubPerCov, Shrub_MaxHt = ShrubMaxHt, Shrub_MedHt = ShrubMedHt, Tree_PerCov = TreePerCov, Tree_MaxHt = TreeMaxHt, Tree_MedHt = TreeMedHt, -Study) %>% 
  mutate(type = "lidar",
         point = str_sub(point, 1, -2)) %>% 
  arrange(point)

lidar_pfr_long <- lidar_pfr %>% 
  gather(varb, val, -point, -type)%>% 
  separate(varb, into = c("veg_layer", "variable"), "_", extra = "merge") %>% 
  rename(lidar_value = val) %>% 
  select(-type) %>% 
  mutate(lidar_value = as.numeric(lidar_value),
         lidar_value = ifelse(variable == "PerCov", lidar_value * 100, lidar_value),
         lidar_value = round(lidar_value, 1))


#--------------------------
field_veg_long <- field_veg %>% 
  select(-ztype) %>% 
  gather(varb, val, -point) %>% 
  separate(varb, into = c("veg_layer", "variable"), "_", extra = "merge") %>% 
  rename(field_value = val) %>% 
  mutate(point = as.character(point))


field_lidar_long <- left_join(lidar_pfr_long, field_veg_long) %>% 
  arrange(point, veg_layer, variable) 



modeler <- function(zveg_layer, zvariable){
  
  foo <- field_lidar_long %>% 
    filter(veg_layer == zveg_layer, variable == zvariable)
  
  mod <- lm(lidar_value ~ field_value, data = foo)
  mod_tab <- data.frame(adj_r_sq = round(summary(mod)$adj.r.squared, 3),
                        slope = round(summary(mod)$coefficients[2, 1], 3),
                        veg_layer = zveg_layer, 
                        variable = zvariable)
  return(mod_tab)
  
}


mods_results <- rbind(
  modeler("Ground", "PerCov"),
  modeler("Shrub", "MaxHt"),
  modeler("Shrub", "MedHt"),
  modeler("Shrub", "PerCov"),
  modeler("Tree", "MaxHt"),
  modeler("Tree", "MedHt"),
  modeler("Tree", "PerCov"))



##---
a = ggplot(data = filter(field_lidar_long, veg_layer == "Tree", variable == "MedHt"), aes(x = field_value, y = lidar_value)) +
  ggtitle("Median tree height") +
  ylab("") +
  xlab("") +
  geom_point() +
  lims(x = c(0, 20), y = c(0, 20)) +
  geom_abline(slope = 1, intercept = 0)  + 
  #annotate("text", x = -Inf, y = Inf, label = "A", hjust = -2, vjust = 2, parse = TRUE) +
  theme_bw()

b = ggplot(data = filter(field_lidar_long, veg_layer == "Tree", variable == "MaxHt"), aes(x = field_value, y = lidar_value)) +
  ggtitle("Maximum tree height") +
  ylab("") +
  xlab("") +
  geom_point() +
  #lims(x = c(0, 20), y = c(0, 20)) +
  geom_abline(slope = 1, intercept = 0)  + 
  #annotate("text", x = -Inf, y = Inf, label = "B", hjust = -2, vjust = 2, parse = TRUE) +
  theme_bw()

c = ggplot(data = filter(field_lidar_long, veg_layer == "Tree", variable == "PerCov"), aes(x = field_value, y = lidar_value)) +
  ggtitle("Percent tree cover") +
  ylab("") +
  xlab("") +
  geom_point() +
  lims(x = c(0, 100), y = c(0, 100)) +
  geom_abline(slope = 1, intercept = 0)  + 
  #annotate("text", x = -Inf, y = Inf, label = "B", hjust = -2, vjust = 2, parse = TRUE) +
  theme_bw()

d = ggplot(data = filter(field_lidar_long, veg_layer == "Shrub", variable == "PerCov"), aes(x = field_value, y = lidar_value)) +
  ggtitle("Percent shrub cover") +
  ylab("") +
  xlab("") +
  geom_point() +
  lims(x = c(0, 100), y = c(0, 100)) +
  geom_abline(slope = 1, intercept = 0)  + 
  #annotate("text", x = -Inf, y = Inf, label = "B", hjust = -2, vjust = 2, parse = TRUE) +
  theme_bw()



fig2_square <- grid.arrange(a, b, c, d, ncol = 2, left = "LiDAR value", bottom = "field value")
ggsave("ardeid_fig2_square.tiff", fig2_square)

grid.arrange(a, b, c, d, ncol = 1, left = "LiDAR value", bottom = "field value")


grid.arrange(a, b, c, d, ncol = 4, left = "LiDAR value", bottom = "field value")


