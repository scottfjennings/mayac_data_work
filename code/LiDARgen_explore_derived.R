

# exploring the derived LiDAR variables

library(tidyverse)
library(stringr)
library(psych)
library(data.table)

derived <- read.csv("generated_data/derived_variables/derived_variables_basic_summs.csv")

##############

derived678_50 <- derived %>%
  filter(point == "6L"| point == "7L"| point == "8L", CircleRadius == 50) %>% 
  mutate(shrub_tree_dens_ratio = nShrubReturns/nTreeReturns)


##############


derived_horiz_div <- read.csv("generated_data/derived_variables/derived_variables_horiz_div.csv") %>% 
  rename(radius = zrad)

all_derived <- full_join(derived, derived_horiz_div)

##
# checking covariance of variables
all_derived_num <- all_derived %>% 
  select(-point, -radius, -hex_rad, -num.returns) %>% 
  rename(v1 = allveg_per_cov, v2 = ground_per_cov, v3 = forb_per_cov, v4 = forb_rel_dens, v5 = shrub_per_cov, v6 = shrub_rel_dens, v7 = tree_per_cov, v8 = tree_rel_dens, v9 = forb_max_layer_ht, v10 = forb_mean_layer_ht, v11 = forb_med_layer_ht, v12 = shrub_max_layer_ht, v13 = shrub_mean_layer_ht, v14 = shrub_med_layer_ht, v15 = tree_max_layer_ht, v16 = tree_mean_layer_ht, v17 = tree_med_layer_ht, v18 = div.ind, v19 = norm.div.ind, v20 = horiz_div_ind_all, v21 = norm_horiz_div_ind_all, v22 = horiz_div_ind_shrub, v23 = norm_horiz_div_ind_shrub, v24 = horiz_div_ind_tree, v25 = norm_horiz_div_ind_tree)


zcor <- corr.test(all_derived_num)$ci

setDT(zcor, keep.rownames = TRUE)[]

foof <- data.frame(str_split_fixed(zcor$rn, "-", 2))

derived_corrs <- cbind(foof, zcor)

foofer <- data.frame(znums = paste("v", seq(1:25), sep = ""),
                     znames = all_derived %>% 
                       select(-point, -radius, -hex_rad, -num.returns) %>% 
                       names())

derived_corrsX1 <- inner_join(foofer, derived_corrs, by = c("znums" = "X1")) %>% 
  rename(firstNum = znums, firstName = znames)
derived_corrs_final <- inner_join(foofer, derived_corrsX1, by = c("znums" = "X2")) %>% 
  rename(secondNum = znums, secondName = znames) %>% 
  mutate(abs_r = abs(r)) %>% 
  select(firstName, secondName, r, abs_r, rn, firstNum, secondNum) %>% 
  arrange(-abs_r)

write.csv(derived_corrs_final, "generated_data/derived_variables/derived_variables_correlations.csv", row.names = F)

# comparing to the dround-collected veg data (run through creating pf.long.PtInt.avs in PineFlatVeg_data_explore.R)
field_veg <- pf.long.PtInt.avs %>% 
  select(point = station, ground_per_cov = bare.ground.cov, tree_layer_per_cov = canopy.cov, forb_layer_per_cov = ground.veg.cov, shrub_layer_per_cov = shrub.cov) %>% 
  mutate(type = "field") %>% 
  ungroup()


derived50 <- filter(derived, radius == 50) %>% 
  select(point, ground_per_cov, forb_layer_per_cov, shrub_layer_per_cov, tree_layer_per_cov) %>% 
  mutate(type = "lidar") %>% 
  arrange(point)

derived50pfr <- derived50[1:16,]

derived50pfr <- derived50pfr %>% 
  mutate(point = str_sub(point, 1, -2),
         ground_per_cov = round(ground_per_cov * 100, 1), 
         forb_layer_per_cov = round(forb_layer_per_cov * 100, 1), 
         shrub_layer_per_cov = round(shrub_layer_per_cov *100, 1), 
         tree_layer_per_cov = round(tree_layer_per_cov * 100, 1))


both_compare <- rbind(field_veg, derived50pfr)

both_compare_long <- both_compare %>% 
  gather(varb, val, -point, -type)


ggplot(data = both_compare_long, aes(x = point)) +
  geom_col(aes(y = val, fill = type), position = "dodge") +
  facet_grid(~varb)



both_compare_wide <- both_compare_long %>% 
  mutate(type.varb = paste(type, varb, sep = "_")) %>% 
  select(-type, -varb) %>% 
  spread(type.varb, val)


ground_comp <- lm(field_ground_per_cov ~ lidar_ground_per_cov, data = both_compare_wide)

tree_comp <- lm(field_tree_layer_per_cov ~ lidar_tree_layer_per_cov, data = both_compare_wide)
treeR2_lab <- paste("adj. R2 =", round(summary(tree_comp)$adj.r.squared, 2), sep = " ")
tree_lidar_coeff <- paste("increase in % cover in LiDAR for every 1% increase in field-based:", round(summary(tree_comp)$coefficients[2, 1], 2), sep = " ")


shrub_comp <- lm(field_shrub_layer_per_cov ~ lidar_shrub_layer_per_cov, data = both_compare_wide)
forb_comp <- lm(field_forb_layer_per_cov ~ lidar_forb_layer_per_cov, data = both_compare_wide)

ggplot(data = both_compare_wide, aes(x = field_ground_per_cov, y = lidar_ground_per_cov)) +
  geom_point(aes(color = point))

ggplot(data = both_compare_wide, aes(x = field_tree_layer_per_cov, y = lidar_tree_layer_per_cov)) +
  geom_point(aes(color = point))+ 
  annotate("text", x = 50, y = 18, label = treeR2_lab)+ 
  annotate("text", x = 50, y = 10, label = tree_lidar_coeff) +
  geom_smooth(method = "lm")

ggplot(data = both_compare_wide, aes(x = field_shrub_layer_per_cov, y = lidar_shrub_layer_per_cov)) +
  geom_point(aes(color = point))

ggplot(data = both_compare_wide, aes(x = field_forb_layer_per_cov, y = lidar_forb_layer_per_cov)) +
  geom_point(aes(color = point))


