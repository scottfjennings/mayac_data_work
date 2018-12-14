library(tidyverse)

library(data.table)

all500 <- read.csv("generated_data/derived_variables/derived_variables_500m_old.csv")

df <- all500 %>% 
  select(-point)

mins <- apply(df,2,min)
maxs <- apply(df,2,max)
zrange <- maxs - mins

noPline500 <- read.csv("generated_data/derived_variables/derived_variables_500m.csv")


noP_long <- noPline500 %>% 
  gather(point, val) %>%
  separate(point, c("zpoint", "zpline"), "_") %>% 
  mutate(zpline = ifelse(is.na(zpline), "pline", zpline)) %>% 
  rename(varb = point) 

noP_wider <- noP_long%>% 
  spread(zpline, val)

noP_wider <- noP_wider %>% 
  mutate(diff = pline - noPline) %>% 
  group_by(varb) %>% 
  summarise(mn.diff = mean(diff))

  

 zranger <- data.frame(zrange)
 zranger <- setDT(zranger, keep.rownames = TRUE)[]
 zranger <-  zranger %>% 
   rename(varb = rn)
 tester <- full_join(zranger, noP_wider) %>% 
   mutate(per.diff = mn.diff / zrange)
 