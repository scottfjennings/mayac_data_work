


## Code for an analysis of the relationships between birds and habitat features along Pine Flat Rd, on ACR's Modini-Mayacamas Preserve, and across Sonoma County.
## this analysis will take advantage of 3 datasets:
# 1. Point count bird surveys conducted at 16 points along Pine Flat Rd, which roughly follow the BBS protocol. Data spans 2013-present
# 2. Similar bird surveys conducted at 36 points on the Modini-Mayacamas Preserve. data span 2010-present
# 3. A Sonoma County-wide vegetation map. http://sonomavegmap.org/

##---
# Project assumptions
# 1. there is relatively little change in vegetation from year to year


##---
library(plyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(hms)
library(chron)
#

######################################
## Pine Flat Rd. bird data
######################################

pf.birds = read_csv("PineFlatRd/Pine_Flat_Bird_Data.csv") %>%
  select(Project, Point, Date, Start_Time = "Start Time", Distance_Bin = "Distance Bin ID", Count, Spp, Researcher) %>% 
  mutate(Date = mdy(Date),
         Point = as.character(Point),
         Obs_last = str_split(Researcher, ",", simplify = TRUE) %>% 
           .[,1]) %>% 
  spread(Distance_Bin, Count) %>% 
  select(Project, Point, Date, Start_Time, spp = Spp, Researcher = Obs_last, G50, L50)%>% 
  filter(spp != "---")




######################################
## MEIP bird data
######################################
## exported from MEIPBBA.accdb with the 'qsel_pc_with_sp_and_visitdata' query

miep.birds = read_csv("MIEP/meip_birds_20171019.txt", col_names = c("Year",	"Month",	"Day",	"Start_Time",	"Station_Name",	"Obs_Last",	"Obs_First",	"Rec_Last",	"Rec_First",	"Spp",	"Common_Name", "L50_0_5",	"G50_0_5",	"FO_0_5",	"L50_5_8",	"G50_5_8",	"FO_5_8")) %>% 
  mutate(Date = ymd(paste(Year, Month, Day)),
         Start_Time = mdy_hms(Start_Time),
         Start_Time = as.hms(Start_Time),
         #Start_Time2 = times(paste(hour(Start_Time), minutes(Start_Time), second(Start_Time), sep = ":")),
         #Researcher = paste(Obs_Last, Obs_First, sep = ", "),
         Project = "MIEP") %>% 
  select(Project, Point = Station_Name, Date, Start_Time, spp = Spp, L50 = L50_0_5,	G50 = G50_0_5, FO_0_5, L50_5_8,	G50_5_8, Researcher = Obs_Last)


##--
mayac.birds <- bind_rows(miep.birds, pf.birds)
mayac.birds[is.na(mayac.birds)] = 0
names(mayac.birds) <- tolower(names(mayac.birds))
mayac.birds$researcher <- gsub("Bluestein", "Blustein", mayac.birds$researcher)

mayac.birds <- mayac.birds %>% 
  mutate(start_time = times(start_time),
         total = l50 + g50)



#adds AOU species numbers, from https://www.pwrc.usgs.gov/BBl/manual/speclist.cfm		
#requires AOU_list.csv to be stored somewhere, and for the pathway to be correct
add.spp.num=function(dataframe){							  
  #first add the AOU list and merge with the data
  aou=read.csv("C:/Users/scott.jennings/Documents/Projects/birds/full_list.csv")
  names(dataframe)<- tolower(names(dataframe))
  names(aou)<- tolower(names(aou))
  #aou.a=subset(aou, select=c("species.number", "alpha.code", "common.name"))
  a=merge(dataframe, aou, by.x="spp", by.y="alpha.code", all.x=T)
  return(a)
}

mayac.birds = add.spp.num(mayac.birds) %>% 
  select(spp, project, point, date, start_time, l50, g50, total, researcher, species.number, common.name, scientific.name)



#this function filters out waterbirds, raptors, pigeons, doves, ravens, crows, and swallows
#does this based on species AOU numbers, from https://www.pwrc.usgs.gov/BBl/manual/speclist.cfm
filt.spp=function(dataframe){
  a=subset(dataframe, (species.number>=3920 & species.number<=4840)|	#woodpeckers thru jays, excludes crows and ravens
             (species.number>=4910 & species.number<=6100)|	#clarks'Nutcracker up to Tanagers, 
             #6100-6180 excludes swallows, martins
             (species.number>=6180 & species.number<8500))  	#waxwings to the start of seabirds
  return(a)
}

mayac.birds = filt.spp(mayac.birds)

#Which points were surveyed in which year
mayac_points <- mayac.birds %>% 
  mutate(year = year(date)) %>% 
  select(point, project, year) %>% 
  distinct()

# the dates each point was surveyed on
mayac_point_surveys <- mayac.birds %>% 
  select(point, date, researcher, start_time) %>% 
  distinct() %>% 
  arrange(point, date)

# the dates of all surveys
mayac_proj_surveys <- mayac.birds %>% 
  select(project, date) %>% 
  distinct()

# expanded table with zeros for non-detections, within 50 m only
mayac.birds.l50exp <- mayac.birds %>% 
  filter(l50 > 0)%>% 
  select(spp, point, date, l50) %>% 
  complete(date, point, spp) %>% 
  inner_join(mayac_point_surveys) %>% 
  mutate(l50 = ifelse(is.na(l50), 0, l50))

# expanded table with zeros for non-detections, all distances
mayac.birds.totexp <- mayac.birds %>% 
  filter(total > 0)%>% 
  select(spp, point, date, total) %>% 
  complete(date, point, spp) %>% 
  inner_join(mayac_point_surveys) %>% 
  mutate(total = ifelse(is.na(total), 0, total))


# checking which spp have enough data
# comparing mean vs se


make_l50_summary <- function(){
comp.l50.expand <- mayac.birds.l50exp  %>% 
  group_by(spp) %>% 
  summarise(l50.mean = mean(l50),
            l50.se = sd(l50)/sqrt(nrow(.)),
            se.percent.of.mean = round(100*(l50.se/l50.mean), 2))
## how many points was a sp detected at across all years
birds.by.point <- mayac.birds.l50exp%>% 
  filter(l50 > 0) %>% 
  select(spp, point) %>% 
  distinct() %>%  
  group_by(spp) %>% 
  summarise(n.points.allyrs = n())
## how many years was a sp detected in
num.yrs.l50 <- mayac.birds.l50exp %>% 
  filter(l50 > 0) %>% 
  mutate(year = year(date)) %>% 
  select(year, spp) %>% 
  group_by(spp) %>% 
  distinct()%>%
  summarise(n.years = length(year)) 
l50.summ <- join_all(list(comp.l50.expand, birds.by.point, num.yrs.l50), type='full') %>% 
  arrange(-n.years)
l50.summ[is.na(l50.summ)] <- 0
return(l50.summ)
}

l50_summary <- make_l50_summary()


write_csv(l50_summary, "l50_summary.csv")



make_l400_summary <- function(){
  comp.l400.expand <- mayac.birds.totexp  %>% 
    group_by(spp) %>% 
    summarise(l400.mean = mean(total),
              l400.se = sd(total)/sqrt(nrow(.)),
              se.percent.of.mean = round(100*(l400.se/l400.mean), 2)) %>% 
    mutate(l400.mean = round(l400.mean, 2),
           l400.se = round(l400.se, 2))
  ## how many points was a sp detected at across all years
  birds.by.point <- mayac.birds.totexp%>% 
    filter(total > 0) %>% 
    select(spp, point) %>% 
    distinct() %>%  
    group_by(spp) %>% 
    summarise(n.points.allyrs = n())
  ## how many years was a sp detected in
  num.yrs <- mayac.birds.totexp %>% 
    filter(total > 0) %>% 
    mutate(year = year(date)) %>% 
    select(year, spp) %>% 
    group_by(spp) %>% 
    distinct()%>%
    summarise(n.years = length(year)) 
  summ <- join_all(list(comp.l400.expand, birds.by.point, num.yrs), type='full') %>% 
    arrange(-n.years)
  summ[is.na(summ)] <- 0
  return(summ)
}

l400_summary <- make_l400_summary()

write_csv(l400_summary, "l400_summary.csv")


## preliminary data exploration

#create species list
bird.spp.list <- pf.birds.full %>% 
  filter(year(date)<2017, month(date)==4 | month(date)==5) %>% 
  arrange(species.number) %>% 
  select(common.name.x, scientific.name.x, spp, family) %>% 
  distinct()

#write.csv(bird.spp.list, "pineflatspplist.csv", row.names = F)

#create list of unique survey dates
date.counter.list <- pf.birds.full %>% 
  filter(year(date)<2017, month(date)==4 | month(date)==5) %>% 
  select(date, researcher) %>% 
  distinct()


## make table of species occurance at each point
spp.point.tab <- pf.birds.full %>% 
  group_by(spp, point) %>% 
  summarise() %>% 
  mutate(present = 1) %>% 
  spread(key="point", value="present")

spp.point.tab <- add.spp.num(spp.point.tab) 
spp.point.tab <- spp.point.tab  %>% 
  select(common.name, species.number,  everything(), -spp)%>% 
  arrange(species.number)
spp.point.tab[is.na(spp.point.tab)] = 0

spp.point.long <- spp.point.tab %>% 
  select(common.name, family, species.number, everything(), -x, -scientific.name, -order, -subfamily, -genus, -species) %>% 
  gather(key="point", value="detected", -common.name, -species.number, -family) %>% 
  arrange(species.number)
spp.point.long$detected=as.logical(spp.point.long$detected)


spp.point.plotter <- function(data, main){
ggplot(data=data, aes(x=reorder(point, as.numeric(point)), y=reorder(common.name, -species.number), fill=factor(detected)))+
             geom_tile()+
  scale_fill_manual(values=c("FALSE"="red", "TRUE"="blue"))+
  ylab("")+
  xlab("Point")+ 
  theme(legend.position="none")+
  ggtitle(main) +
  theme(plot.title = element_text(hjust = 0.5))
}

woodpecker <- spp.point.plotter(filter(spp.point.long, family %in% "Picidae"), "Woodpeckers")
flycatcher <- spp.point.plotter(filter(spp.point.long, family %in% "Tyrannidae"), "Flycatchers")
star.black <- spp.point.plotter(filter(spp.point.long, family %in% c( "Sturnidae", "Icteridae")), "Starling and Blackbirds")
finch <- spp.point.plotter(filter(spp.point.long, family %in% c("Fringillidae", "Cardinalidae")), "Finches and friends")
sparrow <- spp.point.plotter(filter(spp.point.long, family %in% "Passerellidae"), "Sparrows")
corv.vir <- spp.point.plotter(filter(spp.point.long, family %in% c("Corvidae", "Vireonidae")), "Corvids and Vireos")
warbler <- spp.point.plotter(filter(spp.point.long, family %in% "Parulidae"), "Warblers")
mim.wren <- spp.point.plotter(filter(spp.point.long, family %in% c("Mimidae", "Troglodytidae")), "Mimids and Wrens")
chic.n.friends <- spp.point.plotter(filter(spp.point.long, family %in% c("Certhiidae", "Sittidae", "Paridae")), "Chickadees and friends")
thrush.n.friends <- spp.point.plotter(filter(spp.point.long, family %in% c("Sylviidae", "Aegithalidae", "Polioptilidae", "Turdidae")), "Thrushes and friends")


library(cowplot)
##, rel_heights = c(5/11, 7/11, 7/11, 7/11, 11/11)
plot_grid(woodpecker, flycatcher, star.black, finch,  nrow = 4, align = "v", rel_heights = c(7/8, 1, 1, 1))

plot_grid(sparrow, corv.vir, warbler, nrow = 3, align = "v", rel_heights = c(1, 1/2, 1/2))

plot_grid(mim.wren, chic.n.friends, thrush.n.friends, nrow = 3, align = "v", rel_heights = c(1/3, 1/3, 1/2))


mainer <- plot_grid(a, b, c, nrow = 1)

spp.point.grid <- grid.arrange(woodpecker, flycatcher, star.black, finch, sparrow, corv.vir, warbler, mim.wren, chic.n.friends, thrush.n.friends)


## filter to just Apr and May
pf.birds <- pf.birds.full %>% 
  filter(year(date)<2017) %>% 
  select(date, point, distance.bin.id, spp, count, researcher) %>% 
  filter(month(date)==4 | month(date)==5) %>% 
  arrange(date, point) %>% 
  droplevels()

#preliminary data exploration cont.;








summs <- full_join(ppy, num.yrs)
summs2 <- full_join(summs, comp.l50)
summs3 <- full_join(summs2, comp.all) %>% 
  arrange(-n.years)

indivs.detected <- data.frame(table(mayac.birds$spp, mayac.birds$l50)) %>% 
  select(spp = Var1, count = Var2, num.occasions = Freq) %>% 
  spread(count, num.occasions)

summs4 <- full_join(summs3, indivs.detected)

  
#preliminary data exploration cont.; any effect of month, year, observer, point start time on count or spp richness?

data.summ <- pf.birds %>% 
  group_by(researcher, date) %>% 
  summarise(tot.vis.birds = sum(count)) 
  

test.lm <- lm(tot.vis.birds~Researcher+as.factor(year(Date))+as.factor(month(Date)), data = data.summ)
resercher.test <- lm(tot.vis.birds~Researcher, data = filter(data.summ, month(Date)==4 | month(Date)==5))

ggplot(data.summ, aes(x = as.factor(month(Date)), y = tot.vis.birds)) +
  geom_boxplot()+
  ylim(0, 400)

ggplot(data.summ, aes(x = as.factor(year(Date)), y = tot.vis.birds)) +
  geom_point(aes(color=as.factor(month(Date))))+
  ylim(0, 400)

data.frame(group_by(data.summ, year(Date), month(Date), Researcher) %>% summarize(num.surveys = n()))


table(data.summ$Researcher, month(data.summ$Date))
table(pf.birds$Count, month(pf.birds$Date)) ## most surveys occured in Apr and May
spp.per.year = data.frame(table(pf.birds$sp, year(pf.birds$date))) %>% 
  rename(spp=Var1, year=Var2, count=Freq)


pf.birds.summ <- pf.birds %>% 
          group_by(date, point, distance.bin.id) %>% 
          summarise(abundance = sum(count),
                    richness = n())

##########################################

# basic trend for each spp


  comp.l50.trends <- mayac.birds.l50exp  %>% 
    filter(spp == "SPTO" |  spp == "OCWA" |  spp == "ACWO" |  spp == "HOWR" |  spp == "CALT" |  spp == "ANHU" |  spp == "WREN" |  spp == "PSFL" |  spp == "DEJU" |  spp == "STJA" |  spp == "BEWR" |  spp == "OATI") %>% 
  mutate(year = year(date)) %>% 
  group_by(spp, year) %>% 
  summarise(mean.spp.year = mean(l50))


ggplot(aes(x = year, y = mean.spp.year), data = comp.l50.trends) +
  geom_point() +
  geom_line() +
  facet_wrap(~spp, nrow = 3) +
  ylab("Average number of birds detected")


