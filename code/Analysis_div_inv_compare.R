
# comparing Shannon diversity index between different toy datasets

library(tidyverse)

even <- data.frame(val = rep(10, 10))
mixed <- data.frame(val = seq(1, 20, by = 2))
lop <- data.frame(val = c(rep(1, 9), 91))
lop.even <- data.frame(val = c(rep(19, 5), rep(1, 5)))


div.inder <- function(foo){
  foo2 <- foo %>% 
    mutate(prop = val/sum(foo),
           prop.l.prop = prop * log(prop)) %>% 
    summarise(div.ind = -1 * (sum(prop.l.prop)))
  return(foo2)
}


even.div.ind <- div.inder(even) %>% 
  mutate(type = "even")

mixed.div.ind <- div.inder(mixed) %>% 
  mutate(type = "mixed")

lop.div.ind <- div.inder(lop) %>% 
  mutate(type = "lop")

lop.even.div.ind <- div.inder(lop.even) %>% 
  mutate(type = "lop.even")

goof <- rbind(even.div.ind, mixed.div.ind, lop.div.ind, lop.even.div.ind)


foo.even <- even %>% 
  mutate(spp = paste("sp", seq(1, 10), sep = "")) %>% 
  spread(spp, val)

foo.mixed <- mixed %>% 
  mutate(spp = paste("sp", seq(1, 10), sep = "")) %>% 
  spread(spp, val)

foo.lop <- lop %>% 
  mutate(spp = paste("sp", seq(1, 10), sep = "")) %>% 
  spread(spp, val)

foo.lop.even <- lop.even %>% 
  mutate(spp = paste("sp", seq(1, 10), sep = "")) %>% 
  spread(spp, val)

foof <- rbind(foo.even, foo.mixed, foo.lop, foo.lop.even)


goof.foof <- cbind(goof, foof) 
goof.foof$type <- with(goof.foof, reorder(type, type, function(x) (x)))

ggplot(data = goof.foof, aes(x = type, y = div.ind)) +
  geom_point()+ 
  scale_x_discrete(limits=c("even", "mixed", "lop.even", "lop"))
