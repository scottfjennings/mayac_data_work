library(ggplot2)

npoles <- 1

pole <- rep(1:npoles, each=2)
corner <- rep(c("a", "b"), npoles)
x <- c(0, 5)
y <- c(0, 3)




pole <- rep(1:npoles, each=2)
corner <- rep(c("a", "b"), npoles)
x <- c(520493.00, 520502.00, 520515.00, 520522.19)
y <- c(4280279.00, 4280281.0, 4280287.00, 4280288.42)

poles <- data.frame(pole, corner, x, y) %>% 
  mutate(pole = as.factor(pole))



ggplot(data = poles) +
  geom_point(aes(x = x, y = y, color = pole, shape = corner)) +
  xlim(-15, 15) +
  ylim(-15, 15)


poles_wide <- poles %>% 
  gather(coord, value, -pole, -corner) %>% 
  unite(temp, corner, coord) %>% 
  spread(temp, value)

rad2deg <- function(rad) {(rad * 180) / (pi)} 
deg2rad <- function(deg) {(deg * pi) / (180)}

# from https://stackoverflow.com/questions/32370485/r-convert-radians-to-degree-degree-to-radians
poles_wide <- poles_wide %>% 
  mutate(rise = b_y - a_y,
         run = b_x - a_x,
         c_x = b_x - run,
         c_y = b_y + rise,
         d_x = a_x - rise,
         d_y = a_y + run)
         
         
         
poles_relong <- poles_wide %>% 
  select(-rise, -run) %>% 
  gather(coord, value, -pole) %>% 
  separate(coord, c("corner", "coord"))


poles_rewide <- poles_relong %>% 
  spread(coord, value)
  
ggplot(data = poles_rewide) +
  geom_point(aes(x = x, y = y, color = pole, shape = corner)) +
  xlim(-15, 15) +
  ylim(-15, 15)

