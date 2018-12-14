
# make a simple vector of all the bird count points for possible use in looping through various data management tasks for each point

point.names <- c("1R", "2L", "3L", "4L", "5R", "6L", "7L", "8L", "9R", "10R", "11R", "12L", "13L", "14R", "15R", "16R", "HOME2", "HOME3", "MCDO10", "MCDO6", "WERA1", "WERA2", "HOME5", "MCDO2", "MCDO11", "FORK10", "MCDO12", "LIIN2", "LIIN3", "MIRA11", "MIRA12", "MIRA2", "BIHI2", "LIIN1", "LIIN4", "WERA3", "WEBL1", "WEBL3", "BIHI1", "BIHI3", "BIHI5", "BIHI6", "MIRA10", "BIHI4", "EABL2", "EARA1", "BEAR1", "EARA2", "EARA3", "HIVA1", "HIVA3", "HIVA2")

zstudy <- c(rep("PFR", 16), rep("MMP", 36))

mayac.points <- data.frame(point = point.names,
                           Study = zstudy)
