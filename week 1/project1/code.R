dat<-read.csv("femaleMiceWeights.csv")
library(dplyr)

controls<-filter(dat, Diet=="chow")
controls<-select(controls, Bodyweight)
unlist(controls)

controls<-filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist

dat2<-read.csv("msleep_ggplot2.csv")
class(dat2)

t<-filter(dat2, order=="Primates")
s<-select(t, sleep_total)
class(s)
mean(unlist(s))
summarise(s,mean(s))
