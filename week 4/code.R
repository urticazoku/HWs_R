dim(dat)
#par(mfrow = c(3,3))
for (i in 1:9){
  qqnorm(dat[,c(0,i)])
}
#par(mfrow=c(1,1))
qqnorm(dat[,c(0,4)])
qqline(dat[,c(0,4)])
qqnorm(dat[,c(0,9)])
qqline(dat[,c(0,9)])
hist(dat[,c(0,4)])
hist(dat[,c(0,9)])
###############################

h<-head(InsectSprays)
is<-InsectSprays
spray<-is$spray
data<-split(is,spray)

median(data$A$count)
median(data$B$count)
median(data$C$count)
median(data$D$count)
median(data$E$count)

boxplot(InsectSprays$count ~ InsectSprays$spray)

library(dplyr)
#install.packages("UsingR")
data(nym.2002, package="UsingR")
d<-nym.2002
d
dg<-split(d,d$gender)
hist(dg$Female$time)
hist(dg$Male$time)
median(dg$Female$time)
median(dg$Male$time)

mean(dg$Female$time)
mean(dg$Male$time)

mypar(1,3)
males <- filter(nym.2002, gender=="Male") %>% select(time) %>% unlist
females <- filter(nym.2002, gender=="Female") %>% select(time) %>% unlist
boxplot(females, males)
hist(females,xlim=c(range( nym.2002$time)))
hist(males,xlim=c(range( nym.2002$time)))

##############

males <- filter(nym.2002, gender=="Male") 
females <- filter(nym.2002, gender=="Female") 

cor(males$age,males$time)
cor(females$age,females$time)
boxplot(0,cor(nym.2002$age,nym.2002$time))

############

time = sort(nym.2002$time)
max(time/median(time))
min(time/median(time))
