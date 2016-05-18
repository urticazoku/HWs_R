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
###########

data(ChickWeight)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")
head(chick)
chick = na.omit(chick)

old_c<-chick[,5]
c<-old_c
c<-append(c,3000)
mean(c)/mean(old_c)
median(c)/median(old_c)
sd(c)/sd(old_c)
mad(c)/mad(old_c)
old_c21<-chick[,14]

plot(old_c,old_c21)

c21<-old_c21
c21<-append(c21,3000)
cor(c,c21)/cor(old_c,old_c21)

spl<-split(chick,chick$Diet)
x<-spl$`1`[,5]
y<-spl$`4`[,5]
t.test(x,y)
wilcox.test(x,y)
x<-append(x,200)
t.test(x,y)$p.value
wilcox.test(x,y)

library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)
t.test(x,y+10)$statistic-t.test(x,y+100)$statistic
wilcox.test(c(1,2,3),c(4,5,6))
wilcox.test(c(1,2,3),c(400,500,600))
