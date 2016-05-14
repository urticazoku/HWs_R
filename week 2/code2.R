x<-unlist(read.csv("femaleControlsPopulation.csv"))
set.seed(1)
n<-1000
avgs1<-vector("numeric",n)
avgs2<-vector("numeric",n)
mice1<-5
mice2<-50
for(i in 1:n){
  avgs1[i]<-mean(sample(x,mice1))
  
  avgs2[i]<-mean(sample(x,mice2))
}
hist(avgs1)
hist(avgs2)
mean(avgs2<=25)-mean(avgs2<=23)

m<-23.9
sd<-0.43
lb<-23
ub<-25
hx<-mean(x,m,sd)
ans<-pnorm(ub,m,sd)-pnorm(lb,m,sd)

install.packages("downloader")
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename)
dat <- na.omit( dat )

x<-filter(dat, Diet=="chow" & Sex=="M") %>% select(Bodyweight) %>% unlist
mean(x)

#install.packages("rafalib")
#library(rafalib)
popsd(x)

set.seed(1)
X<-sample(x,25)
mean(X)

y<-filter(dat, Diet=="hf" & Sex=="M") %>% select(Bodyweight) %>% unlist
mean(y)
popsd(y)
set.seed(1)
Y<-sample(y,25)
mean(Y)
diff<-abs(mean(y)-mean(x))-abs(mean(X)-mean(Y))
############
x<-filter(dat, Diet=="chow" & Sex=="F") %>% select(Bodyweight) %>% unlist
mean(x)

#install.packages("rafalib")
#library(rafalib)
popsd(x)

set.seed(1)
X<-sample(x,25)
mean(X)

y<-filter(dat, Diet=="hf" & Sex=="F") %>% select(Bodyweight) %>% unlist
mean(y)
popsd(y)
set.seed(1)
Y<-sample(y,25)
mean(Y)
diff<-abs(mean(y)-mean(x))-abs(mean(X)-mean(Y))
###############
y<-filter(dat, Diet=="chow" & Sex=="M") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
mean( abs(z) <=3 )

qqnorm(z)
abline(0,1)

#############

mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)


y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs)
popsd(avgs)

#################################
x<-unlist(read.csv("femaleControlsPopulation.csv"))

n<-10000
nulls<-vector("numeric",n)
for(i in 1:n){
  control<-sample(x,3)
  treatment<sample(x,3)
  nulls[i]<-mean(treatment)-mean(control)
}

library(rafalib)
mypar()
qqnorm(nulls)
qqline(nulls)
#################################
library(dplyr)
dat<-read.csv("femaleMiceWeights.csv")
control<-filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist
X<-control
treatment<-filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
Y<-treatment

SE<-sqrt(((sd(treatment))^2)/12+((sd(control))^2)/12)
SE

ttest<-t.test(treatment,control)
ttest

Z <- ( mean(Y) - mean(X) ) / sqrt( var(X)/12 + var(Y)/12)
2*( 1-pnorm(Z)) 

qqnorm(control)
qqline(control)
qqnorm(treatment)
qqline(treatment)
#################################

set.seed(1)
#n <- 100
n<-100
sides <- 6
#p <- 1/sides
p<-0.01
zs <- replicate(10000,{
  x <- sample(1:sides,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}) 
qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
mean(abs(zs) > 2)
