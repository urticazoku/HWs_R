library(dplyr)
dat<-read.csv("femaleMiceWeights.csv")

control<-filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment<-filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
obs<-mean(treatment)-mean(control)
mean(treatment)
mean(control)
population<-read.csv("femaleControlsPopulation.csv")
population<-unlist(population)
mean(sample(population, 12))
avg<-mean(population)
set.seed(1)
avg-mean(sample(population,5))
set.seed(5)
avg-mean(sample(population,5))
n<-10000
nulls<-vector("numeric",n)
for(i in 1:n){ control<-sample(population,12); treatment<-sample(population,12); nulls[i]<-mean(treatment)-mean(control)}
hist(nulls)

set.seed(1)
n<-10000
avg5<-vector("numeric",n)
for(i in 1:n){
  x<-sample(population,5)
  avg5[i]<-mean(x)
}
hist(avg5)
mean(abs(avg5-mean(population))>1)

set.seed(1)
n<-1000
avg5<-vector("numeric",n)
for(i in 1:n){
  x<-sample(population,50)
  avg5[i]<-mean(x)
}
hist(avg5)
mean(abs(avg5-mean(population))>1)

install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)

x<-gapminder %>% filter(year==1952)
x<-x$lifeExp
a<-mean(x<=40)
b<-mean(x<=60)
b-a
