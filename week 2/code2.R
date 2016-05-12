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