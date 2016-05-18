install.packages("UsingR")
library(UsingR)
data("father.son",package="UsingR")
mean(father.son$sheight)
############################
fr<-subset(father.son,round(father.son$fheight)==71)
mean(fr$sheight)
############################
############################
fr<-subset(father.son,round(father.son$fheight)==71)
mean(fr$sheight)
############################
c(1,5,3,4)
X = matrix(1:1000,100,10)
X[25,3]

x<-c(1:10)
c<-cbind(1:10,2*x,3*x,4*x,5*x)
sum(c[7,])
############################

a<-matrix(c(10,5,7,4))
b<-matrix(c(3,2,1,5,4,2,-1,0,-5,2,5,0,1,-1,-5,1),4,4)
solve(b)%*%a

a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)

c<-a%*%b
c[3,2]

sum(a[3,]*b[,2])




