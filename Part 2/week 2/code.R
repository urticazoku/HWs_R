g<-9.8
n<-25
tt<-seq(0,3.4,len=n)
f<-56.67+0*tt-0.5*g*tt^2
y<-f+rnorm(n,sd=1)
plot(tt,y,xlab="Time in secs", ylab="Distance in meters")
lines(tt,f,col=2)

rss<-function(beta0,beta1,beta2){
  r<-y-(beta0+beta1*tt+beta2*tt^2)
  sum(r^2)
}

beta2s<-seq(-10,0,len=100)
RSS<-sapply(beta2s,rss,beta0=65,beta1=0)
lines(beta2s,RSS,type="l",col=3)

tt2<-tt^2
fit<-lm(y~tt+tt2)
summary(fit)

X<-cbind(rep(1,length(tt)),tt,tt^2)
head(X)
Beta<-matrix(c(55,0,5),3,1)
r<-y-X%*%Beta
RSS<-t(r)%*%r
RSS<-crossprod(r)

betahat<-solve(t(X)%*%X)%*%t(X)%*%y

QR<-qr(X)
Q<-qr.Q(QR)
R<-qr.R(QR)
backsolve(R,crossprod(Q,y))

#######################

X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")
beta <- c(5, 2)

X%*%beta

X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")

beta <- c(10,3,-3)

X%*%beta

#####################
N<-100000
#ans<-vector("numeric",N)
g<-9.8
h0<-56.67
v0<-0
n<-25
tt<-seq(0,3.4,len=n)

fMC<-function(){
  y<-h0+v0*tt-0.5*g*tt^2+rnorm(n,sd=1)
  X<-cbind(1,tt,tt^2)
  A<-solve(crossprod(X))%*%t(X)
  
  return(-2*(A%*%y)[3])
}
set.seed(1)
r<-replicate(N,fMC())
sd(r)

######################

library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)
N =  50

mean( (y - mean(y))*(x-mean(x) ) )

f<-function(){
index = sample(n,N)
sampledat = father.son[index,]
x = sampledat$fheight
y = sampledat$sheight

betahat =  lm(y~x)$coef
return(betahat[2])
}
set.seed(1)

r<-replicate(10000,f())
sd(r)
#########################
library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)
N <-  50
set.seed(1)
mean( (y - mean(y))*(x-mean(x) ) )

index = sample(n,N)
sampledat = father.son[index,]
x = sampledat$fheight
y = sampledat$sheight

betahat =  lm(y~x)$coef
return(betahat[2])

#r<-replicate(10000,f())
#sd(r)

fit <- lm(y ~ x)
Yhat<-fit$fitted.values
sum((y - fit$fitted.values)^2)

X <- cbind(rep(1,N), x)

N <- nrow(X)
p <- ncol(X)

XtXinv <- solve(crossprod(X))

resid <- y - X %*% XtXinv %*% crossprod(X,y)

s <- sqrt( sum(resid^2)/(N-p))
ses <- sqrt(diag(XtXinv))*s 
