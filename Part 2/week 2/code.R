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
