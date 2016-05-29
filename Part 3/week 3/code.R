prob<-0.49
size<-4
dbinom(2,size,prob)

prob<-0.49
size<-10
dbinom(4,size,prob)

pc<-0.2
pg<-0.2
pt<-0.3
pa<-0.3
n<-20

1-pbinom(0.5*n,n,pc+pg)
pbinom(9,20,0.4)-pbinom(7,20,0.4)
exact = pbinom(450,1000,0.4)-pbinom(350,1000,0.4)
b <- (450 - 1000*.4)/sqrt(1000*.4*.6)
a <- (350 - 1000*.4)/sqrt(1000*.4*.6)
approx <- pnorm(b)-pnorm(a)
abs(exact-approx)

p<-1/175223510
n<-189000000
1 - dbinom(0, 189000000, 1/175223510)
N <- 189000000
p <- 1/175223510

N = 189000000
p = 1/175223510
1 - ppois(1, N*p)
