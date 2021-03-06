species <- factor(c("A","A","B","B"))
condition <- factor(c("control","treated","control","treated"))
X<-model.matrix(~ species + condition)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)
fitTL <- lm(friction ~ type + leg, data=spider)
L4vsL2 <- contrast(fitTL,list(leg="L4",type="pull"),list(leg="L2",type="pull"))
L4vsL2

X <- model.matrix(~ type + leg, data=spider)
(Sigma <- sum(fitTL$residuals^2)/(nrow(X) - ncol(X)) * solve(t(X) %*% X))
C <- matrix(c(0,0,-1,0,1),1,5)
Sigma[3,5]

#####################
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)
spider$log2friction <- log2(spider$friction)
boxplot(log2friction ~ type*leg, data=spider)

X <- model.matrix(~ type + leg + type:leg, data=spider)
colnames(X)
head(X)

fitX <- lm(log2friction ~ type + leg + type:leg, data=spider)
summary(fitX)
coefs <- coef(fitX)

library(contrast)
L2vsL1.push <- contrast(fitX,
                           list(leg="L2", type = "push"), 
                           list(leg="L1", type = "push"))
anova(fitX)
###############
N <- 40
p <- 4

f<-function(){
  group <- factor(rep(1:p,each=N/p))
  X <- model.matrix(~ group)
  Y <- rnorm(N,mean=42,7)
  mu0 <- mean(Y)
  initial.ss <- sum((Y - mu0)^2)
  s <- split(Y, group)
  after.group.ss <- sum(sapply(s, function(x) sum((x - mean(x))^2)))
  group.ss <- initial.ss - after.group.ss
  group.ms <- group.ss / (p - 1)
  after.group.ms <- after.group.ss / (N - p)
  f.value <- group.ms / after.group.ms
  return(f.value)
}
set.seed(1)
r<-replicate(1000,f())
mean(r)

###################
###################

sex <- factor(rep(c("female","male"),each=4))
trt <- factor(c("A","A","B","B","C","C","D","D"))
X <- model.matrix( ~ sex + trt)
qr(X)$rank
Y <- 1:8
makeYstar <- function(a,b) Y - X[,2] * a - X[,5] * b
fitTheRest <- function(a,b) {
  Ystar <- makeYstar(a,b)
  Xrest <- X[,-c(2,5)]
  betarest <- solve(t(Xrest) %*% Xrest) %*% t(Xrest) %*% Ystar
  residuals <- Ystar - Xrest %*% betarest
  sum(residuals^2)
}

fitTheRest(1,2)

expand.grid(1:3,1:3)
betas = expand.grid(1:5,1:5)
rss = apply(betas,1,function(x) fitTheRest(x[1],x[2]))
rss

library(rafalib)
themin=min(rss)

plot(betas[which(rss==themin),])

#################

fit <- lm(friction ~ type + leg, data=spider)
betahat <- coef(fit)
Y <- matrix(spider$friction, ncol=1)
X <- model.matrix(~ type + leg, data=spider)
QR <- qr(X)
Q <- qr.Q( QR )
R <- qr.R( QR )
Q[1,1]
R[1,1]
betahat<-backsolve(R,crossprod(Q,Y))
R%*%betahat
