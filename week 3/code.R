library(downloader)
library(dplyr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

N=25
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N)
dat.s <- sample(bwt.smoke , N)

X.ns <- mean(dat.ns)
sd.ns <- sd(dat.ns)

X.s <- mean(dat.s)
sd.s <- sd(dat.s)

sd.diff <- sqrt(sd.ns^2/N+sd.s^2/N)
tval <- (X.ns - X.s)/sd.diff
tval
2*pnorm(-abs(tval))

N <- 25
qnorm(0.995)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )
N <- 5
set.seed(1)
dat.ns <- sample(bwt.nonsmoke, N) 
dat.s <- sample(bwt.smoke, N) 
qt(0.995,48)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )
t.test(dat.ns,dat.s)
##############################

set.seed(1)
chowPopulation<-read.csv("femaleControlsPopulation.csv")
chowPopulation<-unlist(chowPopulation)
mu_chow<-mean(chowPopulation)
print(mu_chow)

N<-30
chow<-sample(chowPopulation,N)
print(mean(chow))
se<-sd(chow)/sqrt(N)
print(se)

#mean(chow)-mean(chowPopulation)/se
#pnorm(2)-pnorm(-2)

Q<-qnorm(1-0.05/2)
interval<-c(mean(chow)-Q*se, mean(chow)+Q*se)
interval

###############################

library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- "mice_pheno.csv"
if(!file.exists(filename)) download(url,destfile=filename)
library(dplyr)
dat <- read.csv("mice_pheno.csv") #Previously downloaded 

controlPopulation <- filter(dat,Sex == "F" & Diet == "chow") %>%  
  select(Bodyweight) %>% unlist

hfPopulation <- filter(dat,Sex == "F" & Diet == "hf") %>%  
  select(Bodyweight) %>% unlist

mu_hf <- mean(hfPopulation)
mu_control <- mean(controlPopulation)
print(mu_hf - mu_control)
print((mu_hf - mu_control)/mu_control * 100) #percent increase

set.seed(1)
N <- 5
hf <- sample(hfPopulation,N)
control <- sample(controlPopulation,N)
t.test(hf,control)$p.value

N <- 12
alpha <- 0.05
B <- 2000
reject <- function(N, alpha=0.05){
  hf <- sample(hfPopulation,N) 
  control <- sample(controlPopulation,N)
  pval <- t.test(hf,control)$p.value
  pval < alpha
}

N=90
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.01
})
mean(rejects)

#################
#################

set.seed(1)

library(rafalib)
dat<-read.csv("mice_pheno.csv")
controlPopulation<-read.csv("femaleControlsPopulation.csv")
controlPopulation<-unlist(controlPopulation)

ttestgenerator<- function(n) {
  cases<-sample(controlPopulation,n)
  controls<-sample(controlPopulation,n)
  tstat<-(mean(cases)-mean(controls))/sqrt(var(cases)/n+var(control)/n)
  return(tstat)
}
ttests<-replicate(1000,ttestgenerator(10))
hist(ttests)
qqnorm(ttests)
abline(0,1)

ttests<-replicate(1000,ttestgenerator(3))
hist(ttests)
qqnorm(ttests)
abline(0,1)

ps<-(seq(0,999)+0.5)/1000
qqplot(qt(ps,df=2*3-2),ttests,xlim=c(-6,6),ylim=c(-6,6))
abline(0,1)

qqnorm(controlPopulation)
qqline(controlPopulation)

controls<-rnorm(5000,mean=24,sd=3.5)

ttestgenerator<-function(n,mean=24,sd=3.5){
  cases<-rnorm(n,mean,sd)
  controls<-rnorm(n,mean,sd)
  tstat<-(mean(cases)-mean(controls))/sqrt(var(cases)/n+var(controls)/n)
  return(tstat)
}
ttests<-replicate(1000,ttestgenerator(3))
qqnorm(ttests)
abline(0,1)

########################
set.seed(1)
tgen<-function(n,mean=0,sd=1){
  X<-rnorm(n,mean,sd)
  tstat<-sqrt(5)*mean(X)/sd(X)
  return(tstat)
}
set.seed(1)
B<-1000
ttests<-replicate(B,tgen(50))
mean(ttests>2)

set.seed(1)
N <- 1000
B <- 10000
tstats <- replicate(B,{
  X <-  sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
qqnorm(tstats)
abline(0,1)

######################

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- median(smokers) - median(nonsmokers)

set.seed(1)
null <- replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  median(smokersstar) - median(nonsmokersstar)
})
( sum( abs(null) >= abs(obs)) +1 ) / ( length(null)+1 ) 
##we add the 1s to avoid p-values=0 but we also accept:
( sum( abs(null) >= abs(obs)) ) / ( length(null) )

##############
##############

d = read.csv("assoctest.csv")
t<-table(d)
chisq.test(t)
fisher.test(t)


