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




