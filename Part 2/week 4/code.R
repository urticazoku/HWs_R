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
