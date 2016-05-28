alphas <- seq(0,0.25,0.01)
par(mfrow=c(2,2))
for(m in c(2,10,100,1000)){
  plot(alphas,alphas/m - (1-(1-alphas)^(1/m)),type="l")
  abline(h=0,col=2,lty=2)
}


set.seed(1)
B <- 10000
m <- 8793
alpha <- 0.05
pvals <- matrix(runif(B*m,0,1),B,m)
k <- alpha/m
mistakes <- rowSums(pvals<k) 
mean(mistakes>0)


set.seed(1)
B <- 10000
m <- 8793
alpha <- 0.05
pvals <- matrix(runif(B*m,0,1),B,m)
k <- 1-(1-alpha)^(1/m)
mistakes <- rowSums(pvals<k) 
mean(mistakes>0)
#######################
library(devtools)
library(rafalib)
install_github("genomicsclass/GSE5859Subset")
install_bioc("genefilter")
install_bioc("qvalue")
library(GSE5859Subset)
data(GSE5859Subset)
library(genefilter)
?rowttests
View(sampleInfo)
View(geneExpression)
View(geneAnnotation)
g <- factor(sampleInfo$group)
pvals = rowttests(geneExpression,g)$p.value
sum(pvals<0.05)
k = 0.05/length(pvals)
sum(pvals<k)

calls <- p.adjust(pvals,method="fdr") < 0.05
sum(calls)
