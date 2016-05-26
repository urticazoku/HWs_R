install.packages("devtools")
library(devtools)
install_github("genomicsclass/GSE5859Subset")
library(GSE5859Subset)
data(GSE5859Subset) ##this loads the three tables
dim(geneExpression)
head(sampleInfo)
dim(sampleInfo)
identical(colnames(geneExpression),sampleInfo$filename)
rownames(geneExpression)
dim(geneAnnotation)
head(geneAnnotation)
library(dplyr)
install.packages(swirl)
library(swirl)
swirl()
sampleInfo[sampleInfo[,2]=="2005-06-27",]
a<-geneAnnotation[geneAnnotation[,2]=="chrY" & !is.na(geneAnnotation[,2]) ,]
dim(a)
colname<-sampleInfo[sampleInfo[,2]=="2005-06-10",3]
g_name<-geneAnnotation[geneAnnotation[,4]=="ARPC1A" & !is.na(geneAnnotation[,4]),1]
head(geneExpression)
View(geneExpression)
geneExpression[g_name,colname]
median(apply(geneExpression,2,median))
g <- factor(sampleInfo$group)

ans<-apply(geneExpression,1,function(x,y) t.test( x[g==1], x[g==0])$p.value)
min(ans)

#######################
set.seed(1)
library(downloader)
url = "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename = "femaleControlsPopulation.csv"
if (!file.exists(filename)) download(url,destfile=filename)
population = read.csv(filename)
pvals <- replicate(1000,{
  control = sample(population[,1],12)
  treatment = sample(population[,1],12)
  t.test(treatment,control)$p.val
})
head(pvals)
hist(pvals)

mean(pvals<0.05)
mean(pvals<0.01)

cases = rnorm(10,30,2)
controls = rnorm(10,30,2)
t.test(cases,controls)

set.seed(100)
pvals<- replicate(20,{
  cases = rnorm(10,30,2)
  controls = rnorm(10,30,2)
  t.test(cases,controls)$p.value
})

set.seed(100)
pvals1<-replicate(1000,{
  
  pvals<-replicate(20,{
    cases = rnorm(10,30,2)
    controls = rnorm(10,30,2)
    t.test(cases,controls)$p.value
  })
  
  sum(pvals<=0.05)

})
mean(pvals1>0)

