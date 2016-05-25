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

