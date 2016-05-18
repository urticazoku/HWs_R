install.packages("UsingR")
library(UsingR)
data("father.son",package="UsingR")
mean(father.son$sheight)
############################
fr<-subset(father.son,round(father.son$fheight)==71)
mean(fr$sheight)
############################




