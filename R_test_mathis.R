library(tidyverse)
library(randomForest)
library(lme4)
library(corrplot)
library(lattice)
data<-read.table("data.txt",header=TRUE,sep="\t",dec = ",")
str(data)
corrplot(cor(data[,-c(1,22)]))
data$SR.no.per.m2. = as.factor(data$SR.no.per.m2.)
rdf = randomForest(SR.no.per.m2.~ . ,data[,-1], type = "classification" )
str(rdf)
varImpPlot(rdf)
rdf$c
levelplot(rdf$confusion[,27:1], xlab = "observed", ylab = "predicted")
data[data$SR.no.per.m2.==12,]
table(data$SR.no.per.m2.)


##### Mod generalise
str(data)
data<-read.table("data.txt",header=TRUE,sep="\t",dec = ",")
data_long = data%>%
  pivot_longer(-c(ID,SR.no.per.m2.), values_to = "explicative_val",
               names_to = "explicative_name")
str(data_long)
ggplot(data_long)+
  geom_point(aes(x = explicative_val , y = SR.no.per.m2.))+
  facet_wrap(~explicative_name,scales = "free")
glim
