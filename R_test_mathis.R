library(readxl)
library(tidyverse)
library(glmnet)
library(randomForest)
library(caret)
library(doParallel)
library(gbm)
library(kernlab)
library(e1071)

Tibet = read.table("data.txt",header=TRUE,sep="\t",dec = ",")
Tibet<-as.data.frame(Tibet)

names(Tibet) = c("ID","A_Temp","Sp_temp","Su_temp","W_temp","A_prec","Sp_prec",
                 "Su_prec","W_prec","A_rad","Sp_rad","Su_rad","W_rad","Silt",
                 "Sand","Clay","Aspect","DEM","Slope","pH","SOM","SR")
Tibet<-Tibet[,-1]

#SR=Nombre d'espèces identifiées par m2

# création du tableau de résultats (peut etre faire un tableau pour chaque methode?) --> le faire ca sera plus pratique
Results<-matrix(0,ncol=5,nrow=1000,dimnames=list(1:1000,c("RMSEGB","variablesGB","RMSESVM","variablesSVM","BestMod")))
Results<-as.data.frame(Results)

###### Definition des fonctions ########

GradientBoosting <- function(Tibetapp, Tibetvalid, Results){

  # entrainement du modele sur le jeu d'entrainement : Tibetapp
  Gradboost<-gbm(SR~.,data=Tibetapp,distribution = 'poisson',shrinkage=0.01,n.trees=3000) 
 
    # Prediction sur le jeu de validation : Tibetvalid 
  prev.GB<-predict.gbm(Gradboost,newdata=Tibetvalid[,1:20],type ="response")
  
  # Calcul du RMSE pour évaluer la qualité du modele
  Results$RMSEGB[i]<-sqrt(mean((Tibetvalid[,21]-prev.GB)^2))
  
  #Stockage des coefficient associer à chaque variable ?
  
  return(Results)
}


svm.regression = function(Tibetapp, Tibetvalid, Results){
  Modsvm = svm(SR~.,data=Tibetapp)
  
  prev.svm<-predict(Modsvm,newdata=Tibetvalid[,1:20])
  Results$RMSESVM[i]<-sqrt(mean((Tibetvalid[,21]-prev.svm)^2))
  return(Results)
}

############################## Boucle (Faire plusieures boucles séparées serait surement mieux + es ce que des boucles pour lasso ridge elasticnet GB c pertinent vu que il n'y a pas de section des variables avant, en ros es que le resultat est dif a chaque fois..??) ###############################
######################## Validation non croisée de Monte-Carlo
#cl <- detectCores() %>% -1 %>% makeCluster
#registerDoParallel(cl)

for (i in 1:1000){ # Faire des boucles séparées pour chaque methode en vérifant avant le temps que ça prend pour tourner une seule fois 
  
  #Sélection du jeu de données-Validation non croisée de Monte-Carlo
  aleat<-sample(length(Tibet$A_Temp),250) #explication du 250 à faire
  #Séparation en un jeu de données en un d'apprentissage et un de validation
  Tibetapp<-Tibet[aleat,]
  Tibetvalid<-Tibet[-aleat,]
  ##############################################################################
  #Rajouter transformation des variables?Regression de poisson?package keras?
  
  #Gradient boosting 
  Results <- GradientBoosting(Tibetapp,Tibetvalid,Results)
  Results <- svm.regression(Tibetapp,Tibetvalid,Results)
}
Results_long = Results%>%
  pivot_longer(-c(variablesGB,variablesSVM, BestMod))
Results_long$name = as.factor(Results_long$name)
levels(Results_long$name) = c("Grad boost" , "SVM")
ggplot(Results_long)+
  geom_boxplot(aes(name, value))+
  labs(y= "RMSE",x ="Modèles")

#stopCluster(cl)
#Transformation de la variable, régression de poisson
Mtry#Vecteur du nombre de variables sélectionnées
rmse#Plus il est bas, plus le modéle est parfait
var#Nom des variables sélectionnées
var
a<-paste(var[[1]],sep=" ") #??
























