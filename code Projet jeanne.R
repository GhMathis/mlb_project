library(readxl)
library(tidyverse)
library(glmnet)
library(randomForest)
library(caret)
library(doParallel)
library(gbm)
library(kernlab)
library(keras)
library(tensorflow)


fichier <- "C:/Users/thill/Documents/M2 MODE/MLB/mlb_project/data.xlsx"

Tibet<-read_excel(fichier)
Tibet<-as.data.frame(Tibet)

#Voire si on peut faire plus court
Tibet2<-matrix(0,nrow=length(Tibet[,1]),ncol=length(Tibet[1,]),dimnames=list(1:length(Tibet[,1]),c("ID","A_Temp","Sp_temp","Su_temp","W_temp","A_prec","Sp_prec","Su_prec","W_prec","A_rad","Sp_rad","Su_rad","W_rad","Silt","Sand","Clay","Aspect","DEM","Slope","pH","SOM","SR")))
Tibet2<-as.data.frame(Tibet2)

for (i in 1:length(Tibet2[,1])){
  Tibet2[i,]<-Tibet[i,]
}
Tibet<-Tibet2
Tibet<-Tibet[,-1]



#SR=Nombre d'espèces identifiées par m2

# création du tableau de résultats (peut etre faire un tableau pour chaque methode?) --> le faire ca sera plus pratique
Results<-matrix(0,ncol=15,nrow=1000,dimnames=list(1:1000,c("RMSERandomF","variablesRandomF","RMSERegLasso","variablesRegLasso","RMSERegRidge","variablesRegRidge","RMSERegelasticnet","variablesRegelasticnet","RMSEGB","variablesGB","RMSESVM","variablesSVM","RMSEResNeu","variablesResNeu","BestMod")))
Results<-as.data.frame(Results)

###### Definition des fonctions ########

#Forêt aléatoire

ForetAlea <- function(Tibetapp,Tibetvalid,Results){
  
  #selections des variables (du nb de variables ?) utiliser pour le modele = mtry ?
  grille.mtry<-data.frame(mtry=seq(1,length(Tibetapp[1,]),by=1)) #?
  ctrl<-trainControl(method="oob")# out of bag car validation non croisée
  sel.mtry<-caret::train(SR~.,data=Tibetapp, method="rf", trControl=ctrl, tuneGrid=grille.mtry) 
  
  # entrainement du model sur le jeu d'entrainement : Tibetapp
  ForeTibet<-randomForest(SR~.,data=Tibetapp,mtry=sel.mtry$bestTune[1,1],xtest=Tibetvalid[,-21],ytest=Tibetvalid[,21],ntree=250,keep.forest=TRUE)
  
  # Prediction sur le jeu de validation : Tibetvalid
  prev<-predict(ForeTibet,newdata=Tibetvalid[,1:20])
  
  # Calcul du RMSE pour évaluer la qualité du modele
  Results$RMSERandomF[i]<-sqrt(mean((Tibetvalid[,21]-prev)^2))
  
  # Stockages des variables utilisées pour le modele
  Results$variablesRandomF[i]<-paste(list(names(ForeTibet$importance[1:sel.mtry$bestTune[1,1],1])),sep=" ") 
  
  return(Results)
}

#Regression sous contraintes

Lasso <- function(Tibetapp, Tibetvalid, Results){
  
  # entrainement du modele sur le jeu d'entrainement : Tibetapp
  lasso<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],type.measure="mse")
  
  # Prediction sur le jeu de validation : Tibetvalid 
  prev.lasso<-predict(lasso,newx=as.matrix(Tibetvalid[,1:20]))
  
  # Calcul du RMSE pour évaluer la qualité du modele
  Results$RMSERegLasso[i]<-sqrt(mean((Tibetvalid[,21]-prev.lasso)^2))
  
  #Stockage des coefficient associer à chaque variable ?
  
  return(Results)
}


Ridge <- function(Tibetapp, Tibetvalid, Results){
  
  # entrainement du modele sur le jeu d'entrainement : Tibetapp
  ridge<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0,type.measure="mse")
  
  # Prediction sur le jeu de validation : Tibetvalid 
  prev.ridge<-predict(ridge,newx=as.matrix(Tibetvalid[,1:20]))
  
  # Calcul du RMSE pour évaluer la qualité du modele
  Results$RMSERegRidge[i]<-sqrt(mean((Tibetvalid[,21]-prev.ridge)^2))
  
  #Stockage des coefficient associer à chaque variable ?
  
  return(Results)
}


Elasticnet <- function(Tibetapp, Tibetvalid, Results){
  
  # entrainement du modele sur le jeu d'entrainement : Tibetapp
  elasticnet<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0.5,type.measure="mse")
  
  # Prediction sur le jeu de validation : Tibetvalid 
  prev.elasticnet<-predict(elasticnet,newx=as.matrix(Tibetvalid[,1:20]))
  
  # Calcul du RMSE pour évaluer la qualité du modele
  Results$RMSERegelasticnet[i]<-sqrt(mean((Tibetvalid[,21]-prev.elasticnet)^2))
  
  #Stockage des coefficient associer à chaque variable ?
  
  return(Results)
}


GradientBoosting <- function(Tibetapp, Tibetvalid, Results){
  
  # entrainement du modele sur le jeu d'entrainement : Tibetapp
  Gradboost<-gbm(SR~.,data=Tibetapp,distribution = 'gaussian',shrinkage=0.01,n.trees=3000) 
  
  # Prediction sur le jeu de validation : Tibetvalid 
  prev.GB<-predict(Gradboost,newdata=Tibetvalid[,1:20])
  
  # Calcul du RMSE pour évaluer la qualité du modele
  Results$RMSEGB[i]<-sqrt(mean((Tibetvalid[,21]-prev.GB)^2))
  
  #Stockage des coefficient associer à chaque variable ?
  
  return(Results)
}

#Réseau de neurones : il faut modifier les paramêtres de la grille d'expérience parceque ça prend trop de temps à tourner

RdN <- function(Tibetapp, Tibetvalid, Results){
  
  # Initialisation du reseau de neurones
  param_grid<-expand.grid(size=seq(1,length(Tibet[1,]),by=1),
                          lambda=seq(0,1,0.1),
                          batch_size=seq(10,35,1),
                          lr=c(0.001,0.01,0.1,1),
                          rho=seq(0,1,0.1),
                          decay=seq(0,1,0.1),
                          activation=c("relu","tanh"))
  
  ctrl<-trainControl(method="cv",number=5) #? je sais plus ce que fait contrôle deja... (pour dire qu'on fait de la validation croisé)
  
  # entrainement du modele sur le jeu d'entrainement : Tibetapp
  caret_mlp<-caret::train(SR~.,
                          data=Tibetapp,
                          method="mlpKerasDecay",
                          tuneGrid=param_grid,
                          epoch=30,
                          verbose=0,
                          trControl=ctrl)
  
  # Prediction sur le jeu de validation : Tibetvalid 
  prev.ResNeu<-predict(caret_mlp,newdata=Tibetvalid[,1:20])
  
  # Calcul du RMSE pour évaluer la qualité du modele
  Results$RMSEResNeu[i]<-sqrt(mean((Tibetvalid[,21]-prev.ResNeu)^2))
  
  #Stockage des coefficient associer à chaque variable ?
  #Récupérer pour chaque ligne l'algorithme vainqueur ?
  
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
  
  #Forêt aléatoire
  Results <- ForetAlea(Tibetapp,Tibetvalid,Results)
  #Regression sous contraintes
  #Lasso
  Results <- Lasso(Tibetapp,Tibetvalid,Results)
  
  #Ridge
  Results <- Ridge(Tibetapp,Tibetvalid,Results)
  
  #Elasticnet
  Results <- Elasticnet(Tibetapp,Tibetvalid,Results)
  
  #Gradient boosting 
  Results <- GradientBoosting(Tibetapp,Tibetvalid,Results)
  
  
  #SVM ?? Je sais pas si on garde c'est peut etre un peu compliqué
  #Enlever transformation des variables?
  C<-c(0.1,1,10,100) #coût
  degree<-c(1,2,3)
  scale<-1
  sigma<-c(0.0001,0.001,0.01,0.1,1)
  ctrl<-trainControl(method="cv",number=3) #idem je sais plus a quoi ca sert
  
  gr.poly<-expand.grid(C=c(0.1,1,10,100),degree=c(1,2,3),scale=1) #Grille de parametres a tester ?
  sel.poly<-caret::train(SR~.,data=Tibetapp,method="svmPoly",trControl=ctrl,tuneGrid=gr.poly)
  
  gr.radial<-expand.grid(C=C,sigma=sigma)
  sel.radial<-caret::train(SR~.,data=Tibetapp,method="svmRadial",trControl=ctrl,tuneGrid=gr.radial)
  
  if(min(sel.poly$results$RMSE)>min(sel.radial$results$RMSE)){ # si la methode svmRadial renvoie le meilleur modele ? Comment on peut juger avant de l'avoire fait prédire???
    sigma<-sel.radial$bestTune$sigma
    C<-sel.radial$bestTune$C
    Modsvm<-ksvm(SR~.,data=Tibetapp,kernel="rbfdot",kpar=list(sigma=sigma),C=C)
  } else {
    C<-sel.poly$bestTune$C
    degree<-sel.poly$bestTune$degree
    scale<-sel.poly$bestTune$scale
    Modsvm<-ksvm(SR~.,data=Tibetapp,kernel="polydot",kpar=list(degree=degree,scale=scale),C=C)
  } #rajouter une colonne pour la methode
  prev.svm<-predict(Modsvm,newdata=Tibetvalid[,1:20])
  Results$RMSESVM[i]<-sqrt(mean((Tibetvalid[,21]-prev.svm)^2))
  
  
  #Réseau de neurones
  Results <- RdN(Tibetapp,Tibetvalid,Results)
  
}
#stopCluster(cl)
#Transformation de la variable, régression de poisson
Mtry#Vecteur du nombre de variables sélectionnées
rmse#Plus il est bas, plus le modéle est parfait
var#Nom des variables sélectionnées
var
a<-paste(var[[1]],sep=" ") #??

























