library(openxlsx)
library(readxl)
# install.packages("remotes")
# remotes::install_github("hxfan1227/cmdcr")
# library(cmdcr)
# library(dplyr)
library(tidyverse)
library(glmnet)
library(randomForest)
library(caret)
library(doParallel)
library(gbm)
library(kernlab)
library(keras)
library(tensorflow)
# Tibet2<-import_cmdc_data(fname='qinghai')
# 
# worldclim <-worldclim_global(var = "bio", res = 2.5, path="worldclim")
# worldclim<-stack(worldclim[[c("wc2.1_2.5m_bio_1","wc2.1_2.5m_bio_12")]])
# worldclim <- crop(rast(worldclim), ext(15,50,82,122))
# worldclim <- stack(worldclim)
# plot(worldclim)




Tibet<-read_excel("Projet_Machine_Learning_Data.xlsx")
Tibet<-as.data.frame(Tibet)
Tibet2<-matrix(0,nrow=length(Tibet[,1]),ncol=length(Tibet[1,]),dimnames=list(1:length(Tibet[,1]),c("ID","A_Temp","Sp_temp","Su_temp","W_temp","A_prec","Sp_prec","Su_prec","W_prec","A_rad","Sp_rad","Su_rad","W_rad","Silt","Sand","Clay","Aspect","DEM","Slope","pH","SOM","SR")))
Tibet2<-as.data.frame(Tibet2)

for (i in 1:length(Tibet2[,1])){
  Tibet2[i,]<-Tibet[i,]
}
Tibet<-Tibet2
Tibet<-Tibet[,-1]

############################## Boucle ###############################

#SR=Nombre d'espèces identifiées par m2

#Séparation en un jeu de données d'apprentissage et un jeu de données de validation
Results<-matrix(0,ncol=15,nrow=1000,dimnames=list(1:1000,c("RMSERandomF","variablesRandomF","RMSERegLasso","variablesRegLasso","RMSERegRidge","variablesRegRidge","RMSERegelasticnet","variablesRegelasticnet","RMSEGB","variablesGB","RMSESVM","variablesSVM","RMSEResNeu","variablesResNeu","BestMod")))
Results<-as.data.frame(Results)

######################## Validation non croisée de Monte-Carlo
cl <- detectCores() %>% -1 %>% makeCluster
registerDoParallel(cl)
for (i in 1:1000){
  #Sélection du jeu de données-Validation non croisée de Monte-Carlo
  aleat<-sample(length(Tibet$A_Temp),250)
  Tibetapp<-Tibet[aleat,]
  Tibetvalid<-Tibet[-aleat,]
  ##############################################################################
  #Rajouter transformation des variables?Regression de poisson?package keras?
  #Forêt aléatoire
  grille.mtry<-data.frame(mtry=seq(1,length(Tibet[1,]),by=1))
  ctrl<-trainControl(method="oob")# out of bag car validation non croisée
  sel.mtry<-caret::train(SR~.,data=Tibetapp, method="rf", trControl=ctrl, tuneGrid=grille.mtry)
  ForeTibet<-randomForest(SR~.,data=Tibetapp,mtry=sel.mtry$bestTune[1,1],xtest=Tibetvalid[,-21],ytest=Tibetvalid[,21],ntree=250,keep.forest=TRUE)
  prev<-predict(ForeTibet,newdata=Tibetvalid[,1:20])
  Results$RMSERandomF[i]<-sqrt(mean((Tibetvalid[,21]-prev)^2))
  Results$variablesRandomF[i]<-paste(list(names(ForeTibet$importance[1:sel.mtry$bestTune[1,1],1])),sep=" ")
  #Regression sous contraintes
  #Lasso
  lasso<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],type.measure="mse")
  lasso$glmnet.fit$beta
  prev.lasso<-predict(lasso,newx=as.matrix(Tibetvalid[,1:20]))
  Results$RMSERegLasso[i]<-sqrt(mean((Tibetvalid[,21]-prev.lasso)^2))
  
  ridge<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0,type.measure="mse")
  ridge$glmnet.fit$beta
  prev.ridge<-predict(ridge,newx=as.matrix(Tibetvalid[,1:20]))
  Results$RMSERegRidge[i]<-sqrt(mean((Tibetvalid[,21]-prev.ridge)^2))
  #Elasticnet
  elasticnet<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0.5,type.measure="mse")
  prev.elasticnet<-predict(elasticnet,newx=as.matrix(Tibetvalid[,1:20]))
  Results$RMSERegelasticnet[i]<-sqrt(mean((Tibetvalid[,21]-prev.elasticnet)^2))
  #Gradient boosting
  Gradboost<-gbm(SR~.,data=Tibetapp,distribution="gaussian",shrinkage=0.01,n.trees=3000)
  prev.GB<-predict(Gradboost,newdata=Tibetvalid[,1:20])
  Results$RMSEGB[i]<-sqrt(mean((Tibetvalid[,21]-prev.GB)^2))
  #SVM
  #Enlever transformation des variables?
  C<-c(0.1,1,10,100)
  degree<-c(1,2,3)
  scale<-1
  sigma<-c(0.0001,0.001,0.01,0.1,1)
  ctrl<-trainControl(method="cv",number=3)
  gr.poly<-expand.grid(C=c(0.1,1,10,100),degree=c(1,2,3),scale=1)
  sel.poly<-caret::train(SR~.,data=Tibetapp,method="svmPoly",trControl=ctrl,tuneGrid=gr.poly)
  gr.radial<-expand.grid(C=C,sigma=sigma)
  sel.radial<-caret::train(SR~.,data=Tibetapp,method="svmRadial",trControl=ctrl,tuneGrid=gr.radial)
  if(min(sel.poly$results$RMSE)>min(sel.radial$results$RMSE)){
    sigma<-sel.radial$bestTune$sigma
    C<-sel.radial$bestTune$C
    Modsvm<-ksvm(SR~.,data=Tibetapp,kernel="rbfdot",kpar=list(sigma=sigma),C=C)
  }
  if(min(sel.poly$results$RMSE)<min(sel.radial$results$RMSE)){
    C<-sel.poly$bestTune$C
    degree<-sel.poly$bestTune$degree
    scale<-sel.poly$bestTune$scale
    Modsvm<-ksvm(SR~.,data=Tibetapp,kernel="polydot",kpar=list(degree=degree,scale=scale),C=C)
  }
  prev.svm<-predict(Modsvm,newdata=Tibetvalid[,1:20])
  Results$RMSESVM[i]<-sqrt(mean((Tibetvalid[,21]-prev.svm)^2))
  #Réseau de neurones
  param_grid<-expand.grid(size=seq(1,length(Tibet[1,]),by=1),
                          lambda=seq(0,1,0.1),
                          batch_size=seq(10,35,1),
                          lr=c(0.001,0.01,0.1,1),
                          rho=seq(0,1,0.1),
                          decay=seq(0,1,0.1),
                          activation=c("relu","tanh"))
  ctrl<-trainControl(method="cv",number=5)
  caret_mlp<-caret::train(SR~.,
                   data=Tibetapp,
                   method="mlpKerasDecay",
                   tuneGrid=param_grid,
                   epoch=30,
                   verbose=0,
                   trControl=ctrl)
  prev.ResNeu<-predict(caret_mlp,newdata=Tibetvalid[,1:20])
  Results$RMSEResNeu[i]<-sqrt(mean((Tibetvalid[,21]-prev.ResNeu)^2))
 #Récupérer pour chaque ligne l'algorithme vainqueur

}
stopCluster(cl)
#Transformation de la variable, régression de poisson
Mtry#Vecteur du nombre de variables sélectionnées
rmse#Plus il est bas, plus le modéle est parfait
var#Nom des variables sélectionnées
var
a<-paste(var[[1]],sep=" ")
################################################################################
############ Régression sous-contraintes-Validation non-croisée de Monte Carlo


lasso<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],type.measure="mse")
ridge<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0,type.measure="mse")



plot(lasso)
plot.cv.glmnet(lasso)

ridge<-glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0)
while (alpha<1){
  elasticnet<-glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0.5)


plot(lasso)
plot(ridge)
plot(elasticnet)

Llasso<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21])
Lridge<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0)
Lelasticnet<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0.5)

plot(Llasso)
plot(Lridge)
plot(Lelasticnet)
#Valeur optimale de lambda obtenue pour l'erreur la plus basse

prev.lasso<-predict(Llasso,newx=as.matrix(Tibetvalid[,1:20]))
prev.ridge<-predict(Lridge,newx=as.matrix(Tibetvalid[,1:20]))
prev.elasticnet<-predict(Lelasticnet,newx=as.matrix(Tibetvalid[,1:20]))




















#Creation matrice resutats
Resulats = data.frame(Mtry,rmse,var)

aleat<-sample(length(Tibet$A_Temp),250)
#Jeu de données d'apprentissage
Tibetapp<-Tibet[aleat,]
#Jeu de données de validation
Tibetvalid<-Tibet[-aleat,]
library(randomForest)
#Selection du parametre mtry (nombre de variables utilisées pour la construction de la foret)
grille.mtry<-data.frame(mtry=seq(1,length(Tibet[1,]),by=1))
library(caret)
ctrl<-trainControl(method="oob")
#oob = Out of bag
library(doParallel)
cl <- detectCores() %>% -1 %>% makeCluster
registerDoParallel(cl)
sel.mtry<-train(SR~.,data=Tibetapp, method="rf", trControl=ctrl, tuneGrid=grille.mtry)
stopCluster(cl)
sel.mtry#Différence entre vraies valeurs et prédictions
mtry<-sel.mtry$bestTune[1,1]



ForeTibet<-randomForest(Tibetapp$SR~.,data=Tibetapp,mtry=2)
plot(ForeTibet)
Prev.valid<-predict(ForeTibet,newdata=Tibetvalid[,1:20])
ForeTibet2<-randomForest(SR~.,data=Tibetapp,xtest=Tibetvalid[,-21],ytest=Tibetvalid[,21],keep.forest=TRUE)
ForeTibet3<-randomForest(SR~.,data=Tibetapp,mtry=2,xtest=Tibetvalid[,-21],ytest=Tibetvalid[,21],keep.forest=TRUE)
ForeTibet2
ForeTibet3#Modèle retenu
# library(pROC)
prev3<-predict(ForeTibet3,newdata=Tibetvalid[,1:20])
# 
# pre<-rep(0,95)
# for (i in 1:length(prev3)){
#   pre[i]<-prev3[[i]]
# }
# prev3<-pre
# roc3<-multiclass.roc(Tibetvalid$SR,prev3)
var.imp<-ForeTibet3$importance
ord<-order(var.imp,decreasing=TRUE)
barplot(sort(var.imp,decreasing=TRUE),
        names.arg=rownames(var.imp)[ord],cex.names=0.6)
################################################################################
######################## Regression sous contraintes
library(glmnet)
lasso<-glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21])
ridge<-glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0)
elasticnet<-glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0.5)
par(mfrow=c(1,3))

plot(lasso)
plot(ridge)
plot(elasticnet)

Llasso<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21])
Lridge<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0)
Lelasticnet<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0.5)

plot(Llasso)
plot(Lridge)
plot(Lelasticnet)
#Valeur optimale de lambda obtenue pour l'erreur la plus basse

prev.lasso<-predict(Llasso,newx=as.matrix(Tibetvalid[,1:20]))
prev.ridge<-predict(Lridge,newx=as.matrix(Tibetvalid[,1:20]))
prev.elasticnet<-predict(Lelasticnet,newx=as.matrix(Tibetvalid[,1:20]))

##Vérification des prédiction?ROC?


################################################################################
############################ Gradient boosting

#set.seed(1234) fixation de graine car échantillons bootstrapés
library(gbm)
hist(Tibetapp$SR)
Gradboost<-gbm(SR~.,data=Tibetapp,distribution="poisson",shrinkage=0.01,n.trees=3000)
pred.GB<-predict(Gradboost,newdata=Tibetvalid)
################################################################################
########################### SVM

################################################################################
########################## Réseaux de neuronnes

####### Comparaison de méthodes avec caret























