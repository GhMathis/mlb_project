
# Function for random forest 

rdm_forest <- function(data_app, data_valid, i){

xtest=data_valid[,-21]
ytest=data_valid$SR
ForeTibet<-randomForest(SR ~ ., data=data_app, xtest=xtest, ytest=ytest,ntree=100,keep.forest=TRUE)

prev<-predict(ForeTibet,newdata=data_valid[,1:20])
SR = (data_valid[,21]-prev)^2

return(list(Forest = ForeTibet, 
           RMSE = sqrt(sum(SR$SR)/length(SR$SR))))
}


for (i in 1:1000){
  aleat<-sample(length(Tibet$A_Temp),250)
  Tibetapp<-Tibet[aleat,]
  Tibetvalid<-Tibet[-aleat,]
  hop = rdm_forest(Tibetapp, Tibetvalid, i)
  Results$variablesRandomF[i]<-list(hop[[1]]$importance)
  Results$RMSERandomF[i]<-hop$RMSE
}

meilleur <- which(Results$RMSERandomF==min(Results$RMSERandomF)) # RdmF qui a le plus petit RMSE
meilleur <-  data.frame(variable = as.factor(row.names(Results$variablesRandomF[[meilleur]])),
                        importance = Results$variablesRandomF[[meilleur]])

ggplot(meilleur , aes(x = variable,y = IncNodePurity ))+
  geom_bar(stat = "identity")
  
# Function for Lasso


for (i in 1:1000){
  aleat<-sample(length(Tibet$A_Temp),250)
  Tibetapp<-Tibet[aleat,]
  Tibetvalid<-Tibet[-aleat,]
  lasso<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp$SR,type.measure="mse")
  prev.lasso<-predict(lasso,newx=as.matrix(Tibetvalid[,1:20]))
  Results$RMSERegLasso[i]<-sqrt(mean((Tibetvalid$SR-prev.lasso)^2))
}


# for ridge :

for (i in 1:1000){
  aleat<-sample(length(Tibet$A_Temp),250)
  Tibetapp<-Tibet[aleat,]
  Tibetvalid<-Tibet[-aleat,]
  ridge<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp$SR,alpha=0,type.measure="mse")
  prev.ridge<-predict(ridge,newx=as.matrix(Tibetvalid[,1:20]))
  Results$RMSERegRidge[i]<-sqrt(mean((Tibetvalid$SR-prev.ridge)^2))
}



# Elasticnet

for (i in 1:1000){
  aleat<-sample(length(Tibet$A_Temp),250)
  Tibetapp<-Tibet[aleat,]
  Tibetvalid<-Tibet[-aleat,]
  elasticnet<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp$SR,alpha=0.5,type.measure="mse")
  prev.elasticnet<-predict(elasticnet,newx=as.matrix(Tibetvalid[,1:20]))
  Results$RMSERegelasticnet[i]<-sqrt(mean((Tibetvalid$SR-prev.elasticnet)^2))
}


# Gradient boosting

for (i in 1:1000){
  aleat<-sample(length(Tibet$A_Temp),250)
  Tibetapp<-Tibet[aleat,]
  Tibetvalid<-Tibet[-aleat,]
  Gradboost<-gbm(SR~.,data=Tibetapp,distribution="gaussian",shrinkage=0.01,n.trees=3000)
  prev.GB<-predict(Gradboost,newdata=Tibetvalid[,1:20])
  Results$RMSEGB[i]<-sqrt(mean((Tibetvalid$SR-prev.GB)^2))
}




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


# réseau de neurones 

param_grid<-expand.grid(size=seq(1,length(Tibet[1,]),by=1),
                        lambda=seq(0,1,0.1),
                        batch_size=seq(10,35,1),
                        lr=c(0.001,0.01,0.1,1),
                        rho=seq(0,1,0.1),
                        decay=seq(0,1,0.1),
                        activation=c("relu","tanh"))
ctrl<-trainControl(method="cv",number=5)
require(tensorflow)
caret_mlp<-caret::train(SR~.,
                        data=Tibetapp,
                        method="mlpKerasDecay",
                        tuneGrid=param_grid,
                        epoch=30,
                        verbose=0,
                        trControl=ctrl)
prev.ResNeu<-predict(caret_mlp,newdata=Tibetvalid[,1:20])
Results$RMSEResNeu[i]<-sqrt(mean((Tibetvalid[,21]-prev.ResNeu)^2))

# install.packages("reticulate")
# install.packages("remotes")
# library("reticulate")
# remotes::install_github("rstudio/tensorflow")
# library(tensorflow)
# install_tensorflow(version = "2.0.0b1", method = "conda", envname = "r-reticulate")
# 
# remotes::install_github("rstudio/keras", dependencies = TRUE)
# library(keras)