########################### Script propre ######################################
#### Réseau de neurones

library(neuralnet)

ResNeur<-rep(100,1000)                    #Ne marche pas avec NA 


for (i in 1:1000){   
  aleat<-sample(length(Tibet$A_Temp),250)
  Tibetapp<-Tibet[aleat,]
  Tibetvalid<-Tibet[-aleat,]
  n <- neuralnet(SR~.,                    #Modèle à ajuster
                 data = Tibetapp,         #Jeu de données avec variables de la formule
                 hidden = c(20,15,10,5,2),#Nombre de neurones sur la couche cachée
                 err.fct = "sse",         #Erreur estimée par la somme des carrés des erreurs absolues que l'algorithme va chercher à minimiser
                 linear.output = FALSE,   #Application linéaire aux neuronnes de sortie
                 lifesign = 'full',       #Imprimer ou non le calcul
                 rep = 2,                 #Nombre de répétition pour l'entraînement du réseau
                 algorithm = "rprop+",    #retropropagation résiliente avec retour arriere de poids
                 stepmax = 10000000)      #Etapes maximales pour la formation du reseau
  if(min(n$result.matrix[1,1]<n$result.matrix[1,2])){
    output <- compute(n, rep = 1, Tibetvalid[,-20])
  }
  if(min(n$result.matrix[1,2]<n$result.matrix[1,2])){
    output <- compute(n, rep = 2, Tibetvalid[,-20])
  }
  ResNeur[i]<-sqrt(mean((Tibetvalid[,20]-output$net.result)^2))
  if(ResNeur[i]== min(ResNeur)){
    Neu<-n
  }
#write.table(ResNeur,"ResNeuSeco.txt")
}

plot(Neu)# Pour plot du réseau de neurones le plus performant

################################################################################
##### Régressions pénalisées Lasso, Ridge et Elasticnet  

Results<-matrix(NA,nrow=1000,ncol=3, dimnames = list(1:1000,c("Resultats_Lasso","Resultats_Ridge","Resultats_elasticnet")))

for (i in 1:1000){
  
  aleat<-sample(length(Tibet$A_Temp),250)
  Tibetapp<-Tibet[aleat,]
  Tibetvalid<-Tibet[-aleat,]
  #Lasso
  lasso<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],family="gaussian",type.measure="mse")
  lasso<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],family="gaussian",lambda=exp(c(lasso$lambda))) 
  prev.lasso<-predict(lasso,newx=as.matrix(Tibetvalid[,1:20]))
  Results[i,1]<-sqrt(mean((Tibetvalid[,21]-prev.lasso)^2))
  
  #Ridge
  ridge<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0,family="gaussian",type.measure="mse")
  ridge<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0,family="gaussian",lambda=exp(seq(from=min(ridge$lambda),to=(min(ridge$lambda)+10),by=0.1)))
  prev.ridge<-predict(ridge,newx=as.matrix(Tibetvalid[,1:20]))
  Results[i,2]<-sqrt(mean((Tibetvalid[,21]-prev.ridge)^2))
  
  #Elasticnet
  elasticnet<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0.5,family="gaussian",type.measure="mse")
  elasticnet<-cv.glmnet(as.matrix(Tibetapp[,1:20]),Tibetapp[,21],alpha=0,family="gaussian",lambda=exp(elasticnet$lambda))
  prev.elasticnet<-predict(elasticnet,newx=as.matrix(Tibetvalid[,1:20]))
  Results[i,3]<-sqrt(mean((Tibetvalid[,21]-prev.elasticnet)^2))
  
}
