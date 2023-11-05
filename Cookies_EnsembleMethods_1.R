url="https://perso.univ-rennes1.fr/valerie.monbet/doc/cours/Biscuits.csv"
#url = "~/Dropbox/ENSEIGNEMENT/RADO/Biscuits.csv"
biscuits=read.csv(url,sep=";")
# Extraction de la colonne fat
fat=biscuits[,1]
# Extraction des variables explicatives
X=biscuits[,-1]

# Bagging =====================================================================
library(ipred)


n = dim(X)[1]
B = 10 # number of samples for the cross validation
mse_bag = NULL
for (b in 1:B){
  itrain = sample(1:n,round(n*7/8))
  itest = setdiff(1:n,itrain)
  Xs_train = scale(X[itrain,])
  Xs_test = scale(X[itest,],center=apply(X[itrain,],2,mean),scale=apply(X[itrain,],2,sd))
  # train = data.frame(cbind(Xs_train,fat[itrain]))
  # colnames(train)[length(colnames(train))] = "y"
  # test = data.frame(cbind(Xs_test,y[itest]))
  # colnames(test)=colnames(train)
  bag = ipredbagg(fat[itrain],Xs_train,nbagg=20,control=rpart.control(maxdepth=5, minsplit=15))
  # maxdepth = maximal depth of each individual tree
  # minsplit = minimum number of observations in a node to allow a split
  mse_bag[b] = mean((fat[itest]-predict(bag,Xs_test))^2)
}

print(paste("Error of bagging:", round(sqrt(mean(mse_bag))*100)/100))


# Random Forest =====================================================================
library(randomForest)


n = dim(X)[1]
B = 10 # number of samples for the cross validation
mse_rf = NULL
for (b in 1:B){
  itrain = sample(1:n,round(n*7/8))
  itest = setdiff(1:n,itrain)
  Xs_train = scale(X[itrain,])
  Xs_test = scale(X[itest,],center=apply(X[itrain,],2,mean),scale=apply(X[itrain,],2,sd))
  train = data.frame(cbind(fat[itrain],Xs_train))
  colnames(train)[1] = "y"
  test = data.frame(cbind(fat[itest],Xs_test))
  colnames(test) = colnames(train)
  RF = randomForest(y~.,data=train,mtry=20,control=rpart.control(maxdepth=5, minsplit=15))
  # maxdepth = maximal depth of each individual tree
  # minsplit = minimum number of observations in a node to allow a split
  y_pred = predict(RF,test)
  mse_rf = mean((y_pred-fat[itest])^2)
}

print(paste("Error of random forest:", round(sqrt(mean(mse_rf))*100)/100))

