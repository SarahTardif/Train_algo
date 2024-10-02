## script pour créer et tester le modèle de classification XGBoost
## pour toutes questions, contacter Gauthier Lapa, gauthier.lapa@gmail.com
## version du 2023-04-03
setwd("C:/Users/User/OneDrive - UQAM/PaqLab/Sarah/CytoR/data")
getwd()
install.packages("xgboost")
install.packages("caTools")
library(xgboost)
library(caTools)
library(caret)  
library(doParallel) ## calcul parallèle, pour aller plus vite !
library(tibble)


## charger les données 
trainset<-read.csv('trainset_genus.csv', h=T) ## jeux de données pour entraîner le modèle
testset<-read.csv('testset_genus.csv', h=T) ## jeux de données pour tester le modèle
trainset$Class<-as.factor(trainset$Class)
testset$Class<-as.factor(testset$Class)

## entraînement du modèle
cl<-makePSOCKcluster(detectCores()-2)## calcul parallèle, tous les coeurs - 2
registerDoParallel(cl)

xg.boost <- train(Class ~.,
                  data = trainset, 
                  method = "xgbTree")


stopCluster(cl)
registerDoSEQ()

## Enregistrer le modèle créé (pour pouvoir le réutiliser ensuite)
saveRDS(xg.boost, "modelXGBoost_genus_20231116.rds") ##0.9856

## tester le modèle crée

predictXB<-predict(xg.boost, testset)
predictXB

testset$Class

cmXB<-confusionMatrix(predictXB, testset$Class)
cmXB

## enregistrer la matrice de confusion
matrixcm<-as.matrix(cmXB)
dataframe_data=as.data.frame(matrixcm)

dataframe_data <- tibble::rownames_to_column(dataframe_data, "Prediction")
write.csv(dataframe_data, "ConfusionMatrixXB_genus.csv", row.names = F)

## matrice de confusion vers un dataframe pour l'enregistrer
mat<-as.matrix(cmXB$byClass)
mat2<-round(mat, 4)
dataframe_data=as.data.frame(mat2)

dataframe_data <- tibble::rownames_to_column(dataframe_data, "Prediction")
write.csv(dataframe_data, "ConfusionMatrixXB_Class_genus.csv", row.names = F)

## des indos :
## https://www.kaggle.com/code/pelkoja/visual-xgboost-tuning-with-caret
