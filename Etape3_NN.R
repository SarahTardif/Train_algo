## script pour créer et tester le modèle de classification par réseau de neurones
## pour toutes questions, contacter Gauthier Lapa, gauthier.lapa@gmail.com
## version du 2023-04-03
setwd("C:/Users/User/OneDrive - UQAM/PaqLab/Sarah/CytoR/data")
getwd()
#install.packages("caret")
#install.packages("tibble")
library(dplyr)
library(doParallel)
library(caret)
library(tibble)


## charger les données 
trainset<-read.csv('trainset_genus.csv', h=T) ## jeux de données pour entraîner le modèle
testset<-read.csv('testset_genus.csv', h=T) ## jeux de données pour tester le modèle
trainset$Class<-as.factor(trainset$Class)
testset$Class<-as.factor(testset$Class)

## entraînement du modèle
CV<-trainControl(method = "cv", number=10, savePredictions=TRUE)

# Grid of tuning parameters
nnet_grid <- expand.grid(size = c(5, 10, 15),
                         decay = c(0.001, 0.01, 0.1))

## entraînement du modèle
cl<-makePSOCKcluster(detectCores()-2)## calcul parallèle, tous les coeurs - 2
registerDoParallel(cl)
model_nn <- caret::train(Class ~ . , method = "nnet", data = trainset,
                         importance = TRUE,
                         maxit = 1000, # set high enough so to be sure that it converges
                         allowParallel = TRUE,
                         tuneGrid = nnet_grid,
                         trControl = CV)


stopCluster(cl)
registerDoSEQ()

## Enregistrer le modèle créé (pour pouvoir le réutiliser ensuite)
saveRDS(model_nn, "modelNN_genus_20231116.rds")

## tester le modèle crée
predictNN<-predict(model_nn, testset)
predictNN

cmNN<-confusionMatrix(predictNN, testset$Class)
cmNN

## matrice de confusion vers un dataframe pour l'enregistrer
matrixcm<-as.matrix(cmNN)
dataframe_data=as.data.frame(matrixcm)

dataframe_data <- tibble::rownames_to_column(dataframe_data, "Prediction")
write.csv(dataframe_data, "ConfusionMatrixNN_genus.csv", row.names = F)

## statistiques par classe, pour enregistrement
mat<-as.matrix(cmNN$byClass)
mat2<-round(mat, 4) ## garder seulement 4 décimales
dataframe_data=as.data.frame(mat2)

dataframe_data <- tibble::rownames_to_column(dataframe_data, "Prediction")
write.csv(dataframe_data, "ConfusionMatrixNN_class_genus.csv", row.names = F)

