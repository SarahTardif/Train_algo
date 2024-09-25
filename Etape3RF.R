

## script pour créer et tester le modèle de classification Random Forest

library(caret)
library(doParallel)
library(tibble)
#library(randomForest)
library(dplyr)
library(ranger)
library(e1071)


## charger les données 
trainset<-read.csv('./trainset.csv', h=T) ## jeux de données pour entraîner le modèle
testset<-read.csv('./testset.csv', h=T) ## jeux de données pour tester le modèle
trainset<-dplyr::select(trainset, -Family, -Genus, -Cytometry_Name) 
testset<-dplyr::select(testset, -Family, -Genus, -Cytometry_Name)
trainset$species<-as.factor(trainset$species)
testset$species<-as.factor(testset$species)


## Preparer les paramètres de random forest
CV<- trainControl(method = 'CV',
                  number=10,
                  savePredictions = TRUE)

rfGrid<-expand.grid(mtry=5,splitrule="gini",min.node.size=1)


cl<-makePSOCKcluster(detectCores()-2) ## calcul parallèle, tous les coeurs - 1
registerDoParallel(cl)
## entrainement du modèle
rf<-train(species~. , data=trainset,
          method='ranger',  ## rf = random forest
          trControl=CV,
          tuneGrid=rfGrid,
          importance=TRUE,
	  na.action=na.exclude)
stopCluster(cl)
registerDoSEQ()
gc()

## enregistrer le modèle
saveRDS(rf, "./modelRF_species_20240924.rds")

## tester le modèle crée
#predicted_class_test<-predict(rf, testset)
#predicted_class_test
#cmRF<-confusionMatrix(predicted_class_test, testset$species)
#cmRF

## matrice de confusion vers un dataframe pour l'enregistrer
#matrixcm<-as.matrix(cmRF)
#dataframe_data=as.data.frame(matrixcm)

#dataframe_data <- tibble::rownames_to_column(dataframe_data, "Prediction")
#write.csv(dataframe_data, "./ConfusionMatrixRF_species.csv", row.names = F)

## statistiques par classe, pour enregistrement
#mat<-as.matrix(cmRF$byClass)
#mat2<-round(mat, 4) ## garder seulement 4 décimales
#dataframe_data=as.data.frame(mat2)

#dataframe_data <- tibble::rownames_to_column(dataframe_data, "Prediction")
#write.csv(dataframe_data, "./ConfusionMatrixRF_class_species.csv", row.names = F)

## des infos sur random forest
## https://afit-r.github.io/random_forests

## des infos pour optimiser le modèle
## https://rpubs.com/phamdinhkhanh/389752

## il  est aussi possible d'utiliser le paquet RandomForest, exemples ici :
## http://mehdikhaneboubi.free.fr/random_forest_r.html et là
## https://www.listendata.com/2014/11/random-forest-with-r.html
