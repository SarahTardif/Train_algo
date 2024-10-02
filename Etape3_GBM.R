## script pour créer et tester le modèle de classification Gradient Boosting
## pour toutes questions, contacter Gauthier Lapa, gauthier.lapa@gmail.com
## version du 2023-04-03
install.packages("gbm")
library(gbm)
library(tibble)

setwd("C:/Users/User/OneDrive - UQAM/PaqLab/Sarah/CytoR/data")
getwd()
## charger les données 
trainset<-read.csv('trainset_genus.csv', h=T) ## jeux de données pour entraîner le modèle
testset<-read.csv('testset_genus.csv', h=T) ## jeux de données pour tester le modèle
trainset$Class<-as.factor(trainset$Class)
testset$Class<-as.factor(testset$Class)

## entraînement du modèle
## (pas de calcul parallèle ici, gbm ne semble pas fonctionner avec doParallel)
cl<-makePSOCKcluster(detectCores()-2)## calcul parallèle, tous les coeurs - 2
registerDoParallel(cl)
model_gbm = gbm(Class ~.,
                data = trainset,
                cv.folds = 10,
                shrinkage = .01,
                n.minobsinnode = 10,
                n.trees = 500)  


   ## Enregistrer le modèle créé (pour pouvoir le réutiliser ensuite)
saveRDS(model_gbm, "modelGBM_genus_20231116.rds")


class_names = colnames(predictGB)[apply(predictGB, 1, which.max)]
result = data.frame(testset$Species, class_names)

print(result)
## tester le modèle crée
length(testset$Species)
length(class_names)
class_names<-as.factor(class_names)

cmGBM = confusionMatrix(class_names, testset$Species)
print(cmGBM)

## enregistrer la matrice de confusion
matrixcm<-as.matrix(cmGBM)
dataframe_data=as.data.frame(matrixcm)

dataframe_data <- tibble::rownames_to_column(dataframe_data, "Prediction")
write.csv(dataframe_data, "ConfusionMatrixGBM_genus.csv", row.names = F)

## statistiques par classe, pour enregistrement
mat<-as.matrix(cmGBM$byClass)
mat2<-round(mat, 4) ## garder seulement 4 décimales
dataframe_data=as.data.frame(mat2)

dataframe_data <- tibble::rownames_to_column(dataframe_data, "Prediction")
write.csv(dataframe_data, "ConfusionMatrixGBM_class_genus.csv", row.names = F)


## des infos sur les Gradient Boosting Machines
## http://uc-r.github.io/gbm_regression

