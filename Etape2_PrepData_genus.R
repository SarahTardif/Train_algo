
setwd("C:/Users/User/OneDrive - UQAM/PaqLab/Sarah/CytoR/data_reference/model_train_selection_fluo")
getwd()

data<-read.csv("references_pollens_all.csv", h=T)
names(data)[1]<-"Cytometry_Name_pollens"
## charger le tableau qui contient le nom des échantillons et 
names<-read.csv("Collection_Reference_Pollens.csv", sep= ";",h=T)
library(dplyr)
data2<-left_join(data, names, by="Cytometry_Name_pollens")
data2$Cytometry_Name_debris<- NULL
#data2$ID<- paste0(data2$Family,"_",data2$Genus,"_",data2$Species)

training<-dplyr::select(data2, -Cytometry_Name_pollens, -Family, -Species, -Time, -SampleID)
#training$Class<-as.factor("Pollens")
training$Class<-as.factor(training$Genus)
training$Cytometry_Name<-NULL
training$Genus<-NULL
str(training)

## ajouter les débris
deb<-read.csv("references_debris_fluo_all.csv", h=T)
deb1 <- deb[sample(nrow(deb),10000),] ## garder seulement 10000 débris
deb2<-dplyr::select(deb1, -Cytometry_Name, -Time, -SampleID)
deb2$Class<-as.factor("Debris")
str(deb2)
## combine training et deb2
training2<-rbind(training, deb2)

## nettoyage, pour supprimer les lignes sans valeurs (inf, NA)
completerecords <- na.omit(training) 
completerecords2 <-  completerecords %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.))) # checking only numeric columns:

## S'il y a moins d'obs. dans completerecords2 que dans training, regarder pourquoi ! 
## possible problème dans le nom d'échantillons de référence et mauvaise liaison avec names
## il ne devrait pas y avoir de NA ou de inf normalement

## placement aléatoire des lignes (ID)
datamod <- completerecords2[sample(nrow(completerecords2)),]


## nettoyage (garde seulement le tableau datamod dans les fichiers à droite)
rm(list=setdiff(ls(), "datamod"))

## occurrence de chaque espèce
as.data.frame(table(datamod$Class))

## table d'entraînement et de test du modèle
index     <- 1:nrow(datamod)
testindex <- sample(index, trunc(length(index)*30/100))
testset   <- datamod[testindex,]
trainset  <- datamod[-testindex,]

## sauvegarder les données de train et test !!!
write.csv(datamod, 'trainingdata_genus.csv', row.names = F) ## jeux de données complet
write.csv(trainset, 'trainset_genus.csv', row.names = F) ## jeux de données pour entraîner le modèle
write.csv(testset, 'testset_genus.csv', row.names = F) ## jeux de données pour tester le modèle

