## script pour prédire la classe de nouvelles données

setwd("C:/Users/User/OneDrive - UQAM/PaqLab/Sarah/CytoR/data")
getwd()

library(dplyr)
## charger les paquets nécessaires pour le  modèle choisi

## charger le modèle retenu
model<-readRDS("modelXGBoost_20230330.rds")

## charger les  données à classifier
data<-read.csv("testsetRFv3.csv", h=T)

## mise en forme des données
## nettoyage, pour supprimer les lignes sans valeurs (inf, NA)
completerecords <- na.omit(data) 
completerecords2 <-  completerecords %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.))) # checking only numeric columns:

## S'il y a moins d'obs. dans completerecords2 que dans training, regarder pourquoi ! 
## possible problème dans le nom d'échantillons de référence et mauvaise liaison avec names
## il ne devrait pas y avoir de NA ou de inf normalement

## il est possible d'appliquer des filtres pour supprimer les données qui ont un FSC_A et SSC_A > a une valeur seuil
# dataset<-filter(completerecords2 ,FSC_A < 'seuil' & SSC_A < 'seuil')

## prédiction
pred<-predict(model, completerecords2)

## enregistrer les prédictions
completerecords2$Class_pred<-pred

write.csv(completerecords2, "ID_sampleXX.csv", row.names = F)
