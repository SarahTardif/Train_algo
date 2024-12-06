# Charger les bibliothèques nécessaires
library(ggplot2)
library(FactoMineR)

## charger les données 
trainset<-read.csv('./trainset_wodebris.csv', h=T) ## jeux de données pour entraîner le modèle

trainset<-dplyr::select(trainset, -Genus, -Family, -Cytometry_Name, -Class)

# Vérification des valeurs manquantes (si nécessaire)
sum(is.na(trainset))

# PCA
acp<- PCA(trainset)

#graphe valeurs propres
barplot(acp$eig[1:7,1])
abline(h=1,col="red") # axes 1,2,3,4 avec valeurs propres >1 --> intéressants
dimdesc(acp)

#plot acp sur les autres axes 
plot(acp, axes = c(1,2),choix="var")
plot(acp, axes = c(2,3),choix="var")
plot(acp, axes = c(3,4),choix="var")
plot(acp, axes = c(1,3),choix="var")
plot(acp, axes = c(1,4),choix="var")

#axe1: 45%
#axe2: 23%
#axe3: 15%
#axe4: 6%