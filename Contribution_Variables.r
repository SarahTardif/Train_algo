# Charger les bibliothèques nécessaires
library(ggplot2)
library(FactoMineR)

## charger les données 
trainset<-read.csv('./trainset_balanced.csv', h=T) ## jeux de données pour entraîner le modèle

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


#### Contribution des variables #### 
library(randomForest)
library(datasets)
library(caret)
library(pdp)
library(performanceEstimation)
# Get variable importance measures
model<-readRDS("modelRF_species_balanced_20250106.rds")
modrf<-model$finalModel
imp = varImpPlot(modrf)
# Normalisation en pourcentage
imp <- as.data.frame(imp)
imp$var <- rownames(imp)
imp$MeanDecreaseAccuracy_Normalized <- (imp$MeanDecreaseAccuracy - min(imp$MeanDecreaseAccuracy)) / 
                                        (max(imp$MeanDecreaseAccuracy) - min(imp$MeanDecreaseAccuracy)) * 100

imp$MeanDecreaseGini_Normalized <- (imp$MeanDecreaseGini - min(imp$MeanDecreaseGini)) / 
                                    (max(imp$MeanDecreaseGini) - min(imp$MeanDecreaseGini)) * 100
# Création du premier boxplot pour MeanDecreaseAccuracy
ggplot(imp, aes(x =var, y = MeanDecreaseAccuracy_Normalized, fill = var)) +
  geom_point(size=4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=30),
    axis.text.y = element_text(size=30),
    legend.position = "none")
# Création du deuxième boxplot pour MeanDecreaseGini
ggplot(imp, aes(x =var, y = MeanDecreaseGini_Normalized, fill = var)) +
  geom_point(size=4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=30), 
    axis.text.y = element_text(size=30),
    legend.position = "none")


### détails noeud/architecture modele ###
library(randomForestExplainer)
explain_forest(modrf)
