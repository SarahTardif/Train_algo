
matrixNN<-read.csv("ConfusionMatrixNN_class_species.csv", sep=",", h=T)
matrixRF<-read.csv("ConfusionMatrixRF_class_species.csv", sep=",", h=T)
matrixXB<-read.csv("ConfusionMatrixXB_class_species.csv", sep=",", h=T)

matrixNN$Modele<-"NN"
matrixRF$Modele<-"RF"
matrixXB$Modele<-"XB"
all_data <- rbind(matrixNN,matrixRF)#,matrixXB)

library(ggplot2)
library(tidyr)
library(dplyr)

# comparer les F1-scores
F1NN<-select(matrixNN,Prediction,F1,Modele)
F1RF<-select(matrixRF,Prediction,F1,Modele)
F1XB<-select(matrixXB,Prediction,F1,Modele)

dataF1<- rbind(F1NN,F1RF)#,F1XB)

#graphique avec boxplot pour F1 scores chaque modele (tout taxa mélangés)
ggplot(data = F1RF, aes(x = Modele, y = F1, fill = Modele)) +
    geom_boxplot() +
    labs(x = "Model", y = "Mean of F1-score of all taxa") +
    scale_fill_grey(start = 0, end = 0.6) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

## F1 score pmodele RF pour chaque taxon
# Calculer les barres d'erreur (erreur standard)
matrixRF$se <- qt(0.975, 4-1) * sqrt(matrixRF$F1 * (1 - matrixRF$F1) / 4)
# Créer le graphique
matrixRF$species<-gsub("Class: ","",matrixRF$Prediction)
matrixRF<-subset(matrixRF,species!="Debris")
ggplot(matrixRF, aes(x = species, y = F1)) +
  geom_col(width = 0.5) +  # Utiliser geom_col() pour les barres
  labs(title = "F1 Score par Taxon avec modele RF",
       x = "Taxon",
       y = "F1 Score") +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Incliner les étiquettes de l'axe x

#heatmap
rawmatrixRF<-read.csv("ConfusionMatrixRF_species.csv", sep=",", h=T)
rawmatrixRF_long <- pivot_longer(rawmatrixRF, cols = -Prediction, names_to = "Taxon", values_to = "Nombre")
# Créer la heatmap
ggplot(data = rawmatrixRF_long, aes(Prediction, Taxon, fill = Nombre)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Classe réelle", y = "Classe prédite", title = "Heatmap de la matrice de confusion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


