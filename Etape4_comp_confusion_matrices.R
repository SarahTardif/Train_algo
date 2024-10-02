setwd("C:/Users/sarah/OneDrive - UQAM/PaqLab/Sarah/CytoR/data_reference/model_train_to_genus")
getwd()
matrixNN<-read.csv("ConfusionMatrixNN_class_genus.csv", sep=",", h=T)
matrixRF<-read.csv("ConfusionMatrixRF_class_genus.csv", sep=",", h=T)
matrixXB<-read.csv("ConfusionMatrixXB_class_genus.csv", sep=",", h=T)

matrixNN$Modele<-"NN"
matrixRF$Modele<-"RF"
matrixXB$Modele<-"XB"
all_data <- rbind(matrixNN,matrixRF,matrixXB)

library(ggplot2)
library(tidyr)
library(dplyr)

# comparer les F1-scores
F1NN<-select(matrixNN,Prediction,F1,Modele)
F1RF<-select(matrixRF,Prediction,F1,Modele)
F1XB<-select(matrixXB,Prediction,F1,Modele)

dataF1<- rbind(F1NN,F1RF,F1XB)

  
ggplot(data = dataF1, aes(x = Modele, y = F1, fill = Modele)) +
    geom_boxplot() +
    labs(x = "Model", y = "Mean of F1-score of all taxa") +
    scale_fill_grey(start = 0, end = 0.6) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data = dataF1, aes(x = Modele, y = F1, fill = Modele)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red", fill = "red") +
  labs(x = "Model", y = "Mean of F1-score of all taxa") +
  scale_fill_grey(start = 0, end = 0.6) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convertir les données en format long
data_long <- all_data %>%
  pivot_longer(cols = -c(Prediction,Modele), names_to = "Variable", values_to = "Value")

# Créer la heatmap
ggplot(data_long, aes(x = Variable, y = Prediction, fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_distiller(direction = -1, limits = c(0, 1), na.value = "white") +
  facet_wrap(~Modele, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# calcul précision modele RF abondances : 
matrice<-read.csv("ConfusionMatrixRF_class_species.csv",sep=",",h=T)
matrice$TOTAL_lign<-rowSums(subset(matrice,select=-Prediction))
Prediction<-matrice$Prediction
somcol<-colSums(subset(matrice,select=-Prediction))
somcol <- somcol[-which(names(somcol) == "TOTAL_lign")]
TOT <- data.frame(Species = Prediction,Total_lign = matrice$TOTAL_lign,Total_col = somcol,row.names=NULL)
TOT_long <- pivot_longer(TOT, cols = c(Total_lign, Total_col), names_to = "Variable", values_to = "Value")
ggplot(TOT_long, aes(x = Species, y = Value, fill=Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


## Matrices de confusion RF performance pour chaque taxon
# Calculer les barres d'erreur (erreur standard)
matrixRF$se <- qt(0.975, 4-1) * sqrt(matrixRF$F1 * (1 - matrixRF$F1) / 4)
# Créer le graphique

ggplot(matrixRF, aes(x = Prediction, y = F1)) +
  geom_col(width = 0.5) +  # Utiliser geom_col() pour les barres
  labs(title = "F1 Score par Taxon avec modele RF",
       x = "Taxon",
       y = "F1 Score") +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Incliner les étiquettes de l'axe x

#heatmap
rawmatrixRF<-read.csv("ConfusionMatrixRF_genus.csv", sep=",", h=T)
rawmatrixRF_long <- pivot_longer(rawmatrixRF, cols = -Prediction, names_to = "Taxon", values_to = "Nombre")
# Créer la heatmap
ggplot(data = rawmatrixRF_long, aes(Prediction, Taxon, fill = Nombre)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Classe réelle", y = "Classe prédite", title = "Heatmap de la matrice de confusion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


