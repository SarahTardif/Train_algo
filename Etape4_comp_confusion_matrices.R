
matrixNN<-read.csv("ConfusionMatrixNN_class_species.csv", sep=",", h=T)
matrixRF<-read.csv("ConfusionMatrixRF_class_balanced.csv", sep=",", h=T)
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

mean(F1RF$F1)

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

# Trier les données par ordre alphabétique du nom d'espèce
matrixRF <- matrixRF %>%
  mutate(color_group = case_when(
    F1 <= 0.5 ~ "0–0.5",
    F1 <= 0.75 ~ "0.5–0.75",
    TRUE ~ "0.75–1.0"
  )) 

# Visualisation avec réordonnancement par nom d'espèce et couleurs en fonction du F1score de l'espece
ggplot(matrixRF, aes(x = reorder(species, species), y = F1, fill = color_group)) +
  geom_col(width = 0.5) +  
  labs(title = "F1 Score par Taxon avec modèle RF",
       x = "Taxon",
       y = "F1 Score") +  
  scale_fill_manual(values = c("0–0.5" = "darkred", 
                               "0.5–0.75" = "gold", 
                               "0.75–1.0" = "darkgreen")) +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#heatmap
rawmatrixRF<-read.csv("ConfusionMatrixRF_genus_wodebris.csv", sep=",", h=T)
rawmatrixRF_long <- pivot_longer(rawmatrixRF, cols = -Prediction, names_to = "Taxon", values_to = "Nombre")

# matrice en % 
matrix <- rawmatrixRF[, -1]
rawmatrixRF_percent <- sweep(matrix, 2, colSums(matrix), FUN = "/") * 100
rawmatrixRF_percent <- cbind(Prediction = rawmatrixRF$Prediction, rawmatrixRF_percent)
rawmatrixRF_percent_long <- pivot_longer(rawmatrixRF_percent, cols = -Prediction, names_to = "Taxon", values_to = "Nombre")


matrix <- rawmatrixRF[-1,-1]
matrix_percentage_by_row <- sweep(matrix, 1, rowSums(rawmatrixRF), FUN = "/") * 100
# Ajouter une colonne pour les catégories
rawmatrixRF_percent_long$Categorie <- cut(
  rawmatrixRF_percent_long$Nombre,
  breaks = c(-Inf, 1, 10, 50, 75, 100),
  labels = c("0", "1–10", "10–50", "50–75", "75–100"),
  include.lowest = TRUE
)

#créer la heatmap
ggplot(data = rawmatrixRF_percent_long, aes(Prediction, Taxon, fill = Categorie)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(Nombre < 1, "", round(Nombre))), color = "white",size=3) +
  scale_fill_manual(
    values = c(
      "0" = "lightgrey",           
      "1–10" = "lightblue",    
      "10–50" = "darkred",    
      "50–75" = "gold",        
      "75–100" = "darkgreen"   
    )) +
  labs(x = "Actual", y = "Predicted", size=8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8),axis.text.y = element_text(size=8))