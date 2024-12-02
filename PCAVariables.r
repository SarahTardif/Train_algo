# Charger les bibliothèques nécessaires
library(ggplot2)
library(caret)

## charger les données 
trainset<-read.csv('./trainset_wodebris.csv', h=T) ## jeux de données pour entraîner le modèle

trainset<-dplyr::select(trainset, -Genus, -Family, -Cytometry_Name, -Class)


# Vérification des valeurs manquantes (si nécessaire)
sum(is.na(trainset))

# Appliquer la PCA
pca_result <- pca(trainset)

# Résumé de la PCA : Variance expliquée par chaque composante
biplot(pca_result)#,xlim=c(-1,1),ylim=c(-1,1))
text(acp_result$axes[1, ], acp_result$axes[2, ], 
     labels = colnames(data), pos = 4, cex = 0.8)


