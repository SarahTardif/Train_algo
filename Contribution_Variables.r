#### Contribution des variables #### 
library(randomForest)
library(datasets)
library(caret)
library(pdp)
library(performanceEstimation)
# Get variable importance measures
model<-readRDS("modelRF_species_balanced_essentials_20250110.rds")
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
