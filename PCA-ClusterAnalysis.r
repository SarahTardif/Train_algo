
#test pca avec vegan base
library(vegan)
library(dplyr)
library(ggplot2)

# Charger les données
data <- read.csv("trainset_genus.csv", sep = ",", h = TRUE)

# Échantillonner 500 observations par classe
data <- data %>%
  group_by(Class) %>%
  sample_n(500)%>%
  ungroup()
# Séparer 'Class' des données numériques
class_labels <- data$Class
data_numeric <- data %>% select(-Class)

# Vérifier et convertir les colonnes en numérique si nécessaire
data_numeric[] <- lapply(data_numeric, as.numeric)

# Standardisation (centrer et réduire)
data_scaled <- decostand(data_numeric, method = "standardize")
# Réaliser l'ACP
acp <- pca(data_scaled)
#valpropres
valpropres<-eigenvals(acp)
barplot(as.numeric(valpropres), names.arg=names(valpropres))
abline(h=1)

# Obtenir les coordonnées des individus sur les axes principaux
acp_scores <- acp$CA$u

# Convertir en data frame pour ggplot
df_acp <- as.data.frame(acp_scores)
df_acp$Class <- class_labels  # Réintégrer les classes
df_centroids <- df_acp %>%
  group_by(Class) %>%
  summarise(across(starts_with("PC"), mean))

# Obtenir les coordonnées des variables sur les axes principaux
acp_scores_var <- acp$CA$v
df_var<-as.data.frame(acp_scores_var)
df_var<-df_var/20

# Calcul des pourcentages de variance expliquée par chaque composant principal
total_variance <- sum(valpropres)  # Somme totale des valeurs propres
percent <- valpropres / total_variance * 100  # Pourcentage de variance expliquée
# Créer un vecteur des labels pour les axes avec les pourcentages
axis_labels <- paste("PC", 1:25, "\n", round(percent[1:25], 1), "%", sep = "")

# Afficher le graphique ACP avec ggplot

# Afficher les points avec les labels au centre de chaque classe
# PC1 PC2
ggplot(df_acp, aes(x = PC1, y = PC2, color = Class)) +
  # geom_point(alpha = 0.7) +  # Si tu veux afficher les points, décommente cette ligne
  geom_text(data = df_centroids, 
            aes(x = PC1, y = PC2, label = Class),  # Correctement spécifié les coordonnées et le label
            size = 3, fontface = "bold", color = "black") +  # Texte en noir, taille 3 et gras
  geom_segment(data = df_var, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), # Réduire la taille des flèches
               color = "darkgray", size = 0.5) +  # Flèches en gris et largeur réduite
   geom_text(data = df_var, 
            aes(x = PC1, y = PC2, label = rownames(df_var)), 
            color = "darkblue", size = 4, fontface = "italic") +            
  theme_minimal() +
  labs(title = "Analyse en Composantes Principales (ACP) 1-2 ",
       x = axis_labels[1],
       y = axis_labels[2])  



# PC2 PC3
ggplot(df_acp, aes(x = PC2, y = PC3, color = Class)) +
  # geom_point(alpha = 0.7) +  # Si tu veux afficher les points, décommente cette ligne
  geom_text(data = df_centroids, 
            aes(x = PC2, y = PC3, label = Class),  # Correctement spécifié les coordonnées et le label
            size = 3, fontface = "bold", color = "black") +  # Texte en noir, taille 3 et gras
  geom_segment(data = df_var, 
               aes(x = 0, y = 0, xend = PC2, yend = PC3), 
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), # Réduire la taille des flèches
               color = "darkgray", size = 0.5) +  # Flèches en gris et largeur réduite
   geom_text(data = df_var, 
            aes(x = PC2, y = PC3, label = rownames(df_var)), 
            color = "darkblue", size = 4, fontface = "italic") +            
  theme_minimal() +
  labs(title = "Analyse en Composantes Principales (ACP) 2-3 ",
       x = axis_labels[2],
       y = axis_labels[3])  

# PC3 PC4
ggplot(df_acp, aes(x = PC3, y = PC4, color = Class)) +
  # geom_point(alpha = 0.7) +  # Si tu veux afficher les points, décommente cette ligne
  geom_text(data = df_centroids, 
            aes(x = PC3, y = PC4, label = Class),  # Correctement spécifié les coordonnées et le label
            size = 3, fontface = "bold", color = "black") +  # Texte en noir, taille 3 et gras
  geom_segment(data = df_var, 
               aes(x = 0, y = 0, xend = PC3, yend = PC4), 
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), # Réduire la taille des flèches
               color = "darkgray", size = 0.5) +  # Flèches en gris et largeur réduite
   geom_text(data = df_var, 
            aes(x = PC3, y = PC4, label = rownames(df_var)), 
            color = "darkblue", size = 4, fontface = "italic") +            
  theme_minimal() +
  labs(title = "Analyse en Composantes Principales (ACP) 3-4 ",
       x = axis_labels[3],
       y = axis_labels[4])   

# PC1 PC3
ggplot(df_acp, aes(x = PC1, y = PC3, color = Class)) +
  # geom_point(alpha = 0.7) +  # Si tu veux afficher les points, décommente cette ligne
  geom_text(data = df_centroids, 
            aes(x = PC1, y = PC3, label = Class),  # Correctement spécifié les coordonnées et le label
            size = 3, fontface = "bold", color = "black") +  # Texte en noir, taille 3 et gras
  geom_segment(data = df_var, 
               aes(x = 0, y = 0, xend = PC1, yend = PC3), 
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), # Réduire la taille des flèches
               color = "darkgray", size = 0.5) +  # Flèches en gris et largeur réduite
   geom_text(data = df_var, 
            aes(x = PC1, y = PC3, label = rownames(df_var)), 
            color = "darkblue", size = 4, fontface = "italic") +            
  theme_minimal() +
  labs(title = "Analyse en Composantes Principales (ACP) 1-3 ",
       x = axis_labels[1],
       y = axis_labels[3])  

# PC1 PC4
ggplot(df_acp, aes(x = PC1, y = PC4, color = Class)) +
  # geom_point(alpha = 0.7) +  # Si tu veux afficher les points, décommente cette ligne
  geom_text(data = df_centroids, 
            aes(x = PC1, y = PC4, label = Class),  # Correctement spécifié les coordonnées et le label
            size = 3, fontface = "bold", color = "black") +  # Texte en noir, taille 3 et gras
  geom_segment(data = df_var, 
               aes(x = 0, y = 0, xend = PC1, yend = PC4), 
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), # Réduire la taille des flèches
               color = "darkgray", size = 0.5) +  # Flèches en gris et largeur réduite
   geom_text(data = df_var, 
            aes(x = PC1, y = PC4, label = rownames(df_var)), 
            color = "darkblue", size = 4, fontface = "italic") +            
  theme_minimal() +
  labs(title = "Analyse en Composantes Principales (ACP) 1-4 ",
       x = axis_labels[1],
       y = axis_labels[4])  


# PC2 PC4
ggplot(df_acp, aes(x = PC2, y = PC4, color = Class)) +
  # geom_point(alpha = 0.7) +  # Si tu veux afficher les points, décommente cette ligne
  geom_text(data = df_centroids, 
            aes(x = PC2, y = PC4, label = Class),  # Correctement spécifié les coordonnées et le label
            size = 3, fontface = "bold", color = "black") +  # Texte en noir, taille 3 et gras
  geom_segment(data = df_var, 
               aes(x = 0, y = 0, xend = PC2, yend = PC4), 
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), # Réduire la taille des flèches
               color = "darkgray", size = 0.5) +  # Flèches en gris et largeur réduite
   geom_text(data = df_var, 
            aes(x = PC2, y = PC4, label = rownames(df_var)), 
            color = "darkblue", size = 4, fontface = "italic") +            
  theme_minimal() +
  labs(title = "Analyse en Composantes Principales (ACP) 2-4 ",
       x = axis_labels[2],
       y = axis_labels[4])   



#plot en 3d pour 3 axes
library(rgl)
plot3d(df_acp$PC1, df_acp$PC2, df_acp$PC3)
