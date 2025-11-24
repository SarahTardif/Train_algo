library(dplyr)
library(tidyr)

## charger les données 
trainset<-read.csv('./inputs_outputs/trainset_genus_V2.csv',h=T)
trainset$taxon<-trainset$Class
trainset<-dplyr::select(trainset, -Genus, -Family, -Cytometry_Name, -Class)
trainset$taxon<-as.factor(trainset$taxon)
trainset$Class<-NULL

# Sélectionner les variables d'intérêt
df_plot <- trainset %>%
  select(taxon, Violet610_A, PB450_A, FITC_A, SSC_A) %>%
  pivot_longer(cols = -taxon, names_to = "variable", values_to = "value")

library(ggplot2)

ggplot(df_plot, aes(x = taxon, y = value, fill = taxon)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Intensity (_A)", x = "Taxon")


# avec données log transformées 
df_plot_norm <- df_plot %>%
  mutate(value_log = log10(value + 1))  # +1 pour éviter log(0)

ggplot(df_plot_norm, aes(x = taxon, y = value_log, fill = taxon)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Log10(Intensity + 1)", x = "Taxon")
