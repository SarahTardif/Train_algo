## Script pour convertir les fichiers FCS du cytomètre en fichier CSV
## adapté du scipt de Thomas Ashhurst
## https://github.com/sydneycytometry/CSV-to-FCS/blob/master/FCS-to-CSV%20v2.0.R
## pour toutes questions, contacter Gauthier Lapa, gauthier.lapa@gmail.com
## version du 2023-04-03

#####
## Certains paquets nécessitent l'istallation de BiocManager, les installer ainsi :
#install.packages("BiocManager")
#BiocManager::install("flowCore") 
#BiocManager::install("Biobase")


# Charger les paquets 
library('flowCore')
library('Biobase')
library('data.table')

# Choisir le répertoire de travail
setwd("C:/Users/sarah/OneDrive - UQAM/PaqLab/Sarah/CytoR/comp_mi_cy_mo/comp_mi_cy_mo_FCS") # /!/ vérifier que les fichiers (échantillons) ont le bon nom, les renommer dès maintenant si besoin
getwd() ## check
PrimaryDirectory <- getwd()
PrimaryDirectory ## re check

## Récupère les noms des fichiers .fcs dans le répertoire de travail
FileNames <- list.files(path=PrimaryDirectory, pattern = ".fcs")     # fichier fsc dans une liste
as.matrix(FileNames) # en matrice
#testcommitvsuju
## lire des données des fichiers dans un dataframe
DataList=list() # crée une liste vide

for (File in FileNames) { # boucle pour lire les fichiers dans la liste
  fcsfile <- read.FCS(File, transformation = FALSE,truncate_max_range = FALSE)
  tempdata <- exprs(fcsfile)
  colnames(tempdata)<-fcsfile@parameters@data$desc ## récupère les noms des variables
  tempdata <- tempdata[1:nrow(tempdata),1:ncol(tempdata)]
  File <- gsub(".fcs", "", File)
  DataList[[File]] <- tempdata
}
rm(tempdata)
rm(fcsfile)
AllSampleNames <- names(DataList)

## Check les données
head(DataList)



##### END USER INPUT #####
# pour créer un sous dossier pour les fichier csv, nom au format "Output_FCS-to-CSV %Y-%m-%d-%H:%M:%S"
x <- Sys.time()
x <- gsub(":", "-", x)
x <- gsub(" ", "_", x)

newdir <- "C:/Users/sarah/OneDrive - UQAM/PaqLab/Sarah/CytoR/comp_mi_cy_mo/comp_mi_cy_mo_CSV" # peut être remplacé par newdir<-"FolderName"

setwd(PrimaryDirectory)
dir.create(paste0(newdir), showWarnings = FALSE) #  crée un sous dossier, avec le nom de newdir
setwd(newdir)

# pour extraire les fichiers dans un dossier existant, utiliser : setwd("/an/existing/folder/")
# dans tous les cas, faire attention de ne pas écraser des fichiers préexistants
for(i in c(1:length(AllSampleNames))){
  data_subset <- DataList[i][[1]]
  data_subset <- as.data.frame(data_subset)
  colnames(data_subset) = gsub("-", "_", colnames(data_subset)) ## remplace - par _ dans le nom des colonnes (parce que R n'aime pas les -)
  a <- names(DataList)[i]
  data_subset$SampleID <- seq.int(nrow(data_subset)) ## ajout le nom de l'échantillon comme ID
  write.csv(data_subset, paste0(a, ".csv"), row.names = FALSE)
}


