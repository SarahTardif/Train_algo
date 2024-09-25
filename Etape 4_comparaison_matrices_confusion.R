setwd("C:/Users/sarah/OneDrive - UQAM/PaqLab/Sarah/CytoR/data")
getwd()
library(tibble)


rf<-readRDS("modelRF_genus_20231116.rds")
NN<-readRDS("modelNN_genus_20231116.rds")
XGB<-readRDS("modelXGBoost_genus_20231116.rds")

matrf<-read.csv("ConfusionMatrixRF_class_genus.csv")
view(matrf)
matNN<-read.csv("ConfusionMatrixNN_class_genus.csv")
view(matNN)
matXGB<-read.csv("ConfusionMatrixXB_Class_genus.csv")
view(matXGB)

#trouver quel est le meilleur modèle en comparant les matrices de confusion

nbRF=0
nbNN=0
nbXGB=0
matrf<-matrf[,-1]
matNN<-matNN[,-1]
matXGB<-matXGB[,-1]
for (line in 1:27){
  for (col in 1:11){
    if (matrf[line,col]>=0.8){
        nbRF <- nbRF + 1
    }
    if (matXGB[line,col]>=0.8){
      nbXGB <- nbXGB + 1
    }
  }
}
nbRF
nbXGB
