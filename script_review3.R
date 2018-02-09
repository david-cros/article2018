#################################################################################
#
#   A. NYOUMA (Univ. Yaoundé 1) / D. CROS (CIRAD) - 09/02/2018
#
#   Exemple de calcul de valeurs génétiques par le BLUP chez le palmier à huile
#
#################################################################################
  

# Definition du dossier de travail contenant les donnees   ---------------
setwd("D:/Achillesoft/Mes_publications/Script_article") 


# Chargement des paquets :   -----------------------------
library(breedR)   #paquet pour l'analyse BLUP (voir http://famuvie.github.io/breedR/ pour l'installation)
library(kinship2) #paquet pour le calcul de la matrice A a partir du pedigree


# Importation des donnees :  -----------------------
load("./oilpalm_data_BLUP_example2.RData")
attach(yield_data) # permet d'avoir directement acces aux colonnes de yield_data

# Visualisation des donnees de l'essai :
View(yield_data)

# Visualisation du plan de croisements hybrides :
table(yield_data$parent_A, yield_data$parent_B)


# Visualisation des pedigrees parentaux :
View(ped_A)
View(ped_B)

# Calcul des matrices A de parente genealogique (matrices de variance-covariance des valeurs additives)
A.mat_A <- kinship(id=ped_A$individual, momid=ped_A$mother, dadid=ped_A$father)
A.mat_B <- kinship(id=ped_B$individual, momid=ped_B$mother, dadid=ped_B$father)
# Supprime des matrices A les individus presents dans le pedigree mais pas testes dans l'essai :
A.mat_A2=A.mat_A[rownames(A.mat_A) %in% yield_data$parent_A, colnames(A.mat_A) %in% yield_data$parent_A]
A.mat_B2=A.mat_B[rownames(A.mat_B) %in% yield_data$parent_B, colnames(A.mat_B) %in% yield_data$parent_B]
# Visualisation des matrices A :
A.mat_A2
A.mat_B2


# Visualisation des matrices d'incidence des effets aleatoires du modele mixte (associant les observations aux parents correspondants) :
View(Z.mat_A)
View(Z.mat_B)



# Analyse du modele lineaire mixte avec la methodologie BLUP :    ----------
analyse<-remlf90(fixed = RENDEMENT~REP,
                 generic = list(parent_A= list(Z.mat_A, A.mat_A2),
                                parent_B= list(Z.mat_B, A.mat_B2)),
                 data=yield_data)


# Variances estimees (par REML) et BLUE des effets fixes :   ----------------
summary(analyse)

# BLUP des effets aleatoires :   -------------------
ranef(analyse)

# Residus:
residuals(analyse)

