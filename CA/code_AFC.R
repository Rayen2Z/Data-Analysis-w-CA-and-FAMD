#vider la mémoire
rm(list=ls())

#chargement des données "housetasks" du package factoextra
library("factoextra")  #install.packages("factoextra") pour installer le package
data(housetasks)

str(housetasks)
View(housetasks)
summary(housetasks)


#Analyse visuelle des données

#convertir les données en tant que table
dt <- as.table(as.matrix (housetasks))
#Affichage graphique
library("gplots")  #install.packages("gplots") pour installer le package
balloonplot(t (dt), main = "housetasks", xlab = "", ylab = "",label = FALSE, show.margins = FALSE)


library("graphics")
mosaicplot(t (dt), main = "housetasks", xlab = "", ylab = "",label = FALSE, show.margins = FALSE)


#fonction : calcul du profil
profil <- function(x){x/sum(x)*100}

#profil ligne : reorésente le pourcentage de contribution de chaque personne dans une activité donnée
prof.ligne <- t(apply(X = housetasks, MARGIN = 1, FUN = profil))
print(round(prof.ligne, digits = 2))

#profil colonne :  représente le poucentage des activités réalisées par chaque personne
prof.colonne <- apply(housetasks,2,profil)
print(round(prof.colonne, digits = 2))

#*******************
#AFC avec FactoMineR
#*******************

#charger le package
library(FactoMineR)

#lancer l'AFC
housetasks.afc <- CA(housetasks, ncp = 2, graph = TRUE)
print(housetasks.afc)


#quelques statistiques sur le tableau de données
print(housetasks.afc$call)

#tableau des valeurs propres et Scree plot
print(housetasks.afc$eig)     #vérification de la quantité d'informations retenue par les deux axes
plot(housetasks.afc$eig[, 1], type = "b", main = "Scree plot")

#coordonnées, contributions et cos2 - lignes
print(housetasks.afc$row)

#visualisation de la contibution des lignes pour chacun des deux axes
library("corrplot")
corrplot(housetasks.afc$row$contrib, is.corr=FALSE) 

#visualisation de la representation des lignes sur les deux axes
corrplot(housetasks.afc$row$cos2, is.corr=FALSE) 

#coordonnées, contributions et cos2 - colonnes
print(housetasks.afc$col)

#visualisation de la contibution des colonnes pour chacun des deux axes
corrplot(housetasks.afc$col$contrib, is.corr=FALSE) 

#visualisation de la representation des lignes sur les deux axes
corrplot(housetasks.afc$col$cos2, is.corr=FALSE)  


#graphique symétrique
plot(housetasks.afc)

#graphique asymétrique
fviz_ca_biplot (housetasks.afc,map = "rowprincipal", arrow = c(TRUE, TRUE), repel = TRUE)



