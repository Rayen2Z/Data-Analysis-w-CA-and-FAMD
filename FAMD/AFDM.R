#vider la mémoire
rm(list=ls())

#installation des packages si nécessaire
install.packages(c("FactoMineR", "factoextra"))

#chargement des librairies
library("FactoMineR")
library("factoextra")

#chargement des données 
data <- read.csv(file.choose(),header = T, sep = ",",stringsAsFactors = T,row.names=1)
head(data)
str(data)

print(summary(data))


#fonction pour centrage-réduction
CR <- function(x){
  n <- length(x)
  m <- mean(x)
  v <- (n-1)/n*var(x) 
  return((x-m)/sqrt(v))
}

#appliquer la fonction sur les variables continues 3:11
data.cont <- data.frame(lapply(subset(data,select=3:11),CR)) 
print(data.cont)
summary(data.cont)

#codage disjonctif complet
library(ade4)
data.disc <- acm.disjonctif(subset(data,select=1:2))
View(data.disc)
#fonction pour pondération des indicatrices
PF <- function(x){ 
  m <- mean(x) 
  return(x/sqrt(m))
}

#appliquer la pondération sur les indicatrices
data.disc.pond <- data.frame(lapply(data.disc,PF))
print(data.disc.pond)

#données transformées envoyées à l'ACP
data.pour.acp <- cbind(data.cont,data.disc.pond) 
rownames(data.pour.acp) <- rownames(data) 
print(round(data.pour.acp,3))
View((data.pour.acp))

#application de l'ACP
res.pca <- PCA(data.pour.acp, graph = FALSE)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

df_sans_nationalite <- subset(data.pour.acp, select = c(1:9, 29:30)) #on exclut les variables des nationnalités
res.pca.sn <- PCA(df_sans_nationalite, graph = FALSE)

# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca.sn, choice = "var", axes = 1:2)


# variables PCA en fonction du cos2: qualité de représentation
fviz_pca_var(res.pca.sn, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

# individus PCA en fonction du cos2: qualité de représentation
fviz_pca_ind (res.pca.sn, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)





#acp avec le package ade4
library(ade4)
acp.data <- dudi.pca(data.pour.acp,center=T,scale=F,scannf=F)


#valeurs propres
print(round(acp.data$eig,5))

#coordonnées ACP des variables : Gkh
#pour les quanti -> corrélations avec les facteurs 
print(acp.data$co[,1:2])


#**** pour les quali -> calculs supplémentaires nécessaires **** 
#récupérer coord. acp des modalités
moda <- acp.data$co[10:30,1:2]
print(moda)

#fréquence des modalités
freq.moda <- colMeans(data.disc)
print(freq.moda)

#calcul des moyennes conditionnelles sur les 2 premiers facteurs
coord.moda <- moda[,1]*sqrt(acp.data$eig[1]/freq.moda)
coord.moda <- cbind(coord.moda,moda[,2]*sqrt(acp.data$eig[2]/freq.moda)) 
print(coord.moda)

#coordonnées des individus
print(round(acp.data$li[,1:2],5))

#carré des corrélations 1er facteur
r2 <- acp.data$co[1:9,1]^2
print(r2)

#carré du rapport de corrélation, var. qualitatives
eta2 <- NULL
eta2[1] <- sum(acp.data$co[10:28,1]^2) 
eta2[2] <- sum(acp.data$co[29:30,1]^2)

#valeurs à sommer
criteres <- c(r2,eta2) 
names(criteres) <- colnames(data) 
print(criteres)

#critère de l’AFDM – 1er facteur
lambda1 <- sum(criteres)
print(lambda1)

#confrontation avec résultat (v.p.) de l’ACP 
#sur variables transformées – 1er facteur 
print(acp.data$eig[1])


##############################  AFDM avec FactoMineR ############
#chargement du package
library(FactoMineR)
#lancement de la procédure 
res.famd <- FAMD(data) 
#affichage des résultats 
print(summary(res.famd))

#variance des dimensions
fviz_screeplot(res.famd)


######  Corrélation avec les variables ############
# Graphique des variables
fviz_famd_var (res.famd, repel = TRUE)
# Contribution à la première dimension
fviz_contrib (res.famd, "var", axes = 1)
# Contribution à la deuxième dimension
fviz_contrib (res.famd, "var", axes = 2)


#variables quantitatives
quanti.var <- get_famd_var(res.famd, "quanti.var")

fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
              col.var = "black")



#variables qualitatives
quali.var <- get_famd_var(res.famd, "quali.var")

fviz_famd_var(res.famd, "quali.var", repel = TRUE,
              col.var = "black")


#individus
ind <- get_famd_ind(res.famd)

fviz_famd_ind(res.famd, col.ind = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)


##############################  AFDM avec ade4 ###################
#chargement du package
library(ade4)
#lancement de la procédure
dudi.foot <- dudi.mix(data, scannf=F, nf=2)
#affichage des v.p.
print(round(dudi.foot$eig,5))
#coordonnées factorielles var./moda.
print(round(dudi.foot$co,5))
#graphique des variables/modalités
scatter(dudi.foot,posieig="top",clab.row=0)

##############################  AFDM avec  PCAmixdata ###########
#chargement de la librairie
library(PCAmixdata)
#lancement de la procédure
pcamix.foot <- PCAmix(data[3:11],data[1:2],ndim=2,graph=T) 
#valeurs propres
print(round(pcamix.foot$eig,5))
#corrélations
print(round(pcamix.foot$quanti.cor,5))
#coord. des modalitésdudi.mix de ADE4 
print(round(pcamix.foot$categ.coord,5))

#rotation varimax
rot.foot <- PCArot(pcamix.foot,dim=2,graph=T) 
#valeurs propres
print(round(rot.foot$eig,5))
#corrélations 
print(round(rot.foot$quanti.cor,5))
#coord. modalités
print(round(rot.foot$categ.coord,5))








