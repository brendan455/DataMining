source("http://www.bioconductor.org/biocLite.R")
setwd("C:/Users/brdeniau/Documents/Projet R")
RequiredfunctionsDir<-"requiredFiles/"
source(paste0(RequiredfunctionsDir,"/kmeans_cah_acp.r"))

###########################
####### Data IMPORT #######
###########################

Legumes<-read.table(paste(getwd(),"/Data/Legumes.csv",sep=""),header=T,row.names=1,sep=";")

#############################################################################
###### QUESTIONS GENERALES : lien lin�aire pour ACP mais classifie rien #####
#############################################################################

## 2 calculer les moyennes et les variances des variables ##

MeanLegumes<-colMeans(Legumes)

plot(MeanLegumes,pch=16,type="o",col="blue",ylab="Moyenne composition nutritionelle",xlab="Nutriments")

## ecart-type

sdLegumes<-apply(Legumes,2,sd)

## Variance ##

VarLegumes<-(var(Legumes)*(nrow(Legumes)-1))/nrow(Legumes) #Diagonale donne variances dans l'�chantillon

## Correlation lin�aire

corLegumes<-cor(Legumes)

## 3 Distance entre �l�ments : choix? Manhattan, euclidienne.
# R�duire car variables sont pas comparables de base.

LegumesCR<-centreduire(T = Legumes)

###########
#ACP norm�e : Distance euclidienne (cor=TRUE)
#ACP non norm�e : distance r�duite (cor=FALSE)
###########

###################
### Jeux entier ###
###################

ACPNLegumes<-ACPN(Legumes)
##Question B1
plot(ACPNLegumes) # Variance 2 premiers axes: Kaiser garder axes avec inertie sup�rieure inertie moyenne : En ACP norm�e moyenne de 1
## Question B2
ACPNLegumes$score # Coordonn�es
VP(ACPNLegumes) # Inertie
AXEVAR(Legumes,ACPNLegumes) # Matrice Correlation
CTR(ACPNLegumes,4) #Contributions � deux axes des variables
COS2TETA(ACPNLegumes,3) # Si proche de 0 : mauvais : position sur le graph
# Question B3

# AXE 1 : Noisette et Noix Vs le reste: FIBRE, PROTEINE ET ENG MAJORITAIRE DEDANS
# CTR des NOIX et NOISETTES SUP 80% � l'axe 1!!! (CTR())
COS2TETA(ACPNLegumes,3)[which(a[,1]<0.5|a[,2]<0.5),] #Valeurs inf � 0.5

#Question B4
PLAN(resacp = ACPNLegumes,i=1,j=4)

###############
### K-means ###
###############

kmeans(as.matrix(Legumes),7)

###############
### CAHWard ###
###############

CAH<-ward(Legumes)
plot(CAH)
plot(CAH$height)

##################
### Jeu entier ###
##################

wardLegumes<-ward(Legumes)
plot(wardLegumes)
Cah<-couper(wardLegumes,2) # Matrice distance ward et nb classes
Max_Classe<-getCount(Cah)
Max_Classe[which(Max_Classe$count %in% max(Max_Classe$count)),]
rap_inertie(wardLegumes,2)
cdgcl1(Legumes,wardLegumes,2) # Centre gravit�
ctrcl(Legumes,wardLegumes,2) # Contributions relatives
ctrng(Legumes,wardLegumes,2) # Contribution CTR
rho2(Legumes,wardLegumes,2) # Carr� des distances
iintra(Legumes,wardLegumes,2,1) # Inertie intraclasse
