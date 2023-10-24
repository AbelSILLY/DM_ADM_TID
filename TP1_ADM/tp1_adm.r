WINE=read.csv("C:/Users/abels/OneDrive/Bureau/ADM/TP/TP1_ADM/wine.csv")
WINE
## Partie 1
# Question 1.1
W=diag(1/21, 21) # matrice des poids
I_21=rep(1, 21) #vecteur unitaire  de taille 21
Quan=WINE[,4:32] #ne donne que les variables quantitatives de WINE
Q=matrix(0, nrow=21, ncol=29) #extraction des variables quantitatives
for(j in 1:29){ 
  Q[,j]=Quan[[j]]
}
x_b=t(Q)%*%W%*%I_21
x_c=Q - I_21%*%t(x_b) # le projecteur orthogonal sur I_21
vec_norm=diag(diag(t(x_c)%*%W%*%x_c)) #normes des vecteurs``
x_cr=x_c%*%solve(sqrt(vec_norm)) #normalisation des projetes
tableau_cr=as.data.frame(x_cr) #on crée un data frame avec les données centrées réduites de WINE
#Question 1.2
bar=t(x_cr)%*%W%*%I_21
barycentre=t(bar)
print(barycentre)
inertie=sum(diag(t(x_cr)%*%W%*%x_cr))
print(inertie)
print(tableau_cr)


# Question 2
for (i in (1:21)){
  for (j in (1:29)){
    WINE[i,j+3]=tableau_cr[i,j]  # on mets dans WINE les données centrées réduite extraites avant 
  }
}
print(WINE)
colMeans(WINE[,4:32], na.rm = T)
inertie_total = sum(colMeans((WINE[1:21,4:32])^2, na.rm = TRUE))
inertie_total
Bourg=subset(WINE, WINE$Label=='Bourgueuil')
Bourg2=Bourg[,4:32]
Bourg2
B=matrix(0, nrow=6, ncol=29)
Chin=subset(WINE, WINE$Label=='Chinon')
Chin2=Chin[,4:32]
C=matrix(0, nrow=4, ncol=29)
Saum=subset(WINE, WINE$Label=='Saumur')
Saum2=Saum[,4:32]
S=matrix(0, nrow=11, ncol=29)

for(j in 1:29){ 
  B[,j]=Bourg2[[j]] ## je crée la matrice B avec les informations sur les bourgueuils
}

for(j in 1:29){ 
  C[,j]=Chin2[[j]] ## je crée la matrice C avec les informations sur les chinons
}

for(j in 1:29){ 
  S[,j]=Saum2[[j]]  ## je crée la matrice S avec les informations sur les saumurs
}

Poids_Chinon=0
Poids_Bourgueuil=0
Poids_Saumur=0
for (i in 1:21){
  if(WINE[i,2]=="Bourgueuil"){Poids_Bourgueuil = Poids_Bourgueuil+1}
  else if(WINE[i,2]=="Chinon"){Poids_Chinon = Poids_Chinon+1}
  else { Poids_Saumur = Poids_Saumur+1}
}

W_B=diag(Poids_Bourgueuil/21, Poids_Bourgueuil)
W_C=diag(Poids_Chinon/21, Poids_Chinon)
W_S=diag(Poids_Saumur/21, Poids_Saumur)
Bar_bourgueuil=colMeans(Bourg2)
Bar_Chinon=colMeans(Chin2)
Bar_Saumur=colMeans(Saum2)
Barycentre_tot=rbind(Bar_bourgueuil,Bar_Chinon,Bar_Saumur)
Barycentre_tot

## Calcul de la norme carré des barycentres : 
#Bourgueuil :
norme_eu_b=sum(Bar_bourgueuil^2)
#Chinon :
norme_eu_c=sum(Bar_Chinon^2)

#Saumur :
norme_eu_s=sum(Bar_Saumur^2)

print(c(norme_eu_b,norme_eu_c,norme_eu_s))


inertie_externe = norme_eu_b*(Poids_Bourgueuil/21) + norme_eu_c*(Poids_Chinon/21) + norme_eu_s*(Poids_Saumur/21)
print(inertie_externe)
print(inertie)
R2 = inertie_externe/inertie
print(R2)
pourecentagedisparite = R2*100
print(pourecentagedisparite)

# Question 3
R2_variable = numeric(29)
R2_variable = (Poids_Bourgueuil/21)*Bar_bourgueuil**2 + (Poids_Chinon/21)*Bar_Chinon**2 + (Poids_Saumur/21)*Bar_Saumur**2
print("La moyenne arithmétique des R2 des variables est :")
mean(R2_variable)
print("Le R2 de la partition est :")
R2

Wine2=WINE[,c(2,4:32)]
#variables qui semblent être les plus liés a l'appelation :
boxplot(Wine2$Phenolic~Wine2$Label,main="contenu phenolique", xlab="Variable de contenu phenoliques", ylab="", col='brown2')
#variables qui semblent le moins lié à l'appelation : 
boxplot(Wine2$Spice.before.shaking~Wine2$Label,main="Spice before shaking", xlab="Variable d'aspect épicé avant agitation", ylab="",col='darkred')

## Partie 2
# Question 1
X<-x_cr
print(X) #matrice des variables quantitatives centrées réduites
Label<-matrix(0,nrow=21,ncol=1)
for(j in 1:21){Label[j,]<-(WINE[j,2])}
colnames(Label)<-("Label")
print(Label)
library(ade4)
Y<-acm.disjonctif(Label)
Y<-as.matrix(Y)
print(Y) #matrice des indicatrices d'appellation
print(W) #matrice des poids des individus
M<-diag(1/dim(X)[2],dim(X)[2],dim(X)[2]) #matrice des poids des variables (équipondération)
print(M)

#Création de la matrixe PiY
PiY<-Y%*%(solve(t(Y)%*%W%*%Y))%*%t(Y)%*%W #projecteur sur Y
print(PiY)

#Création des matrice PiXj

Pi_Xj<-list() 
print(Pi_Xj)#liste de matrice qui aura pour valeur la matrice Pi_Xj à la coordonnée j
for(j in 1:29){
  Pi_Xj[[j]]<-X[,j]%*%(solve(t(X[,j])%*%W%*%X[,j]))%*%t(X[,j])%*%W
}
print(Pi_Xj)

# calcul de la trace pour chaque Xj
tr_Pi_Xj_PiY<-rep(0,29) #vecteur qui aura pour valeur la trace de PiXj*PiY en coordonnée j
for(j in 1:29){
  tr_Pi_Xj_PiY[j]<-sum(diag(Pi_Xj[[j]]%*%PiY))
}
TR_Pi_Xj_PiY=as.data.frame(t(tr_Pi_Xj_PiY))
R2_variable_bis=as.data.frame(t(R2_variable))
print(TR_Pi_Xj_PiY)
dim(TR_Pi_Xj_PiY)
dim(R2_variable_bis)
print(R2_variable_bis)
for (i in (1:29)){
  R2_variable_bis[,i]=TR_Pi_Xj_PiY[,i]
}
print(t(R2_variable_bis)) #vecteur contenant les R²(x^j|Y)=variance des x^j par appellation
# Question c)

R<-X%*%M%*%t(X)%*%W
print(sum(diag(R%*%PiY))) #Inertie interclasse 

#Question 2
Sol<-matrix(0,nrow=21,ncol=1)
for(j in 1:21){Sol[j,]<-(WINE[j,3])}
colnames(Sol)<-("Soil")
print(Sol)
Z<-acm.disjonctif(Sol)
print(Y)
Z<-data.matrix(Z)
print(Z)
PiZ<-Z%*%(solve(t(Z)%*%W%*%Z))%*%t(Z)%*%W
print(PiZ)

tr_Pi_Xj_PiZ<-rep(0,29)
for(j in 1:29){
  tr_Pi_Xj_PiZ[j]<-sum(diag(Pi_Xj[[j]]%*%PiZ))
}
print(tr_Pi_Xj_PiZ)
TR_Pi_Xj_PiZ=as.data.frame(t(tr_Pi_Xj_PiZ))
R2_variable_ter=as.data.frame(t(R2_variable))

for (i in (1:29)){
  R2_variable_ter[,i]=TR_Pi_Xj_PiZ[,i]
}
print(t(R2_variable_ter))
print(sum(diag(R%*%PiZ)))#la partition des vins par sol semble davantage expliquer les disparités entre les vins