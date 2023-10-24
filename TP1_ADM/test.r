# Import des données et précision
knitr::opts_chunk$set(echo = TRUE)
data<-read.csv('C:/Users/abels/OneDrive/Bureau/ADM/TP/TP1_ADM/wine.csv',header=TRUE)
precision<-.Machine$double.eps

# Première visualisation de données
knitr::kable(head(data[,c(2,3,10,20,25,27,28)]),caption="Extrait du tableau de données avec un arrondi à $10^{-3}$",digits=3)

# Extraction des données numériques et standardisation
standard<-function(x){(x-mean(x))/(sd(x)*sqrt(1-1/length(x)))}
X<-apply(as.matrix(data[,sapply(data,is.numeric)]),2,standard)
knitr::kable(head(X[,c(2,3,10,20,25,27,28)]),caption="Extrait du tableau de données standardisées",digits=3)

# Calcul du barycentre du nuage
un<- function(x){as.matrix(rep(1,x))}
W<-diag(1/21,21,21)
bar<-t(X)%*%W%*%un(21)
knitr::kable(bar,digits=16,caption='Coordonnées du barycentre arrondies à $10^{-6}$')

# Calcul de l'inertie
inertie<-sum(diag(t(X)%*%W%*%X))
inertie

# partition du nuage et calcul du barycentre
library(kableExtra)
Bourg<-X[data$Label=='Bourgueuil',]
Saumur<--X[data$Label=='Saumur',]
Chinon<-X[data$Label=='Chinon',]

pBourg<-dim(Bourg)[1]/21
pSaumur<-dim(Saumur)[1]/21
pChinon<-dim(Chinon)[1]/21

barBourg<-t(Bourg)%*%diag(1/dim(Bourg)[1],dim(Bourg)[1],dim(Bourg)[1])%*%un(dim(Bourg)[1])
barSaumur<-t(Saumur)%*%diag(1/dim(Saumur)[1],dim(Saumur)[1],dim(Saumur)[1])%*%un(dim(Saumur)[1])
barChinon<-t(Chinon)%*%diag(1/dim(Chinon)[1],dim(Chinon)[1],dim(Chinon)[1])%*%un(dim(Chinon)[1])

results_partition<-rbind(c(pBourg,pSaumur,pChinon),cbind(barBourg,barSaumur,barChinon))
rownames(results_partition)[1]<-'Poids'
colnames(results_partition)<-c('Bourg','Saumur','Chinon')
knitr::kable(results_partition,format='latex',booktabs=T,digits=3,caption='Poids et coordonnées du barycentre du nuage partitionné par les appellations')%>%kable_styling(latex_options=c("striped","HOLD_position"))
