library(FactoMineR)
library (factoextra)
library(corrplot)
library(plotly)
library(Factoshiny)

res<-PCAshiny(X)
print.PCAshiny(res)

####### ACP GENERALE ######
corrplot(PCAX$var$cos2)
corrplot(PCAX$ind$cos2)

#### AXES 1 ET 2 ####
plot.PCA(PCAX,axes = c(1,2),choix = 'var')
plot.PCA(PCAX,axes = c(1,2),choix = 'ind')
fig1<-fviz_cos2(PCAX,choice = "var",axes=c(1,2),title='COS2 ACP NORMEE AXES 1&2')
fig1
fviz_cos2(PCAX,choice = "ind",axes=c(1,2))

#### AXES 2 ET 3 ####
plot.PCA(PCAX,axes = c(2,3),choix = 'var')
fviz_cos2(PCAX,choice = "var",axes=c(2,3),title="COS2 VARIABLES ACP DE RANG AXE 3&4")
#Température, Soleil, Pluie, Ensemble de variables de culture
plot.PCA(PCAX,axes = c(2,3),choix = 'ind')
fviz_cos2(PCAX,choice = "var",axes=c(2,3))
fviz_cos2(PCAX,choice = "ind",axes=c(2,3))

#### AXES 1 ET 3 ####
plot.PCA(PCAX,axes = c(1,3),choix = 'var')
plot.PCA(PCAX,axes = c(1,3),choix = 'ind')
fviz_cos2(PCAX,choice = "var",axes=c(1,3))
fviz_cos2(PCAX,choice = "ind",axes=c(1,3))

#### AXES 3 ET 4 ####
plot.PCA(PCAX,axes = c(3,4),choix = 'var')
plot.PCA(PCAX,axes = c(3,4),choix = 'ind')
fviz_cos2(PCAX,choice = "var",axes=c(3,4))
fviz_cos2(PCAX,choice = "ind",axes=c(3,4))

#### AXES 4 ET 5 ####
plot.PCA(PCAX,axes = c(4,5),choix = 'var')
plot.PCA(PCAX,axes = c(4,5),choix = 'ind')
fviz_cos2(PCAX,choice = "var",axes=c(4,5))
fviz_cos2(PCAX,choice = "ind",axes=c(4,5))

#### AXES 1 ET 4 ####
plot.PCA(PCAX,axes = c(1,4),choix = 'var')
plot.PCA(PCAX,axes = c(1,4),choix = 'ind')
fviz_cos2(PCAX,choice = "var",axes=c(1,4))
fviz_cos2(PCAX,choice = "ind",axes=c(1,4))

#### AXES 2 ET 4 ####
plot.PCA(PCAX,axes = c(2,4),choix = 'var')
plot.PCA(PCAX,axes = c(2,4),choix = 'ind')
fviz_cos2(PCAX,choice = "var",axes=c(2,4))
fviz_cos2(PCAX,choice = "ind",axes=c(2,4))

#### AXES 1 ET 5 ####
plot.PCA(PCAX,axes = c(1,5),choix = 'var')
plot.PCA(PCAX,axes = c(1,5),choix = 'ind')
fviz_cos2(PCAX,choice = "var",axes=c(1,5))
fviz_cos2(PCAX,choice = "ind",axes=c(1,5))

#### AXES 2 ET 5 ####
plot.PCA(PCAX,axes = c(2,5),choix = 'var')
plot.PCA(PCAX,axes = c(2,5),choix = 'ind')
fviz_cos2(PCAX,choice = "var",axes=c(2,5))
fviz_cos2(PCAX,choice = "ind",axes=c(2,5))

#### AXES 3 ET 5 ####
plot.PCA(PCAX,axes = c(3,5),choix = 'var')
plot.PCA(PCAX,axes = c(3,5),choix = 'ind')
fviz_cos2(PCAX,choice = "var",axes=c(3,5))
fviz_cos2(PCAX,choice = "ind",axes=c(3,5))



#### ACP DE RANG ####
corrplot(PCAXrang$var$cos2)

### AXES 1 2 ###
plot.PCA(PCAXrang,axes=c(1,2),choix='var',title = 'ACP de rang')
# Axe 1 toujours très lié à l'économie, corrélation "Attirance&SalaireAnnuel".
#COS2: 60% des infos de AttiranceGlobale et SalaireAnnuel est conservée sur l'axe 1.
# Axe 2: encore nature mais également certaines variables de Risques
plot.PCA(PCAXrang,axes=c(1,2),choix='ind',title = 'ACP de rang')
fviz_pca_biplot(PCAXrang,axes = c(1,2))
# En bas les villes du sud et à droite les villes les plus actives ou de population riche
# Ouest parisien encore regroupé mais plus isolé, NeuillySurSeine n'est plus la ville la plus à droite
# Axe 2:  encore opposition entre Antibes et Lille, on semble retrouver l'opposition villes du sud/villes
# du nord
# Axe 1: étendue Beziers Versailles
# L'ouest Parisien n'est plus isolé
fig2<-fviz_cos2(PCAXrang,choice = "var",axes=c(1,2),title="COS2 VARIABLES ACP DE RANG AXE 1&2")
fig2
fviz_contrib(PCAXrang,choice = 'var',axes = c(1,2))
fig<-subplot(fig1,fig2)
fig
subplot(fviz_contrib(PCAXrang,choice = 'ind',axes = c(1,2)),
fviz_cos2(PCAXrang,choice = 'ind',axes = c(1,2)))


### AXES 2 3 ###
plot.PCA(PCAXrang,axes=c(2,3),choix='var',title = 'ACP de rang axes 2&3')
#Température Soleil, MortaliteGlobale, MortaliteAlcool, Propriétaire, ~Musees
fviz_cos2(PCAXrang,choice = "var",axes=c(2,3),title="COS2 VARIABLES ACP DE RANG AXE 2&3")
fviz_contrib(PCAXrang,choice = 'var',axes = c(2,3))
subplot(fviz_cos2(PCAXrang,choice = "var",axes=c(2,3),title="COS2 VARIABLES ACP DE RANG AXE 2&3"),
        fviz_contrib(PCAXrang,choice = 'var',axes = c(2,3)))
plot.PCA(PCAXrang,axes=c(2,3),choix='ind',title = 'ACP de rang')
# Paris n'est plus du tout isolé, elle devient même une ville assez moyenne (pour les axes 2 et 3)
fig<-subplot(fig3,fig4)
fig
subplot(fviz_contrib(PCAXrang,choice = 'ind',axes = c(2,3)),
fviz_cos2(PCAXrang,choice = 'ind',axes = c(2,3)))

subplot(plot.PCA(PCAX,axes = c(2,3),choix = 'ind'),plot.PCA(PCAXrang,axes=c(2,3),choix='ind',title = 'ACP de rang'))
fviz_pca_biplot(PCAXrang,axes = c(2,3))


### AXES 3 4 ###
plot.PCA(PCAXrang,axes=c(3,4),choix='var',title = 'ACP de rang')
fviz_cos2(PCAXrang,choice = "var",axes=c(3,4),title="COS2 VARIABLES ACP DE RANG AXE 3&4")
# MarcheAPied se projette assez bien.
plot.PCA(PCAXrang,axes=c(3,4),choix='ind',title = 'ACP de rang')
fviz_pca_biplot(PCAXrang,axes = c(3,4))
# Vichy est isolée le long de l'axe 4

# On semble retrouver les mêmes résultats avec l'ACP de rang, on note que pour l'axe 3, la criminalité est
# moins bien représentée.
# L'ACP de rang a équilibré les résultats, on retrouve globalement les mêmes variables sur les mêmes axes
# mais on a moins de petit groupe de ville, le nuage d'individus est plus étendu.

### AXES 1 4 ###
plot.PCA(PCAXrang,axes=c(1,4),choix='var',title = 'ACP de rang')
fviz_cos2(PCAXrang,choice = "var",axes=c(1,4),title="COS2 VARIABLES ACP DE RANG AXE 1&4")
plot.PCA(PCAXrang,axes=c(1,4),choix='ind',title = 'ACP de rang')
fviz_pca_biplot(PCAXrang,axes = c(1,4))

### AXES 2 4 ###
plot.PCA(PCAXrang,axes=c(2,4),choix='var',title = 'ACP de rang')
# Corrélation: Vieillissement Presse, Soleil Mortalite (negativement), MarcheAPied Soleil presque décorrélés
fviz_cos2(PCAXrang,choice = "var",axes=c(2,4),title="COS2 VARIABLES ACP DE RANG AXE 2&4")
# Soleil, Vieillissement, MortaliteAlcool, Température, Presse, MortaliteGlobale, MarcheAPied
plot.PCA(PCAXrang,axes=c(2,4),choix='ind',title = 'ACP de rang')
# Vichy isolé, Dunkerque et Lille bien représenté
fviz_cos2(PCAXrang,choice = "ind",axes=c(2,4),title="COS2 INDIVIDUS ACP DE RANG AXE 2&4")
# Lille Vichy Dunkerque
# Troyes, Périgueux, Belfort, Antibes
fviz_pca_biplot(PCAXrang,axes = c(2,4))

subplot(fviz_cos2(PCAXrang,choice = "var",axes=c(2,4),title="COS2 VARIABLES ACP DE RANG AXE 2&4"),
        fviz_contrib(PCAXrang,choice = 'var',axes = c(2,4)))

####### ACP ECONOMIE #######
corrplot(PCAE$var$cos2)
### AXES 1 2 ###
plot.PCA(PCAE,axes = c(1,2),choix = 'var',title = 'ACP normée thème économie axe 1&2')
fviz_cos2(PCAE,choice='var',axes=c(1,2))
#ImpotFortune, ImpotRevenu, SalaireAnnuel,MetreCarreAncien, AttiranceGlobale, AttiranceActifs
# très bien représentés
plot.PCA(PCAE,axes = c(1,2),choix = 'ind',title = 'ACP thème économie axe 1&2')
fviz_pca_biplot(PCAE,axes = c(1,2))
# On retrouve Neuilly isolée sur les 2 axes et l'ouest parisien (dans une moindre mesure) sur l'axe 1.

### AXES 2 3 ### 
plot.PCA(PCAE,axes = c(2,3),choix = 'var')
fviz_cos2(PCAE,choice = 'var',axes = c(2,3))
# EvolDemographique, Propriétaire, LogtVacant
fviz_cos2(PCAE,choice = 'ind',axes = c(2,3))
plot.PCA(PCAE,axes = c(2,3),choix = 'ind')
fviz_pca_biplot(PCAE,axes = c(2,3))
#Neuilly encore isolée

### AXES 3 4 ###
fviz_cos2(PCAE,choice='var',axes=c(3,4))
plot.PCA(PCAE,axes = c(3,4),choix = 'var')
# Vieillissement, LogtVacant
plot.PCA(PCAE,axes = c(3,4),choix = 'ind')
fviz_pca_biplot(PCAE,axes = c(3,4))
#Vichy isolé, Calais, SaintDenis CorbeilEssonnes isolées

####### ACP DE RANG ECONOMIE #######
corrplot(PCAERang$var$cos2)
Erangshiny=PCAshiny(PCAERang)
fviz_eig(PCAERang, addlabels = TRUE, ylim = c(0, 20))

# cos2 élevé Dim1: Imposables, ImpotRevenu, AttiranceGlobale, SalaireAnnuel, AttiranceActifs, Chomage, 
# ChomageLong, MetreCarreAncien, ImpotFortune
# cos2 élevé Dim2: LogtVacant, EmploiCommune, Propriétaire, Chomage
# cos2 élevé Dim3: Vieillissement, DefailEntreprise
# cos2 élevé Dim4: EvolDemographique, Vieillissement
### AXES 1&2 ###
fviz_cos2(PCAERang,choice='var',axes=c(1,2))
plot.PCA(PCAERang,axes = c(1,2),choix = 'var')
# Imposable, AttiranceGlobale, AttiranceActifs, SalaireAnnuel, Imposables, Chomage, ChomageLong
# Chomage, ChomageLong anticorrélés avec Imposables, SalaireAnnuel ImpotRevenu
plot.PCA(PCAERang,axes = c(1,2),choix = 'ind')
fviz_pca_ind(PCAERang,col.ind = 'cos2',axes=c(1,2))
fviz_pca_ind(PCAE,col.ind = 'cos2',axes=c(1,2))

fviz_pca_biplot(PCAERang,axes = c(1,2))

### AXES 2&3 ###
fviz_cos2(PCAERang,choice='var',axes=c(2,3))
plot.PCA(PCAERang,axes = c(2,3),choix = 'var')
fviz_pca_var(PCAERang,col.var = 'cos2',axes = c(2,3))
subplot(fviz_pca_var(PCAERang,col.var = 'cos2',axes = c(2,3)),
fviz_pca_var(PCAERang,col.var = 'contrib',axes = c(2,3)))
subplot(fviz_pca_ind(PCAERang,col.ind = 'cos2',axes = c(2,3)),
fviz_pca_ind(PCAERang,col.ind = 'contrib',axes = c(2,3)))

# LogtVacant, DefailEntreprise, Vieillissement
plot.PCA(PCAERang,axes = c(2,3),choix = 'ind')
fviz_pca_biplot(PCAERang,axes = c(2,3))
### AXES 3&4 ###
fviz_cos2(PCAERang,choice='var',axes=c(3,4))
plot.PCA(PCAERang,axes = c(3,4),choix = 'var')
# Vieillissement très bien représenté sur ce plan
plot.PCA(PCAERang,axes = c(3,4),choix = 'ind')
fviz_pca_ind(PCAERang,col.ind = 'cos2',axes = c(2,3))
fviz_pca_biplot(PCAERang,axes = c(3,4))

### AXES 1&3 ###
fviz_pca_var(PCAERang,col.var = 'cos2',axes = c(1,3))

fviz_cos2(PCAERang,choice='var',axes=c(1,3))
plot.PCA(PCAERang,axes = c(1,3),choix = 'var')
# Faisceau Impot/Salaire (corrélés) et Chomage
plot.PCA(PCAERang,axes = c(1,3),choix = 'ind')
fviz_pca_biplot(PCAERang,axes = c(1,3))
# villes riches à droite

### AXES 2&4 ###
fviz_cos2(PCAERang,choice='var',axes=c(2,4))
plot.PCA(PCAERang,axes = c(2,4),choix = 'var')
plot.PCA(PCAERang,axes = c(2,4),choix = 'ind')
fviz_pca_biplot(PCAERang,axes = c(2,4))

### AXES 3&4 ###
fviz_contrib(PCAERang,choice = 'var',axes = c(3,4))
fviz_cos2(PCAERang,choice='var',axes=c(3,4))
fviz_pca_var(PCAERang,col.var = 'cos2',axes = c(3,4))
# Vieillissement seule variable bien représentée, seule qui contribue aux axes 3 et 4
fviz_pca_ind(PCAERang,col.ind = 'cos2',axes = c(3,4))
# Corbeil Essonnes, Belfort, Montpellier, Nantes
fviz_pca_ind(PCAERang,col.ind = 'contrib',axes = c(3,4))

plot.PCA(PCAERang,axes = c(3,4),choix = 'var')
plot.PCA(PCAERang,axes = c(3,4),choix = 'ind')
fviz_pca_biplot(PCAERang,axes = c(3,4))



### AXES 1&4 ###
fviz_cos2(PCAERang,choice='var',axes=c(1,4))
fviz_pca_var(PCAERang,col.var = 'cos2',axes = c(1,4))
plot.PCA(PCAERang,axes = c(1,4),choix = 'var')
plot.PCA(PCAERang,axes = c(1,4),choix = 'ind')
fviz_pca_biplot(PCAERang,axes = c(1,4))

####### ACP RISQUES #######
### AXES 1 2 ###
fviz_cos2(PCAR,choice = 'var',axes = c(1,2))
plot.PCA(PCAR,axes = c(1,2),choix = 'var')
# MortalitéGlobale, Mortalité Alcool
plot.PCA(PCAR,axes = c(1,2),choix = 'ind')
fviz_pca_biplot(PCAR,axes = c(1,2))
# Axe 1: Avignon Valenciennes opposées
# Axe 2: NeuillySurSeine Lille opposées

### AXES 2 3 ###
fviz_cos2(PCAR,choice = 'var',axes = c(2,3))
plot.PCA(PCAR,axes = c(2,3),choix = 'var')
# Retard6ème
plot.PCA(PCAR,axes = c(2,3),choix = 'ind')
# Groupe Strasbourg Bordeaux Marseille Antibes Toulouse Lyon


### AXES 1 3 ###
fviz_cos2(PCAR,choice = 'var',axes = c(1,3))
plot.PCA(PCAR,axes = c(1,3),choix = 'var')
# MortaliteAlcool est bien moins bien représenté
plot.PCA(PCAR,axes = c(1,3),choix = 'ind')
fviz_pca_biplot(PCAR,axes = c(1,3))


####### ACP DE RANG RISQUES #######
corrplot(PCARRang$var$cos2)
fviz_eig(PCARRang, addlabels = TRUE, ylim = c(0, 20))


### AXES 1&2 ###
fviz_cos2(PCARRang,choice = 'var',axes = c(1,2))
plot.PCA(PCARRang,axes = c(1,2),choix = 'var')
fviz_pca_var(PCARRang,col.var = 'cos2',axes = c(1,2))
fviz_pca_ind(PCARRang,col.ind = 'cos2',axes = c(1,2))
# MortaliteGlobale, MortaliteAlcool (pos. corrélés), Ensemble de variables de retard scolaire
plot.PCA(PCARRang,axes = c(1,2),choix = 'ind')
fviz_pca_biplot(PCARRang,axes = c(1,2))

subplot(fviz_pca_var(PCARRang,col.var = 'cos2',axes = c(1,2)),
        fviz_pca_var(PCAR,col.var = 'cos2',axes = c(1,2)))

### AXES 2&3 ###
fviz_pca_var(PCARRang,col.var = 'cos2',axes = c(2,3))
fviz_pca_ind(PCARRang,col.ind = 'cos2',axes = c(2,3))
fviz_cos2(PCARRang,choice = 'var',axes = c(2,3))
plot.PCA(PCARRang,axes = c(2,3),choix = 'var')
plot.PCA(PCARRang,axes = c(2,3),choix = 'ind')
fviz_pca_biplot(PCARRang,axes = c(2,3))

### AXES 1&3 ###
fviz_pca_var(PCARRang,col.var = 'cos2',axes = c(1,3))
fviz_pca_ind(PCARRang,col.ind = 'cos2',axes = c(1,3))
fviz_cos2(PCARRang,choice = 'ind',axes = c(1,3))
fviz_cos2(PCARRang,choice = 'var',axes = c(1,3))
plot.PCA(PCARRang,axes = c(1,3),choix = 'var')
# MortaliteGlobale, TerrainsPollues
plot.PCA(PCARRang,axes = c(1,3),choix = 'ind')
fviz_pca_biplot(PCARRang,axes = c(1,3))

subplot(fviz_pca_var(PCARRang,col.var = 'cos2',axes = c(1,3)),
        fviz_pca_var(PCAR,col.var = 'cos2',axes = c(1,3)))

corrplot(PCARRang$var$cos2)
corrplot(PCAR$var$cos2)

### COMPARAISON AXES 1&2 ###

subplot(fviz_pca_var(PCARRang,col.var = 'cos2',axes = c(1,2)),
        fviz_pca_var(PCAR,col.var = 'cos2',axes = c(1,2)))
# globalement pareil
subplot(fviz_pca_ind(PCARRang,col.ind = 'cos2',axes = c(1,2)),
        fviz_pca_ind(PCAR,col.ind = 'cos2',axes = c(1,2)))
# globalement parei
### COMPARAISON AXES 2&3 ###
subplot(fviz_pca_var(PCARRang,col.var = 'cos2',axes = c(2,3)),
        fviz_pca_var(PCAR,col.var = 'cos2',axes = c(2,3)))
# globalement pareil
subplot(fviz_pca_ind(PCARRang,col.ind = 'cos2',axes = c(2,3)),
        fviz_pca_ind(PCAR,col.ind = 'cos2',axes = c(2,3)))
# globalement pareil
### COMPARAISON AXES 1&3 ###
subplot(fviz_pca_var(PCARRang,col.var = 'cos2',axes = c(1,3)),
        fviz_pca_var(PCAR,col.var = 'cos2',axes = c(1,3)))
# globalem
subplot(fviz_pca_ind(PCARRang,col.ind = 'cos2',axes = c(1,3)),
        fviz_pca_ind(PCAR,col.ind = 'cos2',axes = c(1,3)))

####### ACP NATURE #######
corrplot(PCANRang$var$cos2)
### AXES 1&2 ###

fviz_cos2(PCAN,choice = 'var',axes = c(1,2))
plot.PCA(PCAN,axes = c(1,2),choix = 'var')
plot.PCA(PCAN,axes = c(1,2),choix = 'ind')
# Opposition entre les villes du sud et le reste de la France

###### ACP DE RANG NATURE #######
corrplot(PCANRang$var$cos2)
fviz_pca_var(PCANRang,col.var = 'cos2',axes = c(1,2))
fviz_pca_ind(PCANRang,col.ind = 'cos2',axes = c(1,2))
fviz_cos2(PCANRang,choice = 'var',axes = c(1,2))
plot.PCA(PCANRang,axes = c(1,2),choix = 'var')
# Soleil, Température
plot.PCA(PCANRang,axes = c(1,2),choix = 'ind')
fviz_pca_biplot(PCANRang,axes = c(1,2))
# Villes du sud contre reste de la France le long de l'axe 1

subplot(fviz_pca_var(PCANRang,col.var = 'cos2',axes = c(1,2)),
        fviz_pca_var(PCAN,col.var = 'cos2',axes = c(1,2)))
subplot(fviz_pca_ind(PCANRang,col.ind = 'cos2',axes = c(1,2)),
        fviz_pca_ind(PCAN,col.ind = 'cos2',axes = c(1,2)))

fviz_pca_ind(PCAN,col.ind = 'cos2',axes = c(1,2))
fviz_pca_ind(PCANRang,col.ind = 'cos2',axes = c(1,2))





###### ACP CULTURE #######
corrplot(PCAC$var$cos2)
corrplot(PCACRang$ind$cos2)

### AXES 1&2 ###
fviz_cos2(PCAC,choice = 'var',axes = c(1,2))
plot.PCA(PCAC,axes = c(1,2),choix = 'var')
# MonumHistorique Musées RestaurDistingués
plot.PCA(PCAC,axes = c(1,2),choix = 'ind')
# Paris très isolé sur l'axe 1, SaintDenis isolé sur l'axe 2
fviz_cos2(PCACbis,choice = 'var',axes = c(1,2))

### AXES 2&3 ###
fviz_cos2(PCAC,choice = 'var',axes = c(2,3))
plot.PCA(PCAC,axes = c(2,3),choix = 'var')
#Etudiant bien représenté, Presse et Cinema dans une moindre mesure
plot.PCA(PCAC,axes = c(2,3),choix = 'ind')

### AXES 3&4 ###
fviz_cos2(PCAC,choice = 'var',axes = c(3,4))
plot.PCA(PCAC,axes = c(3,4),choix = 'var')
# PretLivres Cinema
plot.PCA(PCAC,axes = c(3,4),choix = 'ind')
# Cannes isolée

###### ACP DE RANG CULTURE ###### 
corrplot(PCACRang$var$cos2)
corrplot(PCACRang$ind$cos2)
fviz_eig(PCACRang, addlabels = TRUE, ylim = c(0, 40))
# Axe 1: Musees, MonumHistorique, RestaurDistingues, Etudiant, Cinema
# Axe 2: Presse, Etudiant, RestaurDinstingues
# Axe 3: PretLivres, Presse

### AXES 1&2 ###
fviz_pca_ind(PCAC,col.ind = 'cos2',axes = c(1,2))
fviz_pca_ind(PCACRang,col.ind = 'cos2',axes = c(1,2))
# Paris, Bordeaux, Sete, Nantes, Dijon, Toulouse, Lille villes les mieux représentées.
# SaintDenis moins bien représenté qu'avant
fviz_cos2(PCACRang,choice = 'ind',axes = c(1,2))

subplot(fviz_pca_ind(PCAC,col.ind = 'cos2',axes = c(1,2)),
        fviz_pca_ind(PCACRang,col.ind = 'cos2',axes = c(1,2)))
fviz_pca_var(PCAC,col.var = 'cos2',axes = c(1,2))
fviz_pca_var(PCACRang,col.var = 'cos2',axes = c(1,2))
# Presse, MonumHistoriques, RestaurDistinques, Musees, Etudiant
# PretLivres et Cinema mal représentés.
# Presse et MonumHistoriques décorrélés, MonumHistoriques et Musees pos. corrélés
# RestaurDistingues et Etudiant presque décorrélés.
subplot(fviz_pca_var(PCAC,col.var = 'cos2',axes = c(1,2)),
        fviz_pca_var(PCACRang,col.var = 'cos2',axes = c(1,2)))
fviz_pca_biplot(PCACRang,axes = c(1,2))

fviz_cos2(PCACRang,choice = 'var',axes = c(1,2))
plot.PCA(PCACRang,axes = c(1,2),choix = 'var')
#Presse, MonumHistoriques, RestaurDistingue,Musees Presse
plot.PCA(PCACRang,axes = c(1,2),choix = 'ind')


### AXES 2&3 ###
fviz_cos2(PCACRang,choice = 'var',axes = c(2,3))
fviz_pca_var(PCACRang,col.var = 'cos2',axes = c(2,3))
# PretLivres et Presse très bien représentés, le reste est mal représenté
# Presse et PretLivres presque décorrélées.
fviz_pca_ind(PCACRang,col.ind = 'cos2',axes = c(2,3))
# Chalon sur Saone, Nevers, Amiens, LeHavre, Brest, Toulon, Carcassonne
fviz_pca_ind(PCAC,col.ind = 'cos2',axes = c(2,3))
subplot(fviz_pca_ind(PCACRang,col.ind = 'cos2',axes = c(2,3)),
        fviz_pca_ind(PCAC,col.ind = 'cos2',axes = c(2,3)))
# Sarcelles, Poitiers, Saint Denis moins bien représentées
plot.PCA(PCACRang,axes = c(2,3),choix = 'var')
# PretLivres, Presse quasiment décorrelés
plot.PCA(PCACRang,axes = c(2,3),choix = 'ind')
fviz_pca_biplot(PCACRang,axes = c(2,3))

fviz_cos2(PCACRang,choice = 'ind',axes = c(2,3))
fviz_cos2(PCAC,choice = 'ind',axes = c(2,3))
subplot(fviz_cos2(PCACRang,choice = 'ind',axes = c(2,3)),
        fviz_cos2(PCAC,choice = 'ind',axes = c(2,3)))

subplot(fviz_pca_var(PCACRang,col.var = 'cos2',axes = c(2,3)),
        fviz_pca_var(PCAC,col.var = 'cos2',axes = c(2,3)))

fviz_pca_biplot(PCACRang,axes = c(2,3))

### AXES 1&3 ###
fviz_pca_var(PCACRang,col.var = 'cos2',axes = c(1,3))
#PretLivres, MonumHistoriques, Musees
fviz_pca_ind(PCACRang,col.ind = 'cos2',axes = c(1,3))
# 
fviz_cos2(PCACRang,choice = 'var',axes = c(1,3))
fviz_cos2(PCACRang,choice = 'ind',axes = c(1,3))

plot.PCA(PCACRang,axes = c(1,3),choix = 'var')
# PretLivres, Musees, MonumHistoriques
plot.PCA(PCACRang,axes = c(1,3),choix = 'ind')
fviz_pca_biplot(PCACRang,axes = c(1,3))
# On semble retrouver un groupe de ville étudiantes le long de l'axe 1

subplot(fviz_pca_var(PCACRang,col.var = 'cos2',axes = c(1,3)),
        fviz_pca_var(PCAC,col.var = 'cos2',axes = c(1,3)))

subplot(fviz_pca_ind(PCACRang,col.ind = 'cos2',axes = c(1,3)),
        fviz_pca_ind(PCAC,col.ind = 'cos2',axes = c(1,3)))

corrplot(PCACRang$var$cos2)
corrplot(PCAC$var$cos2)
# Axe 1, beaucoup plus de Musees, MonumHistorique, RestaurDistingues dans la normée que dans l'autre

fviz_pca_ind(PCAC,col.ind = 'cos2',axes = c(1,2))
subplot(fviz_pca_var(PCAC,col.var = 'cos2',axes = c(1,2)),
        fviz_pca_var(PCACRang,col.var = 'cos2',axes = c(1,2)))

fviz_pca_var(PCACRang,col.var = 'cos2',axes = c(2,3))
fviz_pca_var(PCACRang,col.var = 'cos2',axes = c(1,3))
