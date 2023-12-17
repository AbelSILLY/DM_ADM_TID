plot.PCA(PCAXbis,axes = c(1,2),choix = 'var',title = 'ACP générale axes 1 et 2 avec ouest parisien en supplémentaire')
plot.PCA(PCAXbis,axes = c(1,2),choix = 'ind',title = 'ACP générale axes 1 et 2 avec ouest parisien en supplémentaire')
plot.PCA(PCAXbis,axes = c(2,3),choix = 'var',title = 'ACP générale axes 2 et 3 avec ouest parisien en supplémentaire')
plot.PCA(PCAXbis,axes = c(2,3),choix = 'ind',title = 'ACP générale axes 2 et 3 avec ouest parisien en supplémentaire')
plot.PCA(PCAXbis,axes = c(3,4),choix = 'var',title = 'ACP générale axes 3 et 4 avec ouest parisien en supplémentaire')
plot.PCA(PCAXbis,axes = c(3,4),choix = 'ind',title = 'ACP générale axes 3 et 4 avec ouest parisien en supplémentaire')
plot.PCA(PCAXbis,axes = c(4,5),choix = 'var',title = 'ACP générale axes 4 et 5 avec ouest parisien en supplémentaire')
plot.PCA(PCAXbis,axes = c(4,5),choix = 'ind',title = 'ACP générale axes 4 et 5 avec ouest parisien en supplémentaire')


####### ACP ECONOMIE ####### 
plot.PCA(PCAE,axes = c(1,2),choix = 'var',title = 'ACP thème économie axe 1&2')
plot.PCA(PCAE,axes = c(1,2),choix = 'ind',title = 'ACP thème économie axe 1&2')
plot.PCA(PCAE,axes = c(2,3),choix = 'var')
plot.PCA(PCAE,axes = c(2,3),choix = 'ind')
plot.PCA(PCAE,axes = c(3,4),choix = 'var')
plot.PCA(PCAE,axes = c(3,4),choix = 'ind')


####### ACP RISQUES #######
plot.PCA(PCAR,axes = c(1,2),choix = 'var')
plot.PCA(PCAR,axes = c(1,2),choix = 'ind')
plot.PCA(PCAR,axes = c(2,3),choix = 'var')
plot.PCA(PCAR,axes = c(2,3),choix = 'ind')
plot.PCA(PCAR,axes = c(1,3),choix = 'var')
plot.PCA(PCAR,axes = c(1,3),choix = 'ind')


####### ACP NATURE #######
plot.PCA(PCAN,axes = c(1,2),choix = 'var')
plot.PCA(PCAN,axes = c(1,2),choix = 'ind')
plot.PCA(PCAN,axes = c(2,3),choix = 'var')
plot.PCA(PCAN,axes = c(2,3),choix = 'ind')
plot.PCA(PCAN,axes = c(1,3),choix = 'var')
plot.PCA(PCAN,axes = c(1,3),choix = 'ind')

###### ACP CULTURE #######
plot.PCA(PCAC,axes = c(1,2),choix = 'var')
plot.PCA(PCAC,axes = c(1,2),choix = 'ind')
plot.PCA(PCACbis,axes = c(1,2),choix = 'var')
plot.PCA(PCACbis,axes = c(1,2),choix = 'ind')



