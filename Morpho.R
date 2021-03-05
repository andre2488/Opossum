#Morfometria Pomacea sp. nov.
library(geomorph)
library(yarrr)
library(Morpho)
require(ape)
require(geomorph)
require(MASS)
require(Rvcg)
require(stats)
require(vegan)


Pomacea=readland.tps("Pomacea.tps", specID = c("imageID"), negNA = FALSE,
             readcurves = FALSE, warnmsg = TRUE)
View(Pomacea)

write.csv(as.vector(dimnames(Pomacea)[3]), "Nombres.csv")

classifier <-read.csv("Fotos.csv", header=T)
is.factor(classifier$Rivers)

Y <-gpagen(Pomacea)
size<-Y$Csize

dim(Pomacea) # confiere dimensiones, número de individuos
ind=c(1:dim(Pomacea)[3]) # vetor indivÃ???duos

ref<-mshape(Y$coords)
Y2 <- rotate.coords(Y, "rotateC") #rotate image

PCA=plotTangentSpace(Y2$coords,verbose=TRUE, label = T) 


plotAllSpecimens(Y2$coords, links = links)
links=matrix(c(1, rep(2:17, each=2), 18,1,18, 12, 19, 19,3),ncol=2, nrow=20, byrow=T) #forma figura

col.gp=c("#0C5BB0FF", "#9A703EFF", "#15983DFF", "#EC579AFF" ,"#FA6B09FF", "#149BEDFF", "#FEC10BFF","#EE0011FF","#972C8DFF")
names(col.gp) <- levels(classifier$Rivers)
col.gp <- col.gp[match(classifier$Rivers, names(col.gp))]


# Classificadores
# Extrair Classificadores a partir do ID (Stract classifiers from ID String)
categories=strsplit(dimnames(Pomacea)[[3]], "_" ) # separa os nomes pelo h?fen
classifiers=matrix(unlist(categories),ncol=2,byrow=T) # transforma em matriz
classifiers=cbind(dimnames(Pomacea)[[3]],as.character(classifier$Rivers)) # adiciona o ID na primeira coluna
colnames(classifiers)=c("FileID","Place") #renomeia as colunas
classifiers=as.data.frame(classifiers)
classifiers

Place=classifiers[,2]


tiff(file = "Morfometria.tiff", width = 2100, height = 1500, units = "px", res = 400)
xlab <- paste("Principal Component 1 (",  round(PCA$pc.summary$importance[2,1]*100, 1), "%)", sep="")
ylab <- paste("Principal Component 2 (", round(PCA$pc.summary$importance[2,2]*100, 1), "%)", sep="")
mat <- matrix(c(4,5,0,1,1,2,1,1,3), 3)

layout(mat, widths=c(1,1,1), heights=c(1,1,1))# set the size of the rows and columns

# Item 1 to plot, the graph of PC1 vs PC2
par(mar=c(4, 4, 1, 1)) # sets the margins

colbs=c(unique(col.gp))

plot(PCA$pc.scores[,1], PCA$pc.scores[,2], xlim = c(-0.15, 0.07), pch=19, cex=1.2, cex.lab=1.1, cex.main=1.5,cex.axis=1, col=col.gp, xlab=xlab, ylab=ylab, asp=T)
legend(-0.150, 0, legend= unique(classifier$Rivers), 
       pch=19,  col=unique(col.gp), 
       bty = "n", cex = 0.85, y.intersp=0.75 , pt.cex=1.5)

#text(PCA$pc.scores[,1], PCA$pc.scores[,2], row.names(PCA$pc.scores), cex=1, pos=4, col="red")
ref <- mshape(Y2$coords)# assign mean shape for use with plotRefToTarget below

# Item 2 to plot, the first TPS grid; here we use the outline option to add to the visualisation
par(mar = c(0,0,0,0)) # sets the margins
plotRefToTarget(ref,PCA$pc.shapes$PC1min,links = links, mag=1)
# Item 3
plotRefToTarget(ref,PCA$pc.shapes$PC1max,links = links, mag=1)
# Item 4
plotRefToTarget(ref,PCA$pc.shapes$PC2max,links = links, mag=1)
# Item 5
plotRefToTarget(ref,PCA$pc.shapes$PC2min,links = links, mag=1)

dev.off()

P1 <- t(matrix(PCA$rotation[,1], 2, ))
P2 <- t(matrix(PCA$rotation[,2], 2, ))

Imagenes=dimnames(Pomacea)
Imagenes=c(Imagenes[[3]])
row.names(PCA$pc.scores)=Imagenes




Pomaceareev=readland.tps("Preeveifinal.tps", specID = c("imageID"), negNA = FALSE,
                     readcurves = FALSE, warnmsg = TRUE)
classifier <-read.csv("Fotosreevesi.csv", header=T)
is.factor(classifier$Lugar)


Y <-gpagen(Pomaceareev)
size<-Y$Csize

dim(Pomaceareev) # confiere dimensiones, número de individuos
ind=c(1:dim(Pomaceareev)[3]) # vetor indivÃ???duos

ref<-mshape(Y$coords)
Y2 <- rotate.coords(Y, "rotateC") #rotate image

PCA=plotTangentSpace(Y2$coords,verbose=TRUE, label = T) 

plot(PCA$pc.scores)

plotAllSpecimens(Y2$coords, links = links)
links=matrix(c(1, rep(2:17, each=2), 18,1,18, 12, 19, 19,3),ncol=2, nrow=20, byrow=T) #forma figura

col.gp=c("#0C5BB0FF", "#9A703EFF", "#15983DFF", "#EC579AFF" ,"#FA6B09FF", "#149BEDFF", "#FEC10BFF","#EE0011FF", "#972C8DFF")
names(col.gp) <- levels(classifier$Lugar)
col.gp <- col.gp[match(classifier$Lugar, names(col.gp))]


# Classificadores
# Extrair Classificadores a partir do ID (Stract classifiers from ID String)


tiff(file = "Morfometria2.tiff", width = 2100, height = 1500, units = "px", res = 400)
xlab <- paste("Principal Component 1 (",  round(PCA$pc.summary$importance[2,1]*100, 1), "%)", sep="")
ylab <- paste("Principal Component 2 (", round(PCA$pc.summary$importance[2,2]*100, 1), "%)", sep="")
mat <- matrix(c(4,5,0,1,1,2,1,1,3), 3)

layout(mat, widths=c(1,1,1), heights=c(1,1,1))# set the size of the rows and columns

# Item 1 to plot, the graph of PC1 vs PC2
par(mar=c(4, 4, 1, 1)) # sets the margins

colbs=c(unique(col.gp))

plot(PCA$pc.scores[,1], PCA$pc.scores[,2], xlim = c(-0.16, 0.08), ylim = c(-0.045, 0.050), pch=19, cex=1.2, cex.lab=1.1, cex.main=1.5,cex.axis=1, col=col.gp, xlab=xlab, ylab=ylab, asp=T)
legend(-0.160, -0.003, legend= unique(classifier$Lugar), 
       pch=19,  col=unique(col.gp), 
       bty = "n", cex = 0.85, y.intersp=0.75 , pt.cex=1.5)

#text(PCA$pc.scores[,1], PCA$pc.scores[,2], row.names(PCA$pc.scores), cex=1, pos=4, col="red")
ref <- mshape(Y2$coords)# assign mean shape for use with plotRefToTarget below

# Item 2 to plot, the first TPS grid; here we use the outline option to add to the visualisation
par(mar = c(0,0,0,0)) # sets the margins
plotRefToTarget(ref,PCA$pc.shapes$PC1min,links = links, mag=1)
# Item 3
plotRefToTarget(ref,PCA$pc.shapes$PC1max,links = links, mag=1)
# Item 4
plotRefToTarget(ref,PCA$pc.shapes$PC2max,links = links, mag=1)
# Item 5
plotRefToTarget(ref,PCA$pc.shapes$PC2min,links = links, mag=1)

dev.off() 