#Morfometria Pomacea sp. nov.
library(geomorph)
specs=list.files()[grep(".jpg",list.files())]
specs

Pomacea=readland.tps("Conchas.tps", specID = c("imageID"), negNA = FALSE,
             readcurves = FALSE, warnmsg = TRUE)
View(Pomacea)
classifier <-read.csv("Fotos.csv", header=T)
is.factor(classifier$Lugar)

Y <-gpagen(Pomacea)
ref<-mshape(Y$coords)
Y2 <- rotate.coords(Y, "rotateC") #rotate image
plot(Y2)

PCA=plotTangentSpace(Y2$coords,verbose=TRUE) 


plotAllSpecimens(Y2$coords, links = links)
links=matrix(c(1, rep(2:17, each=2), 18,1,18, 12, 19, 19,3),ncol=2, nrow=20, byrow=T) #forma figura

col.gp <- rainbow(length(levels(classifier$Lugar)))
names(col.gp) <- levels(classifier$Lugar)
col.gp <- col.gp[match(classifier$Lugar, names(col.gp))]

xlab <- paste("Principal Component 1", "(", round(PCA$pc.summary$importance[2,1]*100, 1), "%)", sep="")
ylab <- paste("Principal Component 2", "(", round(PCA$pc.summary$importance[2,2]*100, 1), "%)", sep="")
mat <- matrix(c(4,5,0,1,1,2,1,1,3), 3)

layout(mat, widths=c(1,1,1), heights=c(1,1,1))# set the size of the rows and columns

# Item 1 to plot, the graph of PC1 vs PC2
par(mar=c(4, 4, 1, 1)) # sets the margins


plot(PCA$pc.scores[,1], PCA$pc.scores[,2], pch=21, cex=4, bg=col.gp, xlab=xlab, ylab=ylab, asp=T)
legend(-0.125, 0.07, legend= unique(classifier$Lugar), 
       pch=19,  col=unique(col.gp),
       bty = "n", cex = 1.5, y.intersp=0.8)
ref <- mshape(Y2$coords)# assign mean shape for use with plotRefToTarget below

# Item 2 to plot, the first TPS grid; here we use the outline option to add to the visualisation
par(mar = c(0,0,0,0)) # sets the margins
plotRefToTarget(ref,PCA$pc.shapes$PC1min,links = links, mag=1)
# Item 3
plotRefToTarget(ref,PCA$pc.shapes$PC1max,links = links, mag=1)
# Item 4
plotRefToTarget(ref,PCA$pc.shapes$PC2min,links = links, mag=1)
# Item 5
plotRefToTarget(ref,PCA$pc.shapes$PC2max,links = links, mag=1)
