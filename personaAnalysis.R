library(vegan)
library(psych)


#catasks=catasks[,-1]
catasks = read.csv("filename", check.names=FALSE)
dc = vegdist(catasks[,-1], method = "jaccard")
hc.catasks = hclust(dc)
png(filename = "DendrogramGraph.png")
plot(hc.catasks, labels=catasks$Email, hang=-1)
dev.off()

catasks.cor = read.csv("filename")
catmatrix<-cor(catasks.cor[,-1])
write.csv(catmatrix, "catcor.csv")

#plot scree plot
library(nFactors)
catasks.ev = eigen(catmatrix)
catasks.ap = parallel(subject=nrow(catmatrix), var = ncol(catmatrix), rep=100, cent=.05)
nS = nScree(x=catasks.ev$values, aparallel=catasks.ap$eigen$qevpea)
png(filename="ScreePlot.png")
plotnScree(nS)
dev.off()

#transposed data
catasks.trans = read.csv("filename.csv", check.names=FALSE)
dc.trans = vegdist(catasks.trans[,-1], method = "jaccard")
hc.trans = hclust(dc.trans)
plot(hc.trans, labels=catasks.trans$primaryRole, hang=-1)


#FactorAnalysis
sink("filename.txt")
fa.catasks = factanal(catasks.cor[,c(-1)], factor=8, rotation = "varimax")
print(fa.catasks, cutoff=.2, sort =TRUE)

sink()

###Plot factor 1 by factor 2
#load = fa.catasks$loadings[,c(1, 2, 5)]
#plot(load,type="n")
#text(load, labels=names(catasks.cor), cex=.7)
###

###Plot factor 1 by factor 2
par(mfrow=c(2,2))
load1 = fa.catasks$loadings[,c(1, 2)]
load2 = fa.catasks$loadings[,c(2, 3)]
load3 = fa.catasks$loadings[,c(3, 4)]
load4 = fa.catasks$loadings[,c(4, 5)]
plot(load1,type="n")
text(load1, labels=names(catasks.cor), cex=.7)
plot(load2,type="n")
text(load2, labels=names(catasks.cor), cex=.7)
plot(load3,type="n")
text(load3, labels=names(catasks.cor), cex=.7)
plot(load4,type="n")
text(load4, labels=names(catasks.cor), cex=.7)
###




