setwd("/Users/sgriffin/Documents/GitHub/MultivariateStats/Online_Ass2/")

df2 = read.csv("taxdata.csv")
# View(df2)
df = df2[,-1]
# View(df)
rownames(df)=df2[,1]

install.packages("stats")
install.packages("psy")
install.packages("MASS")
library(stats)
library(psy)
library(MASS)


# Euclidian distances computed
euclid = dist(df)
euclid

cities_mds <-isoMDS(euclid, k=2) 
cities_mds
cities_mds$points
# isoMDS is the multidimensional scaling command
# k is the number of dimensions we want the scale to be on - always just make it 2 so you can view it in 2D
df$cities_mds1 = cities_mds$points[,1]
df$cities_mds2 = cities_mds$points[,2]


mds1<- cbind(cities_mds$points[,1])
mds2<- cbind(cities_mds$points[,2])

plot(mds1,mds2,cex= 1, xlab="Dimension 1",ylab="Dimension 2", main="Multidimensional Scaling with State Tax Metrics")
text(mds1,mds2,pos=2,cex=1,labels=rownames(df))





