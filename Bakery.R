library(readr)
bakery <- read_csv("DMAssignment8/bakery-binary.csv")
View(bakery)

newbakery <- bakery

newbakery$Weekend <- NULL

cluster <- kmeans(newbakery,3)
cluster 

table(bakery$Weekend, cluster$cluster)

plot(newbakery[c("Chocolate_Cake", "Chocolate_Tart","Truffle_Cake","Almond_Croissant")], col=cluster$cluster)
points(cluster$centers[,c("Chocolate_Cake", "Chocolate_Tart","Truffle_Cake","Almond_Croissant")], col=1:3, pch=8, cex=2)

library(cluster)
result <- pam(newbakery, 2)
table(result$clustering, bakery$Weekend)
plot(result)

#Hierarchical Clustering
hierarchical <- hclust(dist(newbakery), method = "ave")
plot(hierarchical, hang = -1, labels = bakery$Weekend)
rect.hclust(hierarchical, k = 4)
groups <- cutree(hierarchical, k = 4)
groups

#Density Based Clustering 
library(fpc)
dense <- dbscan(newbakery, eps=0.25, MinPts = 2)
table(dense$cluster, bakery$Weekend)
par(oma=c(5,7,1,1))
plot(dense, newbakery[c(10:30)])
plot(newbakery[c(10:15)], dense$cluster)
