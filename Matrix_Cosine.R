library(readr)
matrix <- read_csv("DMAssignment8/matrix.csv")
#View(matrix)

cosine <- function(v1, v2) crossprod(v1,v2)/sqrt(crossprod(v1)*crossprod(v2))
Doc1 <- c(2, 2, 1, 0, 2, 1)
Doc2 <- c(1, 3, 0, 2, 7, 0)
Doc3 <- c(0, 2, 3, 3, 0, 2)
Doc4 <- c(2, 1, 0, 3, 8, 4)
Doc5 <- c(6, 5, 0, 1, 4, 0)
Doc6 <- c(2, 1, 6, 8, 0, 4)
Doc7 <- c(5, 3, 1, 0, 2, 0)

cluster1 <- cosine(Doc1, Doc1)
cluster1
cluster2 <- cosine(Doc4, Doc6)
cluster2
cluster3 <- cosine(Doc5, Doc7)
cluster3

#Check Doc1 against all three clusters
c1 <- cosine(Doc2, Doc1)
c1
c2 <- cosine(Doc1, Doc4)
c2
c3 <- cosine(Doc1, Doc5)
c3

cat('Document 1 belongs to cluster three with a probability of',c3)

#Check Doc3 against all three clusters
c1 <- cosine(Doc3, Doc1)
c1
c2 <- cosine(Doc3, Doc4)
c2
c3 <- cosine(Doc3, Doc5)
c3

cat('Document 3 belongs to cluster one with a probability of',c1)

cat('Final Document Clusters')
cat('Cluster 1: Document2 and Document3')
cat('Cluster 2: Document4 and Document6')
cat('Cluster 3: Document1, Document5 and Document7')