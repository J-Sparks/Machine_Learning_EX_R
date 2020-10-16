# LOAD DATA ----
data(iris)
plot(iris)

# SCALE DATA ----
irisScaled <- scale(iris[, -5])

# K-MEANS CLUSTERING ----
## CLUSTERING
fitK <- kmeans(irisScaled[, -5], 3)
fitK
str(fitK)
fitK$cluster
plot(iris, col = fitK$cluster)

## CHOOSING K
k <- list()
for(i in 1:10){
  k[[i]] <- kmeans(irisScaled[,1:4], i)
}

k

betweenss_totss <- list()
for(i in 1:10){
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}

plot(1:10, betweenss_totss, type = "b", 
     ylab = "Between SS / Total SS", xlab = "Clusters (k)")

for(i in 1:4){
  plot(iris, col = k[[i]]$cluster)
}

# HIERACHICAL CLUSTERING ----
d <- dist(irisScaled[, 1:4])
fitH <- hclust(d, "ward.D2")
plot(fitH) 
rect.hclust(fitH, k = 3, border = "red") 
clusters <- cutree(fitH, k = 3) 
plot(iris, col = clusters)

# MODEL-BASED CLUSTERING ----
library(mclust)
fitM <- Mclust(irisScaled)
plot(fitM)

# DENSITY-BASED CLUSTERING ----
install.packages("dbscan")
library(dbscan)
kNNdistplot(irisScaled, k = 3)
abline(h = 0.7, col = "red", lty = 2)
fitD <- dbscan(irisScaled, eps = 0.7, minPts = 5)
fitD
plot(iris, col = fitD$cluster)
