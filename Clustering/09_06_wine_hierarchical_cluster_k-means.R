library(gclus)
data("wine")

# Average linkage clustering of milk data
wine.scaled <- scale(wine)                                  
d <- dist(wine.scaled)                                          
fit.average <- hclust(d, method="ward.D")                          
plot(fit.average, main="Average Linkage Clustering",hang = -1)
rect.hclust(fit.average, k=5)



# Selecting the number of clusters

library(NbClust)
nc <- NbClust(wine.scaled, distance="euclidean", 
              min.nc=2, max.nc=15, method="ward.D")
par(mfrow=c(1,1))

nc$Best.nc
nc$Best.nc[1,]

table(nc$Best.nc[1,])
barplot(table(nc$Best.nc[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria") 

plot(fit.average, main="Average Linkage Clustering",hang = -1)
rect.hclust(fit.average, k=3)
# Obtaining the final cluster solution
clusterID <- cutree(fit.average, k=3)
# Assigning ClusterID to observations
wineClust <- data.frame(wine , clusterID)


plot(fit.average,main="Average Linkage Clustering\n3 Cluster Solution")
rect.hclust(fit.average, k=3)

## Coloured Dendrogram ###
library(colorhcplot)
colorhcplot(fit.average,fac = factor(clusterID))

################# Kmeans ####################
k4<-kmeans(wine.scaled, centers=4)
k4$tot.withinss

k5<-kmeans(wine.scaled, centers=5)
k5$tot.withinss

k6<-kmeans(wine.scaled, centers=6)
k6$tot.withinss

# Plot function for within groups sum of squares by number of clusters
wssplot <- function(data, nc=15, seed2=2018) {
  wss <- array(dim=c(nc))
  for (i in 2:nc){
    set.seed(seed2)
    km <- kmeans(data, centers=i)
    wss[i] <- km$tot.withinss
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(wine.scaled) 

k4<-kmeans(wine.scaled, centers=4)

k4clust <- data.frame(wine , k4$cluster)

pairs(wine,col= k4$cluster)
