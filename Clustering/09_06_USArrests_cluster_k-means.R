library(base)
data("USArrests")

# Average linkage clustering of data
US.scaled <- scale(USArrests)                                  
d <- dist(US.scaled)   


fit.average <- hclust(d, method="average")                          
plot(fit.average, main="Average Linkage Clustering",hang = -1)
rect.hclust(fit.average, k=5)

k4<-kmeans(US.scaled, centers=4)
k4$tot.withinss

k5<-kmeans(US.scaled, centers=5)
k5$tot.withinss

k6<-kmeans(US.scaled, centers=6)
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

wssplot(US.scaled) 

k4<-kmeans(US.scaled, centers=4)

k4clust <- data.frame(USArrests , k4$cluster)

pairs(USArrests,col= k4$cluster)
