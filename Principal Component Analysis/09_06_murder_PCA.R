#Principle components
#murder dataset
murder<-read.csv("F:\\ML_Class\\Machine Learning\\Principal Component Analysis\\Murders_India_2013.csv",row.names = 1)

prc <- prcomp(murder,scale. = T)
names(prc)
prc$sdev
prc$rotation # Eigenvectors
prc$center
prc$scale
prc$x # scale(milk,scale = T) %*% prc$rotation

pr.var <- prc$sdev^2
pve <- pr.var / sum(pr.var)
# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cummulative Proportion of Variance Explained",
     type = "b")

biplot(prc,cex=0.7,scale = 0)

prc$rotation %*% diag(prc$sdev^2) %*% t(prc$rotation)
### Similar to the following ###
cor(scale(murder,scale=F))

##apply(prc$rotation,2,sd)
#############################

prc <- prcomp(murder)
biplot(prc,cex=0.7,scale = 0)
prc$rotation

prc <- prcomp(murder,scale. = T)
biplot(prc,cex=0.7,scale = 0)
prc$rotation
