rm(list = ls())
#set.seed(5082)

installIfAbsentAndLoad <- function(neededVector) {
  if(neededVector != "") {
    for(thispackage in neededVector) {
      if( ! require(thispackage, character.only = T) )
      { install.packages(thispackage)}
      require(thispackage, character.only = T)
    }    
  }
}
needed <- c('ISLR', 'Class', 'e1071')

####################
##    Question 1
####################
data <- USArrests

pca1 <- prcomp(data, scale = T)
#biplot(pca1, scale = 0, cex = .5)

# Part 1
pca_var <- pca1$sdev^2
pve1 <- pca_var/sum(pca_var)


# Part 2
rot <- pca1$rotation
scaled <- scale(data)
sumvar <- sum(apply(as.matrix(scaled)^2, 2, sum))
pve2 <- apply((as.matrix(scaled) %*% rot)^2, 2, sum)/sumvar

# Both give the same results, as desired.

####################
##    Question 2
####################

set.seed(5082)
trans.data <- t(data)
newdat <- scale(trans.data)
newdat <- t(newdat)

euclid <- dist(newdat)
euclid_dist <- (as.matrix(euclid^2))[lower.tri(as.matrix(euclid^2))]

corr_dist <- (1 - cor(t(newdat)))[lower.tri(1-cor(t(newdat)))]

#plot(corr_dist, euclid_dist)

# Plot shows linear increase between correlation distance and euclidean distance.
  # Supports proportionality.

####################
##    Question 3
####################

n <- 20
p <- 50
x1 <- matrix(rnorm(n*p), nrow = n, ncol = p)
x2 <- matrix(rnorm(n*p), nrow = n, ncol = p)
x3 <- matrix(rnorm(n*p), nrow = n, ncol = p)
for ( i in 1:n ){
  x1[i,] <- x1[i,] + rep(1,p)
  x2[i,] <- x2[i,] + rep(-1, p)
  x3[i,] <- x3[i,] + c(rep(1, p/2), rep(-1, p/2))
}
x.values <- rbind(x1, x2, x3)

# Part B
y.values <- c(rep(1, n), rep(2, n), rep(3, n))
#plot(x.values[,1:12], col=(4-y.values), pch = 19)

# Part C
partc <- prcomp(x.values, scale = T)
plot(partc$x[,1:2], col = 4-y.values, pch=19, xlab = 'First PCA', ylab = 'Second PCA')

# Part D
partd <- kmeans(x.values, 3, nstart = 20)
table(y.values, partd$cluster)
# K-means preformed perfectly. Each class contains the same number of obs. without overlap.

# Part E
parte <- kmeans(x.values, 2, nstart = 20)
table(y.values, parte$cluster)
# Reducing the number of clusters requires one of the original three to be combined with another
  # giving only 2 clusters for the 3 sets of y.values.

# Part F
partf <- kmeans(x.values, 4, nstart = 20)
table(y.values, partf$cluster)
# Increasing the number of clusters requires one of the original three to be split into two different
  # clusters giving a total of 4 clusters for the 3 sets of y.values.

# Part G
partg <- kmeans(partc$x[,1:2], 3, nstart = 20)
table(y.values, partg$cluster)
# Again, preformed perfectly - no overlap between clusters; same results as Part D

# Part H
x.scale <- scale(x.values, center = F, scale = T)
parth <- kmeans(x.scale, 3, nstart = 20)
table(y.values, parth$cluster)
# Although the 'no overlap' quality is the same, the clusters have changed from Part D
