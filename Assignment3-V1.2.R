rm(list=ls())
options(warn=-1)   # Supress warning messages
##########################################################################################
### Functions
##########################################################################################
installIfAbsentAndLoad <- function(neededVector) {
  if(neededVector != "") {
    for(thispackage in neededVector) {
      if( ! require(thispackage, character.only = T) )
      { install.packages(thispackage)}
      require(thispackage, character.only = T)
    }    
  }
}
##############################
### Load required packages ###
##############################
installIfAbsentAndLoad("ISLR")
installIfAbsentAndLoad("Class")
installIfAbsentAndLoad("e1071")

##############################
#### Question 1 | PCA
##############################

#Read in the USArrests
head(USArrests)
nrow(USArrests)

#Find and PCA(s) using prcomb
pca_out<-prcomp(USArrests,scale=TRUE)

#Plot the PCA(s)
biplot(pca_out,scale = 0, cex=.5)

#(a) Using the sdev output of the prcomp() function, as was done in Section 10.2.3. 
prc_out <- prcomp(USArrests, scale = TRUE)
prc_var <- prc_out$sdev^2
pve <- prc_var / sum(prc_var)
pve
#Output
#0.62006039 0.24744129 0.08914080 0.04335752

#(b) By applying Equation 10.8 directly
loads <- prc_out$rotation
USArrests_Scaled <- scale(USArrests)
sumvar <- sum(apply(as.matrix(USArrests_Scaled)^2, 2, sum))
apply((as.matrix(USArrests_Scaled) %*% loads)^2, 2, sum) / sumvar

#Output
#0.62006039 0.24744129 0.08914080 0.04335752

#Both approaches yiled the same results

##############################
#### Question 2 | Clustering
##############################
set.seed(5082)
USArrests.scaled <- scale(USArrests)
dis.1 <- dist(USArrests.scaled)^2
dis.2 <- as.dist(1 - cor(t(USArrests.scaled)))
#summary(dis.2 / dis.1)
#Dont know what to do now?? 


####################################
#### Question 3 | PCA and Clustering
####################################

#(a) Generate a simulated data set called x.values
n <- 20 # the number of samples per class
p <- 50 # the number of variables
set.seed(5072)
x1 <- matrix(rnorm(n*p), nrow=n, ncol=p)
x2 <- matrix(rnorm(n*p), nrow=n, ncol=p)
x3 <- matrix(rnorm(n*p), nrow=n, ncol=p)
for(i in 1:n){
  x1[i,] <- x1[i,] + rep(1, p)
  x2[i,] <- x2[i,] + rep(-1, p)
  x3[i,] <- x3[i,] + c(rep(+1, p / 2), rep(-1, p / 2))
}
x.values <- rbind(x1, x2, x3)
#head(x.values)
#nrow(x.values)

#(b) Create an vector named y.values that represents class labels
y.values <- c(rep(1,n), rep(2,n), rep(3,n)) # the "true" class labels of the points
plot(x.values[,1:2], col =(4-y.values), pch=19)

#(c) Perform PCA on the 60 observations and plot the first two principal component score vectors
pr.out =prcomp(x.values, scale =TRUE)
# Plot the first two principal component score vectors
plot(pr.out$x[,1:2], col=4-y.values, pch =19, xlab ="First principal component", ylab="Second principal component")

#(d) Perform K-means clustering of the observations with K = 3
km.out <- kmeans(x.values, 3, nstart = 20)
table(y.values, km.out$cluster)
#         Class
#Cluster   1  2  3
#       1 20  0  0
#       2  0 20  0
#       3  0  0 20
#Although K-means clustering will arbitrarily number the clusters, we see that each arbitrary cluster is assigned to one class only. K-means performs perfectly in this case.

#(e) Perform K-means clustering of the observations with K = 2
km.out <- kmeans(x.values, 2, nstart = 20)
table(y.values, km.out$cluster)
#         Class
#Cluster   1  2
#       1 20  0
#       2  0 20
#       3 20  0
# We are aware of the data have three classes which result in 3 clusters
# Forcing 3 Classes into 2 clusters makes a class force itself into another cluster
# In the example, 20 observations that are clustured as 3, find their way to cluster 1

#(f) Perform K-means clustering of the observations with K = 4
km.out <- kmeans(x.values, 4, nstart = 20)
table(y.values, km.out$cluster)
#         Class
#Cluster   1  2  3  4
#       1  0  0  0 20
#       2  0 20  0  0
#       3 13  0  7  0
# When we introduce a new cluster, it does cause some obestavtions to move around
# Class 2 belongs to Cluster 2, Class 1 moves from Cluster 1 to Cluster 4 and Class 3 breaks into Cluster 3 and Cluster 1

#(g) Now perform K-means clustering with K = 3 on the first two principal component score vectors, 
#    rather than on the raw data.
km.out = kmeans(pr.out$x[,1:2], 3, nstart =20)
table(km.out$cluster, y.values, dnn=c("Cluster","Class"))
#         Class
#Cluster    1  2  3
#       1   0 20  0
#       2  20  0  0
#       3   0  0 20
# Below it can be seen that only the first two principal components almost perfectly separate
# each class into unique clusters. Class 1 moves from cluster 1 to cluster 2 and class 2 moves 
# opposite


#(h) Using the scale() function, perform K-means clustering with K = 3
x.scale <- scale(x.values, center = FALSE, scale = TRUE)
km.out =kmeans(x.scale, 3, nstart =20)
table(km.out$cluster, y.values, dnn=c("Cluster","Class"))
#         Class
#Cluster   1  2  3
#       1 20  0  0
#       2  0 20  0
#       3  0  0 20
# Scaling would not improve or change the clustering because the data was made using a 
# normally distributed random number generator. So resutls are similar to results in c)
par(mfrow=c(1,1))
plot(x.values[,1:2], col =(4-y.values), pch=19, xlim=c(-3,3), ylim=c(-3,3))
points(x.scale[,1:2], col =(4-y.values), pch=1)
legend(-3,3, c("Original","Scaled"), pch=c(19,1), cex=.8)