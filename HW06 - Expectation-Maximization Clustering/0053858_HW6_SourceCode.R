set.seed(521)
library(MASS)
library(mixtools)

#Q1
cluster_sizes <- c(50, 50, 50, 50, 100)
cluster_means <- t(matrix(c(2.5, 2.5, -2.5, 2.5, -2.5, -2.5, 2.5, -2.5, 0,  0), 2, 5))
cluster_covariances <- t(matrix(c(0.8, -0.6, -0.6, 0.8, 0.8, 0.6, 0.6, 0.8, 0.8, -0.6, -0.6, 0.8, 0.8, 0.6, 0.6, 0.8, 1.6, 0, 0, 1.6), 4, 5))

X1 <- mvrnorm(cluster_sizes[1], cluster_means[1,], matrix(cluster_covariances[1,], 2, 2))
X2 <- mvrnorm(cluster_sizes[2], cluster_means[2,], matrix(cluster_covariances[2,], 2, 2))
X3 <- mvrnorm(cluster_sizes[3], cluster_means[3,], matrix(cluster_covariances[3,], 2, 2))
X4 <- mvrnorm(cluster_sizes[4], cluster_means[4,], matrix(cluster_covariances[4,], 2, 2))
X5 <- mvrnorm(cluster_sizes[5], cluster_means[5,], matrix(cluster_covariances[5,], 2, 2))
X <- rbind(X1, X2, X3, X4, X5)
plot(X, pch=20, xlim=c(-6, 6), ylim=c(-6, 6), xlab = "x1", ylab = "x2")

#Q2
N <- 300
K <- 5

centroids <- X[sample(1:N, K),]


for (iteration in 1:2){
  D <- as.matrix(dist(rbind(centroids, X), method = "euclidean"))
  D <- D[1:nrow(centroids), (nrow(centroids) + 1):(nrow(centroids) + nrow(X))]
  assignments <- sapply(1:ncol(D), function(i) {which.min(D[,i])})
    for (k in 1:K) {
      centroids[k,] <- colMeans(X[assignments == k,])
    }  
}
centroids

#Q3
centroid_cov <- sapply(X = 1:K, FUN = function(c) {cov(X[assignments == c,])})
centroid_cov <- t(centroid_cov)

cluster_priors <- sapply(X = 1:K, FUN = function(c) {mean(assignments == c)})

#Q4
for(iteration in 1:100){
  #-this part calculates P(Xi|Ck,phi)
  mic <- matrix(0, N, K)
  for (i in 1:N){
    for (c in 1:K){
      mic[i,c] <- (1/(2*pi*det(matrix(centroid_cov[c,],2,2)))^(1/2))*exp((-1/2)*t(X[i,]-centroids[c,])%*%solve(matrix(centroid_cov[c,],2,2))%*%(X[i,]-centroids[c,]))
    }
  }
  
  #-this part calculates numerator of hik
  hik_num <- matrix(0, N, K)
  for (i in 1:N){
    for (c in 1:K){
      hik_num[i,c] <- (mic[i,c] * cluster_priors[c])
    }
  }
  
  #-this part calculates denominator of hik
  hik_den <- rep(0, N)
  for(i in 1:N){
    for(c in 1:K){
      hik_den[i] <- hik_den[i] + hik_num[i,c]
    }
  }  
  
  #-this part calculates hik
  hik <- matrix(0, N, K)
  for (i in 1:N){
    for (c in 1:K){
      hik[i,c] <- hik_num[i,c]/hik_den[i]
    }
  }
  
  #-this part updates the positions of the centroids
  for(c in 1:K){
    centroids[c,] <- colSums(hik[,c]*X)/colSums(hik)[c]
  }
  
  #-this part updates the covariances of the centroids
  for(c in 1:K){
    centroid_cov[c,] <- 0
    for(i in 1:N){
      centroid_cov[c,] <- centroid_cov[c,]+hik[i,c]*(X[i,]-centroids[c,])%*%t(X[i,]-centroids[c,])
    }
    centroid_cov[c,] <- centroid_cov[c,]/colSums(hik)[c]
  }
  
  #-this part updates the prior probabilities of each cluster
  for(c in 1:K){
    cluster_priors[c] <- colSums(hik)[c]/N
  }
  
  #-this part updates the clusters of each data point
  D <- as.matrix(dist(rbind(centroids, X), method = "euclidean"))
  D <- D[1:nrow(centroids), (nrow(centroids) + 1):(nrow(centroids) + nrow(X))]
  assignments <- sapply(1:ncol(D), function(i) {which.min(D[,i])})
}

centroids

#Q5
plot(X[assignments == 1,], pch = 20, xlim = c(-6, 6), ylim = c(-6, 6),
     xlab = "x1", ylab = "x2", col = "red")
points(X[assignments == 2,], pch = 20, col = "orange")
points(X[assignments == 3,], pch = 20, col = "blue")
points(X[assignments == 4,], pch = 20, col = "green")
points(X[assignments == 5,], pch = 20, col = "purple")

for(c in 1:K){
  ellipse(cluster_means[c,], matrix(cluster_covariances[c,], 2,2), alpha = .05, npoints = cluster_sizes[c], newplot = FALSE, draw = TRUE, lty=2, lwd=2)
  ellipse(centroids[c,], matrix(centroid_cov[c,], 2,2), alpha = .05, npoints = nrow(X[assignments == c,]), newplot = FALSE, draw = TRUE, lwd=2)
}