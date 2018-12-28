require(MASS)
require(randomForest)
require(cluster)
require(data.table)
#### necess fctns ####
# this function extracts mean and covariance information per-cluster from clustered data
findparams <- function(data = dists$full[,-ncol(dists$full)], clustering, nclus) {
  clusts <- as.data.table(cbind(data, predcluster = clustering))
  params <- list()
  for (i in 1:ndists) {
    ith_clust <- copy(clusts)[predcluster == i][, predcluster := NULL]
    params[[i]] <- list(means = as.numeric(sapply(ith_clust, mean)), 
                        covmat = cov(ith_clust),
                        i = i)
  }
  return(params)
}
# this function generates <ndists> normally distributed clusters
generatedists <- function(ndists, meanrange, covmax, dims, noise = FALSE) {
  mvns <- list()
  params <- list()
  for(i in 1:ndists) {
    # uniform random means
    mn <- runif(dims, min(meanrange), max(meanrange))
    
    # produce a covariance matrix thats positive definite by "dotting" uniform random
    # matrix with itself
    mat <- matrix(runif(dims^2, min = -covmax, max = covmax), ncol = dims, nrow = dims) 
    matpd <- t(mat) %*% mat
    
    mvns[[i]] <- cbind(mvrnorm(500, mn, matpd), i)
    params[[i]] <- list(i = i, means = mn, covmat = matpd)
  }
  concmvn <- do.call("rbind", mvns)
  
  # if Bernoulli noise should be added, this section forms a noise matrix of size <dims>*<number of data>
  if(noise) {
    noisemat <- matrix(nrow = 500*ndists, ncol = dims)
    for(i in 1:dims) {
      # Bernoulli noise between 0 and 1 is generated and scaled to 5% characteristic data range
      noisemat[,i] <- (rbinom(500*ndists, 1, 0.5)/1 - 0.5)*(max(meanrange)-min(meanrange))/20
    }
    concmvn[,-ncol(concmvn)] <- concmvn[,-ncol(concmvn)] + noisemat
  }
  
  return(list(full = concmvn, mvns = mvns, params = params))
}

# for some cluster mean results, finds the mean result in the reference table which is closest 
findclosest <- function(checktable, reftable) {
  checktable <- as.data.frame(checktable)
  reftable <- as.data.frame(reftable)
  closestvector <- apply(checktable, 1, function(x) which.min(dist(rbind(x, reftable))))
  return(closestvector)
}


#### generate distributions ####
rng <- 4 # this range of means SEVERELY affects how well algorithms will perform
ndists <- 4
meanrange <- c(-rng, rng)
dists <- generatedists(ndists, meanrange, 1, 8, noise = FALSE)
# store means
meanstbl <- cbind(as.data.table(t(sapply(dists$params, function(x) c(x$means, distname = x$i)))), from = rep("generated", ndists), closest = 1:ndists)

#### test to see if dists can be told apart ####
testmatrix <- matrix(ncol = ndists, nrow = ndists)
for (i in 1:ndists) {
  for (j in 1:ndists) {
    testmatrix[i,j] <- ks.test(dists$mvns[[i]][,-ncol(dists$mvns[[i]])], dists$mvns[[j]][,-ncol(dists$mvns[[j]])])$p.value
  }
}
plot(dists$full[,-ncol(dists$full)], col=factor(dists$full[,ncol(dists$full)]))

#### construct RF ####
forest <- randomForest(dists$full[,-ncol(dists$full)], keep.forest=FALSE, proximity=TRUE)
forest$dissimilarity <- as.dist(1 - forest$proximity)

#### PAM ####
pam <- pam(forest$dissimilarity, ndists, diss = TRUE)
print("PAM Clustering Results")
table(pam$clustering, dists$full[,ncol(dists$full)])
pamparams <- findparams(dists$full[,-ncol(dists$full)], pam$clustering, ndists)
pammeanstbl <- cbind(as.data.table(t(sapply(pamparams, function(x) c(x$means, distname = x$i)))), from = rep("PAM", ndists))
pammeanstbl <- cbind(pammeanstbl, closest = findclosest(copy(pammeanstbl)[, c("distname", "from", "closest") := NULL], copy(meanstbl)[, c("distname", "from", "closest") := NULL]))

#### Hclust ####
hclus <- hclust(forest$dissimilarity, method = "ward.D2")
hclusters <- cutree(hclus, k=4)
print("Hierarchical Clustering Results")
table(hclusters, dists$full[,ncol(dists$full)])
hparams <- findparams(dists$full[,-ncol(dists$full)], hclusters, ndists)
hmeanstbl <- cbind(as.data.table(t(sapply(hparams, function(x) c(x$means, distname = x$i)))), from = rep("Hclust", ndists))
hmeanstbl <- cbind(hmeanstbl, closest = findclosest(copy(hmeanstbl)[, c("distname", "from", "closest") := NULL], copy(meanstbl)[, c("distname", "from", "closest") := NULL]))

#### K means ####
kmn <- kmeans(dists$full[,-ncol(dists$full)], centers = 4, nstart = 2)
print("K-Means Clustering Results")
table(kmn$cluster, dists$full[,ncol(dists$full)])
kmnparams <- findparams(dists$full[,-ncol(dists$full)], kmn$cluster, ndists)
kmnmeanstbl <- cbind(as.data.table(t(sapply(kmnparams, function(x) c(x$means, distname = x$i)))), from = rep("K means", ndists))
kmnmeanstbl <- cbind(kmnmeanstbl, closest = findclosest(copy(kmnmeanstbl)[, c("distname", "from", "closest") := NULL], copy(meanstbl)[, c("distname", "from", "closest") := NULL]))

comparemeanstbl <- rbind(meanstbl, pammeanstbl, hmeanstbl, kmnmeanstbl)
print("Comparison of Means")
comparemeanstbl

print("Comparison of Covariances")
covlist <- lapply(dists$params, function(x) x$covmat)
print("Original Cov. Matrix")
covlist

pamcovlist <- lapply(pamparams, function(x) x$covmat)
print("PAM Cov. Matrix")
print(paste("Refers to dists. in order of", paste(pammeanstbl$closest, collapse = " ")))
pamcovlist

hcovlist <- lapply(hparams, function(x) x$covmat)
print("Hier. Clust. Cov. Matrix")
print(paste("Refers to dists. in order of", paste(hmeanstbl$closest, collapse = " ")))
hcovlist

kmncovlist <- lapply(kmnparams, function(x) x$covmat)
print("K-means Cov. Matrix")
print(paste("Refers to dists. in order of", paste(kmnmeanstbl$closest, collapse = " ")))
kmncovlist