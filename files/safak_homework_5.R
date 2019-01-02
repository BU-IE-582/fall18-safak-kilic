require(data.table)
require(cluster)
require(glmnet)

setwd("~/Desktop/okul/Data Mining/data/")
# read data
musk <- fread("Musk1.csv")

# separate data into labels and feature info
musknames <- musk[, c(1, 2)]
setnames(musknames, c("bagclass", "bagid"))

muskfeatures <- musk[, 3:ncol(musk)][, lapply(.SD, scale)]

# this function uses clustering methods to collapse multiple-instance data into bag-level data
# and also runs a 10-fold CV lasso training on the dataset
mil.analysis <- function(clusts, clutype, disttype, names, features) {
  
  # depending on supplied distance type, find distance matrix
  distmat <- dist(features, method = disttype)
  
  # depending on supplied clustering type, carry out clustering
  if (clutype == "pam") {
    clusters <- pam(distmat, clusts, diss = TRUE)$clustering  
  }
  else {
    clusters <- cutree(hclust(distmat, method = "ward.D2"), k = clusts)
  }
  # bind cluster info to features
  clustered <- cbind(features, clusters)     
  # find cluster centroids
  clustcentros <- copy(clustered)[, lapply(.SD, mean), by = clusters] 
  # find distance of each instance from centroids
  clustcentrodist <- as.matrix(dist(rbind(copy(clustcentros)[, clusters := NULL], copy(clustered)[, clusters := NULL]), method = disttype))
  clustcentrodist <- as.data.table(clustcentrodist[(clusts+1):nrow(clustcentrodist), 1:clusts])
  
  # find mean to-centroid distances for each bag, merge with bag classes
  bagdata <- unique(merge(names, cbind(bagid = names$bagid, clustcentrodist)[, lapply(.SD, mean), by = bagid], by = "bagid"))[, bagid := NULL]
  
  # 10-fold CV with lasso penalties
  cvfit <- cv.glmnet(x = as.matrix(copy(bagdata)[, bagclass := NULL]), y = as.factor(bagdata$bagclass), nfolds = 10, type.measure = "auc", family = "binomial")
  
  # returns a line of data so a table can be constructed
  return(data.table(clusts = clusts, clutype = clutype, disttype = disttype, cvminlambda = cvfit$lambda.min, cvminerror = min(cvfit$cvm)))
}

# range of cluster numbers to examine
nrange <- 2:20
# create empty table and populate within for loop
resultstable <- data.table()
for (i in nrange) {
  resultstable <- rbind(resultstable, mil.analysis(i, "pam", "euclidean", musknames, muskfeatures))
  resultstable <- rbind(resultstable, mil.analysis(i, "hclus", "euclidean", musknames, muskfeatures))
  resultstable <- rbind(resultstable, mil.analysis(i, "pam", "manhattan", musknames, muskfeatures))
  resultstable <- rbind(resultstable, mil.analysis(i, "hclus", "manhattan", musknames, muskfeatures))
}

resultstable[order(resultstable$cvminerror)]





