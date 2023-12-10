#' Compute Distance Metrics over K-Means clusterings
#'
#' @description `compute_distances` computes a set of stability metrics for
#' clusterings over two datasets, `training` and `testing`. For each number of clusters two through `Kmax`,
#' each dataset is clustered with `kmeans` and a set of stability distance metrics are computed over the
#' two clusterings. Each metric is designed such that the most stable clustering minimumises the metric.
#' The number of clusters `K` is determined from some selection rule applied over some aggregation of distance metrics
#' computed over some resampling method.
#'
#' @param training An nxp matrix of training data
#' @param testing An mxp matrix of testing data
#' @param Kmax The maximum number of clusters considered
#' @return An (Kmax - 1) x 5 matrix of stability metrics.
#' Each row corresponds to a value of K. Each column corresponds to a particular metric.
#' @export
compute_distances <- function(training, testing, Kmax = 6){

  wass_dists <- rep(-1, Kmax - 1)
  mmd_dists <- rep(-1, Kmax - 1)
  cd_dists <- rep(-1, Kmax - 1)
  wass_centers_balanced_dists <- rep(-1, Kmax - 1)
  wass_centers_prop_dists <- rep(-1, Kmax - 1)

  for(K in 2:Kmax){

    clustering_training <- kmeans(training, K)
    clustering_testing <- kmeans(testing, K)
    wass_dists[K - 1] <- wd(K, training, testing, clustering_training$cluster, clustering_testing$cluster)

    wass_centers_balanced_dists[K-1 ] <- wd_cb(K, clustering_training, clustering_testing)
    wass_centers_prop_dists[K-1 ] <- wd_cp(K, clustering_training, clustering_testing)

    # For MMD, compute estimated clustering on "training data"
    pred_cluster_testing <- getClusterAssignment(testing, clustering_training$centers, K)

    # get distance between actual clustered values versus predicted cluster values
    mmd_dists[K - 1] <- mmd(clustering_testing$cluster, pred_cluster_testing, K)
    cd_dists[K - 1] <- cd(clustering_testing$cluster, pred_cluster_testing)
  }

  # gap_statistics <- clusGap(training, kmeans, Kmax, verbose = F)$Tab[,3]
  matrix(c(wass_dists,
           wass_centers_balanced_dists,
           wass_centers_prop_dists,
           mmd_dists,
           cd_dists),
         Kmax - 1,
         5)
}

#' Get KMeans Cluster Assignment for new observations
#'
#' @description `getClusterAssignment` computes the cluster assignments for a
#'  matrix of new observations, given a set of Kmeans centroids
#'  @param X data matix for which to compute cluster assignments
#'  @param M Kmeans centroids matrix
#'  @param K integer for number of centroids.
#'  @return vector of cluster indicies, assuming Kmeans clustering utlizes 1:K as cluster labels.
getClusterAssignment <- function(X, M, K){
  n <- nrow(X)
  # building an n x K matrix with i,jth value being
  # 2(x_i - center_j)^t(x_i - center_j) - center_j^center_j
  # and taking the columnwise maximum through `max.col` to select the closest center for each x_i.
  #
  # outputs a vector of indices where each index is the index of the column with the obtained maximum.
  #
  # for each row, the center which maximizes this expression is the center
  # which minimizes the squared Euclidean distance to x_i.

  max.col(2 * tcrossprod(X, M) - matrix(rep(rowSums(M^2), each = n), n, K))
}
