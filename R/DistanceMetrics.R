#' Comembership Distance
#'
#' @description `cd` returns the comembership distance between two clusterings
#' of the same dataset. Each clustering must contain the same amount of clusters.
#'
#'
#' @param c1 A numeric vector of cluster labels
#' @param c2 A numeric vector of cluster labels
#' @return A numeric of the comembership distance between the two clusters
#' @export
cd <- function(c1, c2){
  mean(abs(outer(c1,c1,"!=") - outer(c2,c2,"!=")))
}


#' Minimum Matching Distance
#'
#' @description `mmd` returns the comembership distance between two clusterings
#' of the same dataset. Each clustering must contain the same amount of clusters.
#'
#' @param K Integer for number of clusters
#' @param c1 A numeric vector of cluster labels
#' @param c2 A numeric vector of cluster labels
#' @return A numeric of the minimum matching distance between the two clusters
#' @export
mmd <- function(c1, c2, K){
  cost_matrix <- matrix(-1, K, K)
  for (i in 1:K){
    for (j in 1:K){
      cost_matrix[i,j] <- sum(c1 == i & c2 == j)
    }
  }

  pairs <- RcppHungarian::HungarianSolver(-cost_matrix)$pairs

  min_mismatch <- rep(-1, K)
  for (i in 1:K){
    min_mismatch[i] <- sum(c1 == i & c2 != pairs[i,2])
  }
  sum(min_mismatch) / length(c1)
}

#' Minimum Wasserstein Distance
#'
#' @description `wd` computes the minimum matching distance between two clusterings
#' of two distinct datasets. The optimal transport distance between points within
#' each pair of clusters is computed, from which the optimal matching of clusters is computed.
#'
#' @param K an integer for of clusters within both provided clusterings
#' @param training A matrix containing the data which generated the `ctrain` clustering vector
#' @param testing A matrix containing the data which generated the `ctest` clustering vector
#' @param ctrain A numeric vector containing the clustering of the `training` dataset
#' @param ctest A numeric vector containing the clustering of the `testing` dataset
#' @return A numeric of the minimum wasserstein distance.
#' @export
wd <- function(K, training, testing, ctrain, ctest){

  cost_matrix <- matrix(-1, K, K)
  for(i in 1:K){
    for (j in 1:K){

      d1 <- training[ctrain == i,]
      d2 <- testing[ctest == j,]

      n1 <- nrow(d1)
      n2 <- nrow(d2)

      #nrow outputs NULL if di is a vector
      if(is.null(n1)){
        n1 <- 1
        d1 <- matrix(d1, 1, length(d1))
      }

      if(is.null(n2)){
        n2 <- 1
        d2 <- matrix(d2, 1, length(d2))
      }

      mass1 <- rep(1/n1, n1)
      mass2 <- rep(1/n2, n2)

      o1 <- wpp_modified(d1, mass = mass1)
      o2 <- wpp_modified(d2, mass = mass2)

      cost_matrix[i,j] <- transport::wasserstein(o2, o1, p = 1)
    }
  }
  RcppHungarian::HungarianSolver(cost_matrix)$cost
}

#' Minimum Wasserstein Distance over Cluster Centers with Uniform Mass
#'
#' @description `Wd_cd` computes the wasserstein distance between K_means cluster
#' centroids. The cluster centroids are given uniform mass before matching.
#'
#' @param K Integer for number of clusters
#' @param cluster_training A `kmeans` return object built from clustering
#' a training dataset with `K` clusters
#' #' @param cluster_testing A `kmeans` return object built from clustering
#' a testing dataset with `K` clusters
#' @return A numeric of the wasserstein distance between the centroids of the two clusterings
#' @export
wd_cb  <- function(K, cluster_training, cluster_testing){

  # mass of centers is uniform, regardless of number of obs within each cluster
  mass_training <- rep(1/K, K)
  mass_testing <- rep(1/K, K)

  training_centers <- transport::wpp(cluster_training$centers, mass_training)
  test_centers  <- transport::wpp(cluster_testing$centers,mass_testing)

  transport::wasserstein(training_centers, test_centers)
}

#' Minimum Wasserstein Distance over Cluster Centers with Proportional Mass
#'
#' @description `Wd_cp` computes the wasserstein distance between K_means cluster
#' centroids. The cluster centroids are given mass proportional to the number of
#observations within each cluster before matching.
#'
#' @param K Integer for number of clusters
#' @param cluster_training A `kmeans` return object built from clustering
#' a training dataset with `K` clusters
#' #' @param cluster_testing A `kmeans` return object built from clustering
#' a testing dataset with `K` clusters
#' @return A numeric of the wasserstein distance between the centroids of the two clusterings
#' @export
wd_cp <- function(K, cluster_training, cluster_testing){
  training_counts <- table(cluster_training$cluster)
  testing_counts <- table(cluster_testing$cluster)

  # mass of centers is proportionate to the number of observations within the cluster
  mass_training <- training_counts / sum(training_counts)
  mass_testing <-  testing_counts / sum(testing_counts)

  training_centers <- transport::wpp(cluster_training$centers, mass_training)
  test_centers  <- transport::wpp(cluster_testing$centers,mass_testing)

  transport::wasserstein(training_centers, test_centers)
}
