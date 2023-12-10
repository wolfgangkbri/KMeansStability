#' Compute stability distance metrics through cross validation
#'
#'@description `resample_cv` utilized cross-validation to emulate resampling
# to estimate the mean stability metrics for Kmeans clustering for K set to 2 through `Kmax`.
#'@param df A data matrix
#'@param K_folds Integer for number of folds used in cross-validation. Defaulted to four.
#'@param Kmax Maximum number of clusters considered. Defaulted to 6.
#' @export
resample_cv <- function(df, K_folds = 4, Kmax = 6){
  n <- nrow(df)
  folds <- (sample(1:n)) %% (K_folds) + 1
  dist_array <- array(dim = c(Kmax - 1, 5, K_folds))

  for (fold in 1:K_folds){

    holdout_bool <- fold == folds
    training <- df[-holdout_bool, ]
    testing <- df[holdout_bool, ]

    dist_array[,,fold] <- compute_distances(training, testing, Kmax)
  }

  return(dist_array)
}

#' Compute stability distance metrics through bootstrapping
#'
#'@description `resample_boot` utilizes bootstrapping to emulate resampling
# to estimate the mean stability metrics for Kmeans clustering for K set to 2 through `Kmax`.
# One dataset is constructed through bootstrapping the given dataset `df`.
# The other dataset for which stability is computed across is formed by the out-of-bag observations.
#'@param df A data matrix
#'@param B Integer for number of bootstrap sampled. Defaulted to 25.
#'@param Kmax Maximum number of clusters considered. Defaulted to 6.
#'@export
resample_boot <- function(df, B = 25, Kmax = 6){
  n <- nrow(df)
  dist_array <- array(dim = c(Kmax - 1, 5, B))

  for (i in 1:B){
    print(i)
    boot_indicies <- sample(1:n, n, replace = T)
    oob_indicies <- setdiff(1:n, boot_indicies)
    training <- df[boot_indicies, ]
    testing <- df[oob_indicies, ]

    dist_array[,,i] <- compute_distances(training, testing, Kmax)
  }

  return(dist_array)
}

#' Compute stability distance metrics through CV over null clustering distribution
#'
#' @description `null_resample_cv` operated the same as `resample_cv`, but evaluates Kmeans
#' over samples fit from a uniform distribution over the principle components of
#' the original dataset`df`, following the null distribution from the Gap Statistic.
#' For each sample taken from the null distribution, the sample is cut in proportions
#'  matching that of that of K-fold cross validation used to compute the stability
#'  metrics of the raw dataset.
#'
#' @param `df` input dataset matrix
#' @param nreps Number of samples taken from the null distribution
#' @param Kmax Integer for maximum number of clusters considered
#' @param Kfolds Number of folds used in cross-validation
#' @param nreps number of null samples taken
#' @export
null_resample_cv <- function(df, Kmax, Kfolds, nreps = 20){
  xs <- scale(df, center = TRUE, scale = FALSE)
  n <- nrow(df)
  m.x <- rep(attr(xs, "scaled:center"), each = n)
  V.sx <- svd(xs, nu = 0)$v
  xs <- xs %*% V.sx
  rng.x1 <- apply(xs, 2L, range)


  n_training <- floor(n * (Kfolds - 1) / Kfolds)
  n_testing <- n - n_training

  #bootapply <- ifelse(parallel, function(...){mclapply(..., mc.cores = 7)}, lapply)

  bootapply <- lapply

  nsl <- bootapply(1:nreps, function(i){
    z1 <- apply(rng.x1, 2, function(M, nn) runif(nn, min = M[1],
                                                 max = M[2]), nn = n)
    z <- tcrossprod(z1, V.sx) + m.x
    compute_distances(z[1:n_training,], z[(n_training+1):n,], Kmax)
  })
  nsl
}

#' Compute stability distance metrics through bootstrapping over null clustering distribution
#'
#' @description `null_resample_boot` operated the same as `resample_boot`, but evaluates Kmeans
#' over samples fit from a uniform distribution over the principle components of
#' the original dataset`df`, following the null distribution from the Gap Statistic.
#' For each sample taken from the null distribution, the sample is split into a bootstrapped
#' sample and out-of-bag sample matching the computations done in `resample_boot`.
#'
#'  @param `df` input dataset matrix
#'  @param nreps Number of samples taken from the null distribution
#'  @param Kmax Maximum number of clusters considered.
#'  @param nerps number of null samples taken
#'  @export
null_resample_bootstrap <- function(df, Kmax, nreps = 20){
  xs <- scale(df, center = TRUE, scale = FALSE)
  n <- nrow(df)
  m.x <- rep(attr(xs, "scaled:center"), each = n)
  V.sx <- svd(xs, nu = 0)$v
  xs <- xs %*% V.sx
  rng.x1 <- apply(xs, 2L, range)

  #bootapply <- ifelse(parallel, function(...){mclapply(..., mc.cores = 7)}, lapply)

  bootapply <- lapply

  nsl <- bootapply(1:nreps, function(i){
    z1 <- apply(rng.x1, 2, function(M, nn) runif(nn, min = M[1],
                                                 max = M[2]), nn = n)
    z <- tcrossprod(z1, V.sx) + m.x
    boot_indicies <- sample(1:n, n, replace = T)
    oob_indicies <- sample(setdiff(1:n, boot_indicies))
    compute_distances(z[boot_indicies,], z[oob_indicies,], Kmax)
  })
  nsl
}

#' Stability Analysis for Kmeans Clustering
#'
#' @description `stability_analysis` computes a set of stability metrics to estimate the most stable Kmeans clustering
#' Resampling approximations include K-fold cross validation and bootstrapping.
#' A clustering attaining the smallest instability is deemed the most stable clustering.
#' @param df Data matrix to be clustered. Each row is an observation, each column is a variable.
#' @param Kmax Integer for maximum number of clusters used by Kmeans. Larger values slow down computation. Defaulted to 6.
#' @param method String that is either "CV" or "boot" to select cross-validation or bootstrap resampling.
#' Defaulted to "CV", as bootstrapping often takes a larger number of samples to yield adequate performance.
#' @param nreps Integer for number of resamples to be taken.
#' If method is 'CV', `nreps` is the number of folds.
#' If method is 'boot', `nreps` is the number of bootstrap resamples taken.
#' @param null_dist Boolean for whether the stability metrics should be compared to metrics taken from
#' a data generating process with no clusters. Defaulted to 'False', as this is very computationally intensive.
#' @param nreps_null Integer for number of null samples taken in `null_dist` is set to True. Defaulted to 30 but more are recommended.
#'
#' @details `stability_analysis` will compute five stability metrics for Kmeans clusterings for K ranging from two to `Kmax`.
#' Two metrics are cannon in the Kmeans stability literature: co-membership distance, and minimum matching distance.
#' The three other metrics are inspired by optimal transport distance. They are
#' (1) `wd` the minimal matching cost of the wasserstein distances for datapoints within two clusterings,
#' (2) `wd_cb` the Wasserstein distance for Kmeans centroids where each center is given uniform mass, and
#' (3) `wd_cp` ,the Wasserstein distance for Kmeans centroids where each center is given mass proportional to the number of observations within the associated cluster.
#'
#' For all of these metrics, the number of clusters which attains the smallest value of each of these metrics is the clustering deemed most stable.
#' These metrics are computed through the the aid of the packages `transport` and `RcppHungarian`.
#'
#' If `null_dist` is set to true,the five distance metrics are computed across clusterings of samples taken from a uniform distribution aligned to the priciple components of `df`.
#' Following the Gap Statistic, these valuesoperate as baseline values for the metrics under the setting of no clustering.
#' One may select the clustering which attains the largest difference in observed stability metric and the expected null metric to be the most stable clustering.
#'
#'@return An object of class `Kmeans_stability_analysis` which contains the mean stability metrics computed over `df`,
#' and a list of the observed stabiltiy metrics across each resample.
#' If `null_dist` is set to true, the mean stability for the null samples, the associated gao statistics, and the raw stability metrics for each null sample will also be returned.
#' Otherwise, those components of the `Kmeans_stability_analysis` object will be set to `NA`.
#' @export
stability_analysis <- function(df, Kmax = 6, method = "CV", nreps = 4, null_dist = F, nreps_null = 30){

  if(! method %in% c("CV", "boot")){stop("method must be either 'CV' or 'boot'.")}


  if(method == "CV"){
    dist_array <- resample_cv(df, K_folds = nreps, Kmax)
  }

  if(method == "boot"){
    dist_array <- resample_boot(df, B = nreps, Kmax)
  }

  if(nreps < 2 & method == "CV"){stop("nreps must be at least 2 if using cross-validation")}
  if(nreps < 1 & method == "boot"){stop("nreps must be at least one if using bootstrapping.")}

  if(Kmax < 2){stop("Kmax must be at least two")}

  mean_stabilities <- apply(dist_array, 2, rowMeans)
  dimnames(mean_stabilities) <- list(K = 2:Kmax, stab_metric = c("wd", "wd_cb", "wd_cp", "mmd", "cd"))

  if(null_dist){
    if(method == "boot"){
      nsl <- null_resample_bootstrap(df, Kmax = Kmax,  nreps_null)
    } else {

      nsl <- null_resample_cv(df, Kmax = Kmax, Kfolds = nreps, nreps = nreps_null)
    }
    mean_stabilities_null <- apply(array(as.numeric(unlist(nsl)), dim=c(Kmax - 1, 5, nreps_null)), 2, rowMeans)
    gap <- mean_stabilities_null - mean_stabilities
  } else {
    mean_stabilities_null <- gap <- nsl <- NA
  }

  out <- list(mean_stabilities = mean_stabilities,
              obs_stability_array = dist_array,
              mean_stabilities_null = mean_stabilities_null,
              null_stabilities_list = nsl,
              mean_stabilities_gap = gap
              )
  class(out) <- "Kmeans_stability_analysis"
  return(out)
}

