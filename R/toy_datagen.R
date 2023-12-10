#' Toy Data Generating Process for Three Clusters
#'
#' @description `toy_datagen` constructs a two-dimensional data as a testing dataset.
#'
#' @param ns A numeric vector of length three. Each component is the number
#' of samples in one of the three clusters. Defaulted to c(100, 60, 40).
#' @return A `sum(ns)` x 2 matrix.
toy_datagen <- function(ns = c(100, 60, 40)){
  rbind(
    MASS::mvrnorm(ns[1], rep(0, 2), diag(1, 2)),
    MASS::mvrnorm(ns[2], c(5,1), diag(0.5, 2)),
    MASS::mvrnorm(ns[3], c(0,5), diag(0.7,2)))
}
