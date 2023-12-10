#' A Modified Version of `wpp`.
#'
#' @description `wpp_modified` is a stripped down version of `wpp` from the `transport` package
#' designed to allow construction of `wpp.object` objects which are a single point mass.
#' The current implementation of `wpp` prohibits this, but this is necessary for
#' computing `wd` when a single point is assigned its own cluster. See `wpp` in the `transport` package for more details.
#'
#' @param coordinates A data matrix.
#' @param mass A vector of masses.
#' @return A `wpp` object.
#' @export
wpp_modified <- function (coordinates, mass)
{
  coordinates <- as.matrix(coordinates)
  NN <- dim(coordinates)[1]
  stopifnot(length(mass) == NN)
  dimension <- dim(coordinates)[2]
  totmass <- sum(mass)
  stopifnot(all(mass >= rep(0, NN)))
  stopifnot(totmass > 0)
  # massnonzero <- (mass != 0)
  # mass <- mass[massnonzero]
  # zeropoints <- coordinates[!massnonzero, ]
  # coordinates <- coordinates[massnonzero, ]
  #
  N <- dim(coordinates)[1]
  res <- list(dimension = dimension, N = N, coordinates = coordinates,
              mass = mass, totmass = totmass)
  class(res) <- "wpp"
  # if (N < NN) {
  #   attr(res, "zeropoints") <- zeropoints
  # }
  return(res)
}
