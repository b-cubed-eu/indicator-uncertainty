#' Perform bootstrapping for a calculated statistic
#'
#' A function which when applied to data returns a vector containing the
#' statistic(s) of interest.
#'
#' @param data The data as a dataframe. Each row is considered as one
#' multivariate observation.
#' @param indices A vector of indices which define the bootstrap sample.
#' @param fun A function which when applied to data returns the statistic(s) of
#' interest.
#'
#' @returns The returned value is an object of class `"boot"`. See
#' `boot::boot()`.

boot_statistic <- function(data, indices, fun) {
  d <- data[indices]
  return(fun(d))
}
