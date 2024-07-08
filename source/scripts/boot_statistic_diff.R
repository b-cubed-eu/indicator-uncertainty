#' Perform bootstrapping for a difference in a calculated statistic
#'
#' A function which when applied two datasets returns a vector containing the
#' difference in statistic(s) of interest.
#'
#' @param data The data as a dataframe. Each row is considered as one
#' multivariate observation.
#' @param ref_data The reference data as a dataframe. Each row is considered as
#' one multivariate observation.
#' @param indices A vector of indices which define the bootstrap sample.
#' @param fun A function which when applied to data returns the statistic(s) of
#' interest.
#'
#' @returns The returned value is an object of class `"boot"`. See
#' `boot::boot()`.

boot_statistic <- function(data, ref_data, indices, fun) {
  stat <- fun(data[indices])
  ref_stat <- fun(ref_data[indices])

  return(stat - ref_stat)
}
