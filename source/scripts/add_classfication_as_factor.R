#' Classify effects by comparing the confidence intervals with a reference and
#' thresholds as factor variables
#'
#' This function classify effects by comparing the confidence intervals with a
#' reference and thresholds. A wrapper around `effectclass::classify()` that
#' adds effect descriptions as factor variables.
#'
#' @param df A dataframe containing summary data of confidence limits.
#' @param ... Additional argument to be passed to the `effectclass::classify()`
#' function.
#'
#' @returns The returned value is a list of objects of class `"boot"` per year.
#' See `boot::boot()`.

add_classification_as_factor <- function(df, ...) {

}