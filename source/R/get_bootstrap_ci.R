#' Calculate confidence intervals for list of `boot` objects
#'
#' This function calculates confidence intervals for a list of objects of class
#' `"boot"` per year into a dataframe containing all required summaries.
#'
#' @param bootstrap_samples_df A dataframe containting the bootstrap samples.
#' @param grouping_var ...
#' @param type A vector of character strings representing the type of intervals
#' required. The value should be any subset of the values c("perc", "bca") or
#' simply "all" (default) which will compute both types of intervals.
#' @param conf A scalar or vector containing the confidence level(s) of the
#' required interval(s). Default 0.95.
#' @param aggregate ...
#'
#' @returns The returned value is a dataframe containing the time point,
#' the type of interval (`int_type`), the lower limit of the confidence
#' interval (`ll`), the upper limit of the confidence interval (`ul`), and the
#' confidence level of the intervals (`conf_level`).

get_bootstrap_ci <- function(
    bootstrap_samples_df,
    grouping_var,
    type = c("perc", "bca"),
    conf = 0.95,
    aggregate = TRUE) {
  require("dplyr")
  require("rlang")

  # Check if crossv_method is loo or kfold
  type <- tryCatch({
    match.arg(type, c("perc", "bca"))
  }, error = function(e) {
    stop("`type` must be one of 'perc', 'bca'.",
         call. = FALSE)
  })


  return(conf_df_out)
}
