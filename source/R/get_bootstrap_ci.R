#' Calculate confidence intervals for list of `boot` objects
#'
#' This function calculates confidence intervals for a list of objects of class
#' `"boot"` per year into a dataframe containing all required summaries.
#'
#' @param bootstrap_samples_df A dataframe containing the bootstrap samples.
#' @param grouping_var ...
#' @param type A vector of character strings representing the type of intervals
#' required. The value should be any subset of the values
#' `c("perc", "bca", "norm", "basic")` or simply `"all"` (default) which will
#' compute all types of intervals.
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

  # Check if type is correct
  type <- tryCatch({
    match.arg(type, c("perc", "bca", "norm", "basic", "all"))
  }, error = function(e) {
    stop("`type` must be one of 'perc', 'bca', 'norm', 'basic'.",
         call. = FALSE)
  })

  alpha <- (1 - conf) / 2

  if (type == "perc") {
    conf_df <- bootstrap_samples_df %>%
      mutate(
        int_type = type,
        ll = stats::quantile(.data$rep_boot, probs = alpha),
        ul = stats::quantile(.data$rep_boot, probs = 1 - alpha),
        conf_level = conf,
        .by = all_of(grouping_var))
  } else if (type == "bca") {
    stop("bca not implemented yet")
  } else if (type == "norm") {
    conf_df <- bootstrap_samples_df %>%
      mutate(
        int_type = type,
        ll = .data$est_original + qnorm(alpha) * .data$se_boot,
        ul = .data$est_original + qnorm(1 - alpha) * .data$se_boot,
        conf_level = conf,
        .by = all_of(grouping_var))
  } else if (type == "basic") {
    conf_df <- bootstrap_samples_df %>%
      mutate(
        int_type = type,
        lower_quantile = stats::quantile(.data$rep_boot, probs = alpha),
        upper_quantile = stats::quantile(.data$rep_boot, probs = 1 - alpha),
        ll = 2 * .data$est_original - upper_quantile,
        ul = 2 * .data$est_original - lower_quantile,
        conf_level = conf,
        .by = all_of(grouping_var)) %>%
      select(-ends_with("quantile"))
  } else {
    stop("'all' not implemented yet")
  }

  if (aggregate) {
    conf_df_out <- conf_df %>%
      select(-c("sample", "rep_boot")) %>%
      distinct()
  } else {
    conf_df_out <- conf_df
  }

  return(conf_df_out)
}
