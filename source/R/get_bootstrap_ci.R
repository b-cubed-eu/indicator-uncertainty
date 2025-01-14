#' Calculate confidence intervals for list of `boot` objects
#'
#' This function calculates confidence intervals for a list of objects of class
#' `"boot"` per year into a dataframe containing all required summaries.
#'
#' @param bootstrap_samples_df A dataframe containing the bootstrap samples.
#' @param grouping_var ...
#' @param type A vector of character strings representing the type of intervals
#' required. The value should one or more of the following
#' `c("perc", "bca", "norm", "basic")`. Default is `"perc"`.
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
    type = c("perc", "bca", "norm", "basic"),
    conf = 0.95,
    aggregate = TRUE) {
  require("dplyr")
  require("rlang")

  # Check if type is correct
  type <- tryCatch({
    match.arg(type, c("perc", "bca", "norm", "basic"))
  }, error = function(e) {
    stop("`type` must be one of 'perc', 'bca', 'norm', 'basic'.",
         call. = FALSE)
  })

  alpha <- (1 - conf) / 2

  out_list <- vector(mode = "list", length = length(type))
  for (i in seq_along(type)) {
    t <- type[i]

    if (t == "perc") {
      conf_df <- bootstrap_samples_df %>%
        mutate(
          int_type = t,
          ll = stats::quantile(.data$rep_boot, probs = alpha),
          ul = stats::quantile(.data$rep_boot, probs = 1 - alpha),
          conf_level = conf,
          .by = all_of(grouping_var))
    }
    if (t == "bca") {
      stop("bca not implemented yet")
    }
    if (t == "norm") {
      conf_df <- bootstrap_samples_df %>%
        mutate(
          int_type = t,
          ll = .data$est_original + qnorm(alpha) * .data$se_boot,
          ul = .data$est_original + qnorm(1 - alpha) * .data$se_boot,
          conf_level = conf,
          .by = all_of(grouping_var))
    }
    if (t == "basic") {
      conf_df <- bootstrap_samples_df %>%
        mutate(
          int_type = t,
          lower_quantile = stats::quantile(.data$rep_boot, probs = alpha),
          upper_quantile = stats::quantile(.data$rep_boot, probs = 1 - alpha),
          ll = 2 * .data$est_original - upper_quantile,
          ul = 2 * .data$est_original - lower_quantile,
          conf_level = conf,
          .by = all_of(grouping_var)) %>%
        select(-ends_with("quantile"))
    }

    out_list[[i]] <- conf_df
  }

  conf_df_full <- bind_rows(out_list)

  if (aggregate) {
    conf_df_out <- conf_df_full %>%
      select(-c("sample", "rep_boot")) %>%
      distinct()
  } else {
    conf_df_out <- conf_df_full
  }

  return(conf_df_out)
}
