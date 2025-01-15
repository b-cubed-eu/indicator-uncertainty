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
  stopifnot("`type` must be one of 'perc', 'bca', 'norm', 'basic'." =
              all(is.element(type, c("perc", "bca", "norm", "basic"))))

  # Calculate intervals
  alpha <- (1 - conf) / 2

  out_list <- vector(mode = "list", length = length(type))
  for (i in seq_along(type)) {
    t <- type[i]

    if (t == "perc") {
      # Calculate confidence limits per group
      intervals_list <- bootstrap_samples_df %>%
        split(bootstrap_samples_df[[grouping_var]]) %>%
        lapply(function(df) {
          replicates <- df$rep_boot
          boot:::perc.ci(t = replicates, conf = conf)
        })

      # Combine confidence levels in dataframe
      intervals_df <- do.call(rbind.data.frame, intervals_list) %>%
        mutate(group = unique(bootstrap_samples_df[[grouping_var]])) %>%
        rename("ll" = "V4", "ul" = "V5") %>%
        select("group", "ll", "ul", "conf")

      # Join with input data
      conf_df <- bootstrap_samples_df %>%
        mutate(int_type = t) %>%
        left_join(intervals_df, by = join_by(!!grouping_var == "group"))
    }
    if (t == "bca") {
      stop("bca not implemented yet")
    }
    if (t == "norm") {
      # Calculate confidence limits per group
      intervals_list <- bootstrap_samples_df %>%
        split(bootstrap_samples_df[[grouping_var]]) %>%
        lapply(function(df) {
          estimate <- unique(df$est_original)
          replicates <- df$rep_boot
          boot:::norm.ci(t0 = estimate, t = replicates, conf = conf)
        })

      # Combine confidence levels in dataframe
      intervals_df <- do.call(rbind.data.frame, intervals_list) %>%
        mutate(group = unique(bootstrap_samples_df[[grouping_var]])) %>%
        rename("ll" = "V2", "ul" = "V3") %>%
        select("group", "ll", "ul", "conf")

      # Join with input data
      conf_df <- bootstrap_samples_df %>%
        mutate(int_type = t) %>%
        left_join(intervals_df, by = join_by(!!grouping_var == "group"))
    }
    if (t == "basic") {
      # Calculate confidence limits per group
      intervals_list <- bootstrap_samples_df %>%
        split(bootstrap_samples_df[[grouping_var]]) %>%
        lapply(function(df) {
          estimate <- unique(df$est_original)
          replicates <- df$rep_boot
          boot:::basic.ci(t0 = estimate, t = replicates, conf = conf)
        })

      # Combine confidence levels in dataframe
      intervals_df <- do.call(rbind.data.frame, intervals_list) %>%
        mutate(group = unique(bootstrap_samples_df[[grouping_var]])) %>%
        rename("ll" = "V4", "ul" = "V5") %>%
        select("group", "ll", "ul", "conf")

      # Join with input data
      conf_df <- bootstrap_samples_df %>%
        mutate(int_type = t) %>%
        left_join(intervals_df, by = join_by(!!grouping_var == "group"))
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
