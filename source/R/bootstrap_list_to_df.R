#' Convert list of `boot` objects to dataframe
#'
#' This function converts a list of objects of class `"boot"` per time point
#' into a dataframe containing all required summaries.
#'
#' @param bootstrap_list A list of objects of class `"boot"` per time point.
#' @param temporal_list_name The temporal list names of `bootstrap_list`
#' (e.g., year, month ...) containing time point values. Default `year`.
#'
#' @returns The returned value is a dataframe containing the bootstrap sample
#' index (`sample`), the time point column (e.g. `year`), the bootstrap estimate
#' of the statistic (`est_boot`), the original sample estimate of the statistic
#' (`est_original`), the standard deviation of the bootstrap replications
#' (`se_boot`), and the bootstrap bias (`bias_boot`).

bootstrap_list_to_df <- function(bootstrap_list, temporal_list_name = "year") {
  require("dplyr")
  require("rlang")

  bootstrap_data_df <- sapply(bootstrap_list, function(df) df$t) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "sample") %>%
    tidyr::pivot_longer(cols = -sample,
                        values_to = "est_boot",
                        names_to = temporal_list_name) %>%
    dplyr::mutate(sample = as.numeric(sample))

  bootstrap_summaries <- data.frame(
    temp_col = names(bootstrap_list),
    est_original = sapply(bootstrap_list, function(df) df$t0),
    se_boot = sapply(bootstrap_list, function(df) stats::sd(df$t))
  )

  bootstrap_data_full <- bootstrap_data_df %>%
    dplyr::full_join(
      bootstrap_summaries,
      by = dplyr::join_by(!!temporal_list_name == "temp_col")
    ) %>%
    dplyr::arrange(.data$sample, .data[[temporal_list_name]]) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      diff = .data$est_boot - .data$est_original,
      {{ temporal_list_name }} := as.numeric(.data[[temporal_list_name]])) %>%
    dplyr::group_by(.data[[temporal_list_name]]) %>%
    dplyr::mutate(bias_boot = mean(.data$diff)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"diff")

  return(bootstrap_data_full)
}
