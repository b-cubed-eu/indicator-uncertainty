#' Perform bootstrapping for a calculated statistic over time
#'
#' This function generate `samples` bootstrap replicates of a statistic applied
#' to a data cube per time point (e.g., year, month ...).
#'
#' @param data_cube_df A dataframe containing data in biodiversity data cube
#' format.
#' @param fun A function which when applied to data returns the statistic(s) of
#' interest.
#' @param samples The number of bootstrap replicates. A single positive integer.
#' @param ref_group A string indicating the reference time point to compare the
#' statistic. Default `NA`, no reference time point is used.
#' @param temporal_col_name The temporal column name of `data_cube_df`
#' (e.g., year, month ...) containing time point values. Default `year`.
#' @param seed The seed for random number generation to make results
#' reproducible.
#'
#' @returns The returned value is a list of objects of class `"boot"` per time
#' point. See `boot::boot()`.

perform_bootstrap_ts <- function(
    data_cube_df,
    fun,
    samples = 1000,
    ref_group = NA,
    temporal_col_name = "year",
    seed = 123) {
  require("dplyr")
  require("rlang")

  withr::local_seed(seed)

  if (is.na(ref_group)) {
    # Summarise data by temporal column
    bootstrap_list <- data_cube_df %>%
      dplyr::summarize(num_occ = sum(.data$obs),
                       .by = dplyr::all_of(c(temporal_col_name, "taxonKey"))
                       ) %>%
      dplyr::arrange(.data[[temporal_col_name]]) %>%
      tidyr::pivot_wider(names_from = temporal_col_name,
                         values_from = "num_occ",
                         values_fill = 0) %>%
      tibble::column_to_rownames("taxonKey") %>%
      as.list() %>%
      # Perform bootstrapping
      purrr::map(~boot::boot(
        data = .,
        statistic = boot_statistic,
        R = samples,
        fun = fun))
  } else {
    # Summarise data by temporal column
    sum_data_list <- data_cube_df %>%
      dplyr::summarize(num_occ = sum(.data$obs),
                       .by = dplyr::all_of(c(temporal_col_name, "taxonKey"))
                       ) %>%
      dplyr::arrange(.data[[temporal_col_name]]) %>%
      tidyr::pivot_wider(names_from = temporal_col_name,
                         values_from = "num_occ",
                         values_fill = 0) %>%
      tibble::column_to_rownames("taxonKey") %>%
      as.list()

    # Perform bootstrapping
    bootstrap_list <- sum_data_list[
        setdiff(names(sum_data_list), as.character(ref_group))
      ] %>%
      purrr::map(~boot::boot(
        data = .,
        statistic = boot_statistic_diff,
        R = samples,
        fun = fun,
        ref_data = sum_data_list[[as.character(ref_group)]]))
  }

  return(bootstrap_list)
}
