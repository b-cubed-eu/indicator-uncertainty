#' Perform bootstrapping per year for a calculated statistic
#'
#' This function generate `samples` bootstrap replicates of a statistic applied
#' to a data cube per year.
#'
#' @param data_cube_df A dataframe containing data in biodiversity data cube
#' format.
#' @param fun A function which when applied to data returns the statistic(s) of
#' interest.
#' @param samples The number of bootstrap replicates. A single positive integer.
#' @param ref_group A string indicating the reference year to compare the
#' statistic. Default `NA`, no reference year is used.
#' @param seed The seed for random number generation to make results
#' reproducible.
#'
#' @returns The returned value is a list of objects of class `"boot"` per year.
#' See `boot::boot()`.

perform_bootstrap_ts <- function(
    data_cube_df,
    fun,
    samples = 1000,
    ref_group = NA,
    seed = 123) {
  require("dplyr")
  require("rlang")

  withr::local_seed(seed)

  if (is.na(ref_group)) {
    bootstrap_list <- data_cube_df %>%
      dplyr::summarize(num_occ = sum(.data$obs),
                       .by = c("year", "taxonKey")) %>%
      dplyr::arrange(.data$year) %>%
      tidyr::pivot_wider(names_from = "year",
                         values_from = "num_occ",
                         values_fill = 0) %>%
      tibble::column_to_rownames("taxonKey") %>%
      as.list() %>%
      purrr::map(~boot::boot(data = .,
                             statistic = boot_statistic,
                             R = samples,
                             fun = fun))
  } else {

  }

  return(bootstrap_list)
}
