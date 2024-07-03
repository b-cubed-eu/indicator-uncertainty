perform_bootstrap <- function(
    data_cube_df,
    fun,
    replicates = 1000,
    seed = 123) {
  set.seed(seed)  # For reproducibility

  bootstrap_list <- data_cube_df %>%
    dplyr::summarize(num_occ = sum(.data$obs),
                     .by = c("year", "taxonKey")) %>%
    dplyr::arrange(year) %>%
    tidyr::pivot_wider(names_from = "year",
                       values_from = "num_occ",
                       values_fill = 0) %>%
    tibble::column_to_rownames("taxonKey") %>%
    as.list() %>%
    purrr::map(~boot::boot(data = .,
                           statistic = boot_statistic,
                           R = replicates,
                           fun = fun))

  return(bootstrap_list)
}
