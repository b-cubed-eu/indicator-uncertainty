add_bootstrap_to_dataframe <- function(
    data_cube_df,
    fun,
    replicates = 1000,
    seed = 123) {
  set.seed(seed)  # For reproducibility

  bootstrap_list <- data_cube_df %>%
    dplyr::summarize(num_occ = sum(.data$obs),
                     .by = c(.data$year, .data$taxonKey)) %>%
    dplyr::arrange(year) %>%
    tidyr::pivot_wider(names_from = .data$year,
                       values_from = .data$num_occ) %>%
    replace(is.na(.data), 0) %>%
    tibble::column_to_rownames("taxonKey") %>%
    as.list() %>%
    purrr::map(~boot::boot(data = .data,
                           statistic = boot_statistic,
                           R = replicates,
                           FUN = fun))

  return(bootstrap_list)
}
