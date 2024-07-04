bootstrap_list_to_df <- function(bootstrap_list) {
  bootstrap_data_df <- sapply(bootstrap_list, function(df) df$t) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "sample") %>%
    tidyr::pivot_longer(cols = -sample,
                        values_to = "est_boot",
                        names_to = "year") %>%
    dplyr::mutate(sample = as.numeric(sample))

  bootstrap_summaries <- data.frame(
    year = names(bootstrap_list),
    est_original = sapply(bootstrap_list, function(df) df$t0),
    se_boot = sapply(bootstrap_list, function(df) stats::sd(df$t))
  )

  bootstrap_data_full <- bootstrap_data_df %>%
    dplyr::full_join(bootstrap_summaries, by = dplyr::join_by("year")) %>%
    dplyr::arrange(sample, year) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(diff = .data$est_boot - .data$est_original,
                  year = as.numeric(year)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(bias_boot = mean(.data$diff)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"diff")

  return(bootstrap_data_full)
}
