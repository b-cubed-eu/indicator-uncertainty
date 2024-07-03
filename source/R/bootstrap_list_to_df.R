bootstrap_list_to_df <- function(bootstrap_list) {
  bootstrap_data_df <- sapply(bootstrap_list, function(df) df$t) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "rep") %>%
    tidyr::pivot_longer(cols = -rep,
                        values_to = "original",
                        names_to = "year") %>%
    dplyr::mutate(rep = as.numeric(rep))

  bootstrap_summaries <- data.frame(
    year = names(bootstrap_list),
    stat_value = sapply(bootstrap_list, function(df) df$t0),
    stat_se = sapply(bootstrap_list, function(df) stats::sd(df$t))
  )

  bootstrap_data_full <- bootstrap_data_df %>%
    dplyr::full_join(bootstrap_summaries, by = dplyr::join_by("year")) %>%
    dplyr::arrange(rep, year) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(diff = .data$original - .data$stat_value) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(bias = mean(.data$diff)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"diff")

  return(bootstrap_data_full)
}
