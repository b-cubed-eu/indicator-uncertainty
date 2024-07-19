#' Convert list of `boot` objects to dataframe
#'
#' This function converts a list of objects of class `"boot"` per year into a
#' dataframe containing all required summaries.
#'
#' @param bootstrap_list A list of objects of class `"boot"` per year.
#'
#' @returns The returned value is a dataframe containing the bootstrap sample
#' index (`sample`), the year (`year`), the bootstrap estimate of the statistic
#' (`est_boot`), the original sample estimate of the statistic (`est_original`),
#' the standard deviation of the bootstrap replications (`se_boot`), and the
#' bootstrap bias (`bias_boot`).

bootstrap_list_to_df <- function(bootstrap_list) {
  require("dplyr")
  require("rlang")

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
    dplyr::arrange(.data$sample, .data$year) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(diff = .data$est_boot - .data$est_original,
                  year = as.numeric(.data$year)) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::mutate(bias_boot = mean(.data$diff)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"diff")

  return(bootstrap_data_full)
}
