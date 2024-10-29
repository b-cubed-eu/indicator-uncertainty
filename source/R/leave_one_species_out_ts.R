#' Perform bootstrapping for a calculated statistic over time
#'
#' This function generate `samples` bootstrap replicates of a statistic applied
#' to a data cube per time point (e.g., year, month ...).
#'
#' @param data_cube_df A dataframe containing data in biodiversity data cube
#' format. See `b3gbi::process_cube()`.
#' @param fun A function which when applied to data returns the statistic(s) of
#' interest.
#' @param temporal_col_name The temporal column name of `data_cube_df`
#' (e.g., year, month ...) containing time point values. Default `year`.
#'
#' @returns ...

leave_one_species_out_ts <- function(
    data_cube_df,
    fun,
    temporal_col_name = "year") {
  require("dplyr")
  require("rlang")

  # Pivot data by temporal column
  pivot_dataset <- data_cube_df %>%
    dplyr::summarize(num_occ = sum(.data$obs),
                     .by = dplyr::all_of(c(temporal_col_name, "taxonKey"))
    ) %>%
    dplyr::arrange(.data[[temporal_col_name]]) %>%
    tidyr::pivot_wider(names_from = dplyr::all_of(temporal_col_name),
                       values_from = "num_occ",
                       values_fill = 0) %>%
    tibble::column_to_rownames("taxonKey")

  # Calculate indicator based on all data
  true_indicator_list <- pivot_dataset %>%
    as.list() %>%
    purrr::map(~fun(.))

  # Create dataframe
  true_indicator <- data.frame(diversity_val = unlist(true_indicator_list)) %>%
    tibble::rownames_to_column(var = temporal_col_name) %>%
    dplyr::mutate(!!temporal_col_name := as.numeric(.data[[temporal_col_name]]))

  # Create cross validation datasets
  cv_dataset <- modelr::crossv_loo(pivot_dataset, id = "id")

  # Get species left out
  species_df <- data.frame(
    id = seq_len(nrow(pivot_dataset)),
    species_left_out = rownames(pivot_dataset)
  )

  # Perform function on training data
  results <- lapply(cv_dataset$train, function(resample) {
    indices <- as.integer(resample)
    train_data <- resample$data[indices, ]

    value_list <- lapply(as.list(train_data), fun)
    indicator_result <- data.frame(loo_val = unlist(value_list)) %>%
      tibble::rownames_to_column(var = temporal_col_name) %>%
      dplyr::mutate(
        !!temporal_col_name := as.numeric(.data[[temporal_col_name]])
        )

    # Merge with the true indicator to enable comparison
    comparison_result <- indicator_result %>%
      left_join(true_indicator, by = temporal_col_name) %>%
      mutate(
        error = .data$diversity_val - .data$loo_val,
        sq_error = .data$error^2,
        abs_diff = abs(.data$error),
        rel_diff = .data$abs_diff / .data$loo_val,
        perc_diff = .data$rel_diff * 100
      )
  })

  # Summarise CV statistics in dataframe
  out_df <- do.call(bind_rows, c(results, .id = "id")) %>%
    dplyr::mutate(id = as.numeric(.data$id)) %>%
    dplyr::full_join(species_df, by = join_by("id")) %>%
    dplyr::select(-"id") %>%
    dplyr::select(c(all_of(temporal_col_name), "species_left_out",
                    dplyr::everything())) %>%
    dplyr::arrange(.data[[temporal_col_name]]) %>%
    dplyr::mutate(
      mse = mean(.data$sq_error),
      rmse = sqrt(.data$mse),
      .by = all_of(temporal_col_name)
    )

  return(out_df)
}
