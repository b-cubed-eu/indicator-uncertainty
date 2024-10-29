#' Leave-One-Species-Out Time Series Cross-Validation
#'
#' This function performs leave-one-species-out (LOO) or k-fold cross-validation
#' on a biodiversity data cube to assess the performance of a specified
#' indicator function over time. It partitions the data by species, calculates
#' the specified indicator on training data, and compares it with the true
#' values to evaluate the influence of one or more species on the final result.
#'
#' @param data_cube_df A dataframe containing data in biodiversity data cube
#' format. See `b3gbi::process_cube()`.
#' @param fun A function which when applied to data returns the statistic(s) of
#' interest.
#' @param crossv_method Method of data partitioning.
#' If `crossv_method = "loo"` (default), `S = number of species` training
#' partitions are created containing `S - 1` rows each.
#' If `crossv_method = "kfold"`, the aggregated data is split the data into
#' `k` exclusive partitions containing `S / k` rows each.
#' @param k Number of folds (an integer). Used only if
#' `crossv_method = "kfold"`. Default 5.
#' @param temporal_col_name The temporal column name of `data_cube_df`
#' (e.g., year, month ...) containing time point values. Default `year`.
#'
#' @returns A dataframe summarizing the cross-validation results, including:
#' - The temporal values (e.g., years)
#' - The species left out during each cross-validation iteration
#' - The computed statistic values for both training and true datasets
#' - Error metrics summarised by `temporal_col_name`: Mean Squared Error
#' (MSE) and Root Mean Squared Error (RMSE).

leave_one_species_out_ts <- function(
    data_cube_df,
    fun,
    crossv_method = c("loo", "kfold"),
    k = ifelse(crossv_method == "kfold", 5, NA),
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

  # Check if crossv_method is loo or kfold
  crossv_method <- tryCatch({
    match.arg(crossv_method, c("loo", "kfold"))
  }, error = function(e) {
    stop("`crossv_method` must be one of 'loo', 'kfold'.",
         call. = FALSE)
  })

  if (crossv_method == "loo") {
    # Create cross validation datasets
    cv_dataset <- modelr::crossv_loo(pivot_dataset, id = "id_cv")

    # Get species left out
    species_df <- data.frame(
      id_cv = seq_len(nrow(pivot_dataset)),
      species_left_out = rownames(pivot_dataset)
    )
  } else {
    # Create cross validation datasets
    cv_dataset <- modelr::crossv_kfold(pivot_dataset, id = "id_cv", k = k)

    # Get species left out
    species_left_out_list <- lapply(lapply(cv_dataset$test, as.integer),
      function(indices) {
        rownames(pivot_dataset)[indices]
      }
    )
    names(species_left_out_list) <- NULL

    species_df <- tibble(
      id_cv = as.numeric(cv_dataset$id_cv),
      species_left_out = species_left_out_list
    )
  }

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
  out_df <- do.call(bind_rows, c(results, .id = "id_cv")) %>%
    dplyr::mutate(id_cv = as.numeric(.data$id_cv)) %>%
    dplyr::as_tibble() %>%
    dplyr::full_join(species_df, by = join_by("id_cv")) %>%
    dplyr::select(c(all_of(temporal_col_name), "species_left_out", "id_cv",
                    dplyr::everything())) %>%
    dplyr::arrange(.data[[temporal_col_name]]) %>%
    dplyr::mutate(
      mse = mean(.data$sq_error),
      rmse = sqrt(.data$mse),
      .by = all_of(temporal_col_name)
    )

  return(out_df)
}
