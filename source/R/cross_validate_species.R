#' Leave-One-Species-Out Time Series Cross-Validation
#'
#' This function performs leave-one-species-out (LOO) or k-fold cross-validation
#' on a biodiversity data cube to assess the performance of a specified
#' indicator function over time. It partitions the data by species, calculates
#' the specified indicator on training data, and compares it with the true
#' values to evaluate the influence of one or more species on the final result.
#'
#' @param data_cube A data cube object (class 'processed_cube').
#' See `b3gbi::process_cube()`.
#' @param fun A function which when applied to `data` returns the statistic(s)
#' of interest.
#' @param grouping_var ...
#' @param crossv_method Method of data partitioning.
#' If `crossv_method = "loo"` (default), `S = number of species` training
#' partitions are created containing `S - 1` rows each.
#' If `crossv_method = "kfold"`, the aggregated data is split the data into
#' `k` exclusive partitions containing `S / k` rows each.
#' @param k Number of folds (an integer). Used only if
#' `crossv_method = "kfold"`. Default 5.

#'
#' @returns A dataframe summarizing the cross-validation results, including:
#' - Cross-Validation id (`id_cv`)
#' - The grouping variable (e.g., year)
#' - The species left out during each cross-validation iteration
#' - The computed statistic values for both training (`rep_cv`) and true
#' datasets (`est_original`)
#' - Error metrics: error (= difference), squared error, absolute difference,
#' relative difference, and percent difference
#' - Error metrics summarised by `grouping_var`: mean relative difference
#' (`mre`), mean squared error (`mse`) and root mean squared error (`rmse`).

cross_validate_species <- function(
    data_cube,
    fun,
    grouping_var,
    crossv_method = c("loo", "kfold"),
    k = ifelse(crossv_method == "kfold", 5, NA),
    progress = FALSE) {
  require("dplyr")
  require("rlang")

  # Define cross-validation function
  cross_validate_f <- function(x, fun) {
    data_cube_copy <- data_cube
    data_cube_copy$data <- x

    fun(data_cube_copy)$data
  }

  # Calculate true statistic
  t0 <- fun(data_cube)$data

  # Check if crossv_method is loo or kfold
  crossv_method <- tryCatch({
    match.arg(crossv_method, c("loo", "kfold"))
  }, error = function(e) {
    stop("`crossv_method` must be one of 'loo', 'kfold'.",
         call. = FALSE)
  })

  if (crossv_method == "loo") {
    # Create cross validation datasets
    taxon_list <- unique(data_cube$data$taxonKey)
    cv_datasets <- lapply(taxon_list, function(taxon) {
      data_cube$data[data_cube$data$taxonKey != taxon, ]
    })

    # Get species left out
    species_df <- data.frame(
      id_cv = seq_along(taxon_list),
      species_left_out = taxon_list
    )
  } else {
    # Species partitioning
    taxon_list <- data_cube$data %>%
      distinct(.data$taxonKey) %>%
      modelr::crossv_kfold(id = "id_cv", k = k)

    # Get species left out
    species_left_out_list <- lapply(lapply(taxon_list$test, as.integer),
      function(indices) {
        df <- data_cube$data %>%
          distinct(.data$taxonKey)

        df[indices, ] %>%
          pull(.data$taxonKey)
      }
    )
    names(species_left_out_list) <- NULL

    species_df <- tibble(
      id_cv = as.numeric(taxon_list$id_cv),
      species_left_out = species_left_out_list
    )

    # Create cross validation datasets
    cv_datasets <- lapply(species_left_out_list, function(taxa) {
      data_cube$data[!data_cube$data$taxonKey %in% taxa, ]
    })
  }

  # Perform function on training data
  results <- cv_datasets %>%
    purrr::map(
      cross_validate_f,
      fun = fun,
      .progress = ifelse(progress, "Cross-Validation", progress))

  # Summarise CV statistics in dataframe
  out_df <- results %>%
    dplyr::bind_rows(.id = "id_cv") %>%
    dplyr::mutate(id_cv = as.numeric(.data$id_cv)) %>%
    dplyr::full_join(species_df, by = join_by("id_cv")) %>%
    dplyr::rename("rep_cv" = "diversity_val") %>%
    dplyr::left_join(t0, by = grouping_var) %>%
    dplyr::rename("est_original" = "diversity_val") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      error =  .data$rep_cv - .data$est_original,
      sq_error = .data$error^2,
      abs_diff = abs(.data$error),
      rel_diff = .data$abs_diff / (.data$est_original + 10^-8),
      perc_diff = .data$rel_diff * 100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      mre = mean(.data$rel_diff),
      mse = mean(.data$sq_error),
      rmse = sqrt(.data$mse),
      .by = all_of(grouping_var)) %>%
    dplyr::arrange(.data[[grouping_var]]) %>%
    dplyr::select("id_cv", all_of(grouping_var), "species_left_out", "rep_cv",
                  "est_original", dplyr::everything())

  return(out_df)
}
