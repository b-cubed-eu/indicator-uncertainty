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

  # Calculate indicator based on all data
  true_indicator <- fun(data_cube_df)$data

  # Get species list
  species_list <- unique(data_cube_df$data$taxonKey)

  results <- lapply(species_list, function(species_left_out) {
    df_subset  <- data_cube_df$data %>%
      # Filter out the data for the species to leave out
      filter(.data$taxonKey != species_left_out)

    # Calculate the indicator for the remaining data
    data_cube_df_subset <- data_cube_df
    data_cube_df_subset$data <- df_subset
    indicator_result <- fun(data_cube_df_subset)$data

    # Merge with the true indicator to enable comparison
    comparison_result <- indicator_result %>%
      rename("loo_val" = "diversity_val") %>%
      left_join(true_indicator, by = temporal_col_name) %>%
      mutate(
        error = .data$diversity_val - .data$loo_val,
        sq_error = .data$error^2,
        abs_diff = abs(.data$error),
        rel_diff = .data$abs_diff / .data$loo_val,
        perc_diff = .data$rel_diff * 100
      )

    # Return the result with the species left out
    cbind(species_left_out, comparison_result)
  })

  out_df <- do.call(rbind.data.frame, results) %>%
    arrange(year, species_left_out) %>%
    mutate(
      mse = mean(.data$sq_error),
      rmse = sqrt(.data$mse),
      .by = all_of(temporal_col_name)
    )

  return(out_df)
}
