#' Classify effects by comparing the confidence intervals with a reference and
#' thresholds as factor variables
#'
#' This function adds classified effects to a dataframe by comparing the
#' confidence intervals with a reference and thresholds. A wrapper around
#' `effectclass::classify()` and `coarse_classification()` that adds effect
#' descriptions as factor variables to a dataframe.
#'
#' @param df A dataframe containing summary data of confidence limits.
#' @param cl_columns A vector of 2 column names in `df` indicating respectively
#' the lower and upper confidence limits.
#' @param threshold A vector of either 1 or 2 thresholds. A single threshold
#' will be transformed into `reference + c(-abs(threshold), abs(threshold))`.
#' See `effectclass::classify()`.
#' @param reference The null hypothesis. Defaults to 0.
#' See `effectclass::classify()`.
#' @param coarse Logical, defaults to `TRUE`. If `TRUE`, add a coarse
#' classification to the dataframe.
#' See `effectclass::coarse_classification()`.
#'
#' @returns The returned value is a list of objects of class `"boot"` per year.
#' See `boot::boot()`.

add_classification_as_factor <- function(
    df,
    cl_columns,
    threshold,
    reference = 0,
    coarse = TRUE) {
  require("dplyr")
  require("rlang")

  # Classify effects with effectclass
  classified_df <- df %>%
    mutate(effect_code = effectclass::classification(
      !!sym(cl_columns[1]),
      !!sym(cl_columns[2]),
      threshold = threshold,
      reference = reference)
    )

  # Add coarse classification if specified
  if (coarse) {
    classified_df$effect_code_coarse <- effectclass::coarse_classification(
      classified_df$effect_code)
  }

  # Create ordered factors of effects
  out_df <- df %>%
    mutate(
      effect = case_when(
        effect_code == "++" ~ "strong increase",
        effect_code == "+" ~ "increase",
        effect_code == "+~" ~ "moderate increase",
        effect_code == "~" ~ "stable",
        effect_code == "-" ~ "decrease",
        effect_code == "-~" ~ "moderate decrease",
        effect_code == "--" ~ "strong decrease",
        effect_code == "?+" ~ "potential increase",
        effect_code == "?-" ~ "potential decrease",
        effect_code == "?" ~ "unknown"
      ),
      effect = factor(effect,
                      levels = c(
                        "strong increase",
                        "increase",
                        "moderate increase",
                        "stable",
                        "moderate decrease",
                        "decrease",
                        "strong decrease",
                        "potential increase",
                        "potential decrease",
                        "unknown"),
                      ordered = TRUE)
    )

    if (coarse) {
      out_df <- out_df %>%
        mutate(
          effect_coarse = case_when(
            effect_code_coarse == "+" ~ "increase",
            effect_code_coarse == "-" ~ "decrease",
            effect_code_coarse == "~" ~ "stable",
            TRUE ~ "unknown",
          ),
          effect_coarse = factor(effect_coarse,
                                 levels = c("increase",
                                            "stable",
                                            "decrease",
                                            "unknown"),
                                 ordered = TRUE)
        )
    }


}