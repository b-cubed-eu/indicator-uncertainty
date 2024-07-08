#' Classify effects by comparing the confidence intervals with a reference and
#' thresholds as factor variables
#'
#' This function classify effects by comparing the confidence intervals with a
#' reference and thresholds. A wrapper around `effectclass::classify()` that
#' adds effect descriptions as factor variables.
#'
#' @param df A dataframe containing summary data of confidence limits.
#' @param ... Additional argument to be passed to the `effectclass::classify()`
#' function.
#'
#' @returns The returned value is a list of objects of class `"boot"` per year.
#' See `boot::boot()`.

add_classification_as_factor <- function(
    df,
    lcl_column,
    ucl_column,
    threshold,
    reference = 0,
    coarse = TRUE) {
  require("dplyr")

  # Classify effects with effectclass
  classified_df <- df %>%
    mutate(effect_code = classification(
      !!sym(lcl_column),
      !!sym(ucl_column),
      threshold = threshold,
      reference = reference)
    )

  # Add coarse classification if specified
  if (coarse) {
    classified_df$effect_code_coarse <- coarse_classification(
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