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

add_classification_as_factor <- function(df, ...) {
  df$effect <- classification(
    lcl = df$ll,
    ucl = df$ul,
    threshold = 0.1,
    reference = 0.6)

  df$effect_coarse <- coarse_classification(
    df$effect)

  # Create ordered factors of effects
  out_df <- df %>%
    mutate(
      effect = case_when(
        effect == "++" ~ "strong increase",
        effect == "+" ~ "increase",
        effect == "+~" ~ "moderate increase",
        effect == "~" ~ "stable",
        effect == "-" ~ "decrease",
        effect == "-~" ~ "moderate decrease",
        effect == "--" ~ "strong decrease",
        effect == "?+" ~ "potential increase",
        effect == "?-" ~ "potential decrease",
        effect == "?" ~ "unknown"
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
    ) %>%
    mutate(
      effect_coarse = case_when(
        effect_coarse == "+" ~ "increase",
        effect_coarse == "-" ~ "decrease",
        effect_coarse == "~" ~ "stable",
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