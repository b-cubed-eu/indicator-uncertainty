#' Helper functions

# Logit transformation
logit <- function(p) {
  log(p / (1 - p))
}

# Inverse logit transformation
inv_logit <- function(l) {
  exp(l) / (1 + exp(l))
}

# Perform regression on a grouped dataframe and export summary statistics
grouped_regression <- function(df, formula) {
  require("dplyr")
  require("rlang")

  summary(stats::lm(formula, data = df))$coefficients %>%
    data.frame() %>%
    tibble::rownames_to_column("param") %>%
    dplyr::mutate("P < 0.05" = .data$Pr...t.. < 0.05)
}
