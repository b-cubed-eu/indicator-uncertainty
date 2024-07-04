#' Helper functions

# Logit transformation
logit <- function(p) {
  log(p / (1 - p))
}

# Inverse logit transformation
inv_logit <- function(l) {
  exp(l) / (1 + exp(l))
}
