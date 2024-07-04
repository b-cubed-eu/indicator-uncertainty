#' Function to calculate Pielou's evenness

evenness_formula <- function(x) {
  big_s <- length(x)
  n <- x
  big_n <- sum(n)
  p <- n / big_n
  p_squared <- p^2
  summed <- sum(p_squared)
  rooted <- sqrt(summed)
  complement <- 1 - rooted
  even <- complement / (1 - sqrt(1 / big_s))
  if (is.nan(even)) {
    even <- NA
  }

  return(even)
}
