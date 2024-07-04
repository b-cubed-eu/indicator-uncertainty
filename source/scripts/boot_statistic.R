# Function for bootstrapping
boot_statistic <- function(data, indices, fun) {
  d <- data[indices]
  return(fun(d))
}
