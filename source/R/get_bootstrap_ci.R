get_bootstrap_ci <- function(bootstrap_list, ...) {
  # Calculate nonparametric confidence intervals
  conf_ints <- lapply(bootstrap_list, boot::boot.ci, ...)

  # Get interval names
  indices_to_remove <- match(c("R", "t0", "call"), names(conf_ints[[1]]))
  interval_names <- names(conf_ints[[1]])[-indices_to_remove]

  # Get confidence level
  conf_level <- conf_ints[[1]][[interval_names[1]]][1]

  out_list <- vector(mode = "list", length = length(interval_names))
  for (i in seq_along(interval_names)) {
    name <- interval_names[i]

    ll <- sapply(conf_ints, function(list) {
      vec <- list[[name]]
      vec[length(vec) - 1]
    })
    ul <- sapply(conf_ints, function(list) {
      vec <- list[[name]]
      vec[length(vec)]
    })

    out_df <- data.frame(ll, ul)
    names(out_df) <- c(paste(name, "ll", sep = "_"),
                       paste(name, "ul", sep = "_"))
    out_list[[i]] <- out_df
  }



  return(out_df)
}
