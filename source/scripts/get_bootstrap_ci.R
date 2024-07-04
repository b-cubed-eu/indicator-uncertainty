get_bootstrap_ci <- function(bootstrap_list, ...) {
  require("dplyr")

  # Calculate nonparametric confidence intervals
  conf_ints <- lapply(bootstrap_list, boot::boot.ci, ...)

  # Get interval names
  indices_to_remove <- match(c("R", "t0", "call"), names(conf_ints[[1]]))
  interval_names <- names(conf_ints[[1]])[-indices_to_remove]

  # Get confidence level
  conf_level <- conf_ints[[1]][[interval_names[1]]][1]

  # Summarise for each confidence interval upper and lower limits in dataframes
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

    out_list[[i]] <- data.frame(year = as.numeric(names(conf_ints)),
                                conf_name = name,
                                ll = ll,
                                ul = ul)
  }

  # Create combined dataframe
  conf_df_out <- do.call(rbind.data.frame, out_list) %>%
    dplyr::mutate(conf_level = conf_level)
  rownames(conf_df_out) <- NULL

  return(conf_df_out)
}
