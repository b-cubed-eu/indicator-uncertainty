# does not work
boot_statistic <- function(data, indices, fun) {
  subset_cube <- data
  subset_cube$data <- data$data[indices, ]
  return(fun(subset_cube)$data)
}

boot::boot(
  data = insect_data_new,
  statistic = boot_statistic,
  R = 1000,
  fun = b3gbi::pielou_evenness_ts)

# Works, but how to make a boot object for confidence intervals?

boot_list <- modelr::bootstrap(insect_data_new$data, 1000, id = ".id")$strap

boot_out <- lapply(boot_list, function(resample) {
  indices <- as.integer(resample)
  data <- resample$data[indices,]
  insect_data_copy <- insect_data_new
  insect_data_copy$data <- data

  b3gbi::pielou_evenness_ts(insect_data_copy)$data
})

# without modelr
data <- insect_data_new$data
samples <- 1000

boot_list <- lapply(seq_len(samples), function(i) {
  sample.int(nrow(data), replace = TRUE)
})

boot_out <- lapply(boot_list, function(indices) {
  data_new <- data[indices,]
  insect_data_copy <- insect_data_new
  insect_data_copy$data <- data_new

  b3gbi::pielou_evenness_ts(insect_data_copy)$data
})



