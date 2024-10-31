insect_data_new <- insect_data
insect_data_new$data <- insect_data$data %>%
  filter(year > 2010)

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
seed <- 123

boot_list <- lapply(seq_len(samples), function(i) {
  sample.int(nrow(data), replace = TRUE)
})

boot_out <- lapply(boot_list, function(indices) {
  data_new <- data[indices,]
  insect_data_copy <- insect_data_new
  insect_data_copy$data <- data_new

  b3gbi::pielou_evenness_ts(insect_data_copy)$data
})

boot_obj_2011 <- NULL
boot_obj_2011$data <- insect_data_new$data
boot_obj_2011$t0 <- b3gbi::pielou_evenness_ts(insect_data_new)$data %>%
  filter(year == 2011) %>%
  pull(diversity_val)
boot_obj_2011$t <- sapply(boot_out, function(df) {
    df %>%
      filter(year == 2011) %>%
      pull(diversity_val)
  }) %>%
  as.matrix()
boot_obj_2011$R <- samples
boot_obj_2011$seed <- seed
boot_obj_2011$sim <- "ordinary"
boot_obj_2011$stype <- "i"
boot_obj_2011$strata <- rep(1, nrow(data))
boot_obj_2011$weights <- 1 / rep(1, nrow(data))
class(boot_obj_2011) <- "boot"

boot::boot.ci(boot_obj_2011, type = "bca")

# does not work
boot.ci
bca.ci
empinf: n <- nrow(data$data)
usual.jack: n <- nrow(data$data)
stat