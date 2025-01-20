#' Calculate confidence intervals for list of `boot` objects
#'
#' This function calculates confidence intervals for a list of objects of class
#' `"boot"` per year into a dataframe containing all required summaries.
#'
#' @param bootstrap_samples_df A dataframe containing the bootstrap samples.
#' @param grouping_var ...
#' @param type A vector of character strings representing the type of intervals
#' required. The value should be any subset of the values
#' `c("perc", "bca", "norm", "basic")` or simply `"all"` which will compute all
#' types of intervals (default).
#' @param conf A scalar or vector containing the confidence level(s) of the
#' required interval(s). Default 0.95.
#' @param aggregate ...
#' @param data_cube ...
#' @param fun ...
#' @param jackknife `"usual"`, `"pos"` ...
#' @param progress Logical. Whether to show a progress bar for jackknife
#' estimation `TRUE` or not `FALSE` (default).
#'
#' @returns The returned value is a dataframe containing the time point,
#' the type of interval (`int_type`), the lower limit of the confidence
#' interval (`ll`), the upper limit of the confidence interval (`ul`), and the
#' confidence level of the intervals (`conf_level`).

get_bootstrap_ci <- function(
    bootstrap_samples_df,
    grouping_var,
    type = c("perc", "bca", "norm", "basic"),
    conf = 0.95,
    aggregate = TRUE,
    data_cube = NULL,
    fun = NULL,
    jackknife = ifelse(is.element("bca", type), "usual", NULL),
    progress = FALSE) {
  require("dplyr")
  require("rlang")

  # Check if type is correct
  stopifnot("`type` must be one of 'perc', 'bca', 'norm', 'basic'." =
              all(is.element(type, c("perc", "bca", "norm", "basic", "all"))))

  # Calculate intervals
  out_list <- vector(mode = "list", length = length(type))
  for (i in seq_along(type)) {
    t <- type[i]

    if (any(t == "all" | t == "perc")) {
      # Calculate confidence limits per group
      intervals_list <- bootstrap_samples_df %>%
        split(bootstrap_samples_df[[grouping_var]]) %>%
        lapply(function(df) {
          replicates <- df$rep_boot
          boot:::perc.ci(t = replicates, conf = conf)
        })

      # Combine confidence levels in dataframe
      intervals_df <- do.call(rbind.data.frame, intervals_list) %>%
        mutate(group = unique(bootstrap_samples_df[[grouping_var]])) %>%
        rename("ll" = "V4", "ul" = "V5") %>%
        select("group", "ll", "ul", "conf")

      # Join with input data
      conf_df <- bootstrap_samples_df %>%
        mutate(int_type = t) %>%
        left_join(intervals_df, by = join_by(!!grouping_var == "group"))
    }
    if (any(t == "all" | t == "bca")) {
      # Check if jackknife is usual or pos
      jackknife <- tryCatch({
        match.arg(jackknife, c("usual", "pos"))
      }, error = function(e) {
        stop("`jackknife` must be one of 'usual', 'pos'.",
             call. = FALSE)
      })

      # Finite jackknife
      if (inherits(data_cube, "processed_cube")) {
        jackknife_estimates <- purrr::map(
          seq_len(nrow(data_cube$data)),
          function(i) {
            # Identify group
            group <- data_cube$data[[i, grouping_var]]

            # Remove i'th observation
            data <- data_cube$data[-i, ]
            data_cube_copy <- data_cube
            data_cube_copy$data <- data

            # Calculate indicator value without i'th observation
            fun(data_cube_copy)$data %>%
              filter(!!sym(grouping_var) == group) %>%
              pull(.data$diversity_val)
          },
          .progress = ifelse(progress, "Jackknife estimation", progress)) %>%
          unlist()

        jackknife_df <- data_cube$data %>%
          mutate(jack_rep = jackknife_estimates) %>%
          select(all_of(c(grouping_var, "jack_rep")))
      } else {
        jackknife_estimates <- purrr::map(
          seq_len(nrow(data_cube)),
          function(i) {
            # Identify group
            group <- data_cube[[i, grouping_var]]

            # Calculate indicator value without i'th observation
            fun(data_cube[-i, ]) %>%
              filter(!!sym(grouping_var) == group) %>%
              pull(.data$diversity_val)
          },
          .progress = ifelse(progress, "Jackknife estimation", progress)) %>%
          unlist()

        jackknife_df <- data_cube %>%
          mutate(jack_rep = jackknife_estimates) %>%
          select(all_of(c(grouping_var, "jack_rep")))
      }

      acceleration_df <- jackknife_df %>%
        left_join(bootstrap_samples_df %>%
                    distinct(!!sym(grouping_var), .data$est_original),
                  by = join_by(!!grouping_var)) %>%
        mutate(n = n(),
               .by = grouping_var) %>%
        rowwise() %>%
        mutate(intensity = ifelse(
          jackknife == "usual",
          (n - 1) * (.data$est_original - .data$jack_rep),
          (n + 1) * (.data$jack_rep - .data$est_original)
          )
        ) %>%
        ungroup() %>%
        summarise(
          numerator = sum(.data$intensity^3),
          denominator = 6 * sum(.data$intensity^2)^1.5,
          acceleration = .data$numerator / .data$denominator,
          .by = grouping_var
        )

      # Calculate confidence limits per group
      intervals_list <- bootstrap_samples_df %>%
        left_join(acceleration_df, by = join_by(!!grouping_var)) %>%
        split(bootstrap_samples_df[[grouping_var]]) %>%
        lapply(function(df) {
          # Get the original statistic and bootstrap replicates
          t0 <- unique(df$est_original)
          t <- df$rep_boot

          # Get the acceleration
          a <- unique(df$acceleration)
          if (is.finite(a)) {
            warning("Estimated adjustment 'a' is NA.")
            return(cbind(conf, NA, NA))
          }

          # Calculate the BCa critical values
          alpha <- (1 + c(-conf, conf)) / 2
          zalpha <- qnorm(alpha)

          z0 <- qnorm(sum(t < t0) / length(t))
          if (is.finite(z0)) {
            warning("Estimated adjustment 'z0' is infinite.")
            return(cbind(conf, NA, NA))
          }

          # Adjust for acceleration
          adj_alpha <- pnorm(z0 + (z0 + zalpha) / (1 - a * (z0 + zalpha)))
          qq <- boot:::norm.inter(t, adj_alpha)

          return(cbind(conf, matrix(qq[, 2L], ncol = 2L)))
        })

      # Combine confidence levels in dataframe
      intervals_df <- do.call(rbind.data.frame, intervals_list) %>%
        mutate(group = unique(bootstrap_samples_df[[grouping_var]])) %>%
        rename("ll" = "V2", "ul" = "V3") %>%
        select("group", "ll", "ul", "conf")

      # Join with input data
      conf_df <- bootstrap_samples_df %>%
        mutate(int_type = t) %>%
        left_join(intervals_df, by = join_by(!!grouping_var == "group"))
    }
    if (any(t == "all" | t == "norm")) {
      # Calculate confidence limits per group
      intervals_list <- bootstrap_samples_df %>%
        split(bootstrap_samples_df[[grouping_var]]) %>%
        lapply(function(df) {
          estimate <- unique(df$est_original)
          replicates <- df$rep_boot
          boot:::norm.ci(t0 = estimate, t = replicates, conf = conf)
        })

      # Combine confidence levels in dataframe
      intervals_df <- do.call(rbind.data.frame, intervals_list) %>%
        mutate(group = unique(bootstrap_samples_df[[grouping_var]])) %>%
        rename("ll" = "V2", "ul" = "V3") %>%
        select("group", "ll", "ul", "conf")

      # Join with input data
      conf_df <- bootstrap_samples_df %>%
        mutate(int_type = t) %>%
        left_join(intervals_df, by = join_by(!!grouping_var == "group"))
    }
    if (any(t == "all" | t == "basic")) {
      # Calculate confidence limits per group
      intervals_list <- bootstrap_samples_df %>%
        split(bootstrap_samples_df[[grouping_var]]) %>%
        lapply(function(df) {
          estimate <- unique(df$est_original)
          replicates <- df$rep_boot
          boot:::basic.ci(t0 = estimate, t = replicates, conf = conf)
        })

      # Combine confidence levels in dataframe
      intervals_df <- do.call(rbind.data.frame, intervals_list) %>%
        mutate(group = unique(bootstrap_samples_df[[grouping_var]])) %>%
        rename("ll" = "V4", "ul" = "V5") %>%
        select("group", "ll", "ul", "conf")

      # Join with input data
      conf_df <- bootstrap_samples_df %>%
        mutate(int_type = t) %>%
        left_join(intervals_df, by = join_by(!!grouping_var == "group"))
    }

    out_list[[i]] <- conf_df
  }

  # Combine dataframes from all interval types
  conf_df_full <- bind_rows(out_list)

  # Aggregate if requested
  if (aggregate) {
    conf_df_out <- conf_df_full %>%
      select(-c("sample", "rep_boot")) %>%
      distinct()
  } else {
    conf_df_out <- conf_df_full
  }

  return(conf_df_out)
}
