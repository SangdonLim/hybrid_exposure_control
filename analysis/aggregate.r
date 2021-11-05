library(tools)
library(progress)
library(TestDesign)

fp <- "results"

fs <- list.files(file.path(fp))
o <- as.data.frame(matrix(NA, length(fs), 4))
colnames(o) <- c("method", "target", "itempool_size", "replication")

oo <- o
oo$rmse                 <- NA
oo$overexposed          <- NA
oo$utilization_01       <- NA
oo$utilization_03       <- NA
oo$utilization_05       <- NA
oo$uniformity           <- NA
oo$max_spike            <- NA
oo$max_spike_1          <- NA
oo$max_spike_2          <- NA
oo$max_spike_3          <- NA
oo$max_spike_4          <- NA
oo$max_spike_5          <- NA

pb <- progress_bar$new(format = "[:bar] :current / :total | :eta", total = length(fs))

for (i in 1:length(fs)) {

  f <- fs[i]

  IVs <- strsplit(file_path_sans_ext(f), "_")[[1]]
  oo[i, 1:4] <- IVs

  solution <- NULL
  solution <- readRDS(file.path(fp, f))

  solve_time <- lapply(solution@output, function(x) x@solve_time)
  solve_time <- do.call(c, solve_time)
  if (range(solve_time)[2] > 10) {
    print(range(solve_time))
    # file.remove(file.path(fp, f))
  }

  oo$rmse[i] <- sqrt(mean((solution@final_theta_est - solution@true_theta)**2))

  if (is.na(oo$rmse[i])) {
    stop()
  }

  target_rate   <- as.numeric(IVs[2])

  er <- solution@exposure_rate[, "Item ER"]

  oo$overexposed[i] <- mean(er > target_rate)
  oo$utilization_01[i] <- mean(er > 0.01)
  oo$utilization_03[i] <- mean(er > 0.03)
  oo$utilization_05[i] <- mean(er > 0.05)

  # compute worst-case sd (worst non-uniform case)
  n_items_in_test <- sum(er)
  n_items_in_pool <- length(er)
  er_worstcase <- c(
    rep(0, n_items_in_pool - n_items_in_test),
    rep(1, n_items_in_test)
  )
  sd_in_worstcase <- sd(er_worstcase)

  oo$uniformity[i] <- 1 - (sd(er) / sd_in_worstcase)

  oo$max_spike[i] <- max(er)

  segment_cut <- solution@config@exposure_control$segment_cut
  segment_of_theta <- TestDesign:::find_segment(solution@final_theta_est, segment_cut)
  n_segment <- length(segment_cut) - 1
  item_exposure_rate_per_segment <- vector("list", n_segment)
  for (segment in 1:n_segment) {
    item_exposure_rate_this_segment <-
      apply(solution@usage_matrix[segment_of_theta == segment, ], 2, mean)
    max_spike_this_segment <- max(item_exposure_rate_this_segment)
    oo[[sprintf("max_spike_%s", segment)]][i] <- max_spike_this_segment
  }

  pb$tick(1)

}

oo$method <- factor(
  oo$method,
  c(
    "no-exposure-control",
    "eligibility",
    "multiple-objective",
    "progressive-restricted",
    "alpha-stratification-1str-2",
    "alpha-stratification-1str-3",
    "hybrid-se-1str-2",
    "hybrid-se-1str-3"
  )
)

oo$target <- factor(
  oo$target,
  c("0.25", "0.15")
)
oo$itempool_size <- factor(
  oo$itempool_size,
  c("900", "600")
)

oo$replication <- as.numeric(oo$replication)

idx <- order(oo$method, oo$target, oo$itempool_size, oo$replication)
oo <- oo[idx, ]

write.csv(oo, "analysis/results.csv", row.names = FALSE)
