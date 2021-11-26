oo <- read.csv("analysis/results.csv")

d <- aggregate(oo, by = list(oo$method, oo$itempool_size, oo$target), mean)
names(d)[1] <- "method"
names(d)[2] <- "itempool_size"
names(d)[3] <- "target"

d <- d[, -c(4:7)]
idx <- order(d$itempool_size, d$target, decreasing = TRUE)
d <- d[idx, ]

table_data <- list()
methods <- c(
  "no-exposure-control",
  "alpha-stratification-1str-2",
  "alpha-stratification-1str-3",
  "eligibility",
  "progressive-restricted",
  "hybrid-se-1str-2",
  "hybrid-se-1str-3",
  "multiple-objective"
)

for (idx_method in seq_along(methods)) {
  dd <- subset(d, method == methods[idx_method])
  table_data[[methods[idx_method]]] <- c(
    dd$rmse, dd$uniformity, dd$overexposed, dd$utilization_01
  )
}
table_data <- do.call(rbind, table_data)

table_data <- rbind(
  itempool_size = rep(dd$itempool_size, 4),
  target        = rep(dd$target, 4),
  table_data
)
colnames(table_data) <- c(
  rep("rmse", 4),
  rep("uniformity", 4),
  rep("overexposed", 4),
  rep("utilization", 4)
)

table_data <- rbind(
  table_data[, 1:8],
  table_data[, 9:16]
)

write.csv(table_data, "tables/table_3.csv")
