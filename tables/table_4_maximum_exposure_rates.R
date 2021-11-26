oo <- read.csv("analysis/results.csv")

d <- aggregate(oo, by = list(oo$method, oo$itempool_size, oo$target), mean)
names(d)[1] <- "method"
names(d)[2] <- "itempool_size"
names(d)[3] <- "target"

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

d <- d[order(match(d$method, methods)), ]
d <- d[, -c(4:10)]
idx <- order(d$itempool_size, d$target, decreasing = TRUE)
d <- d[idx, ]

rownames(d) <- NULL

write.csv(d, "tables/max_spike.csv", row.names = FALSE)
