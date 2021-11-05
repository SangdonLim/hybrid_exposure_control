oo <- read.csv("analysis/results.csv")

# main effects

oo$method <- factor(
  oo$method,
  c(
    "no-exposure-control",
    "alpha-stratification-1str-2",
    "alpha-stratification-1str-3",
    "eligibility",
    "progressive-restricted",
    "hybrid-se-1str-2",
    "hybrid-se-1str-3",
    "multiple-objective"
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

DVs <- names(oo)[-(1:4)]
for (DV in DVs) {
  m <- formula(sprintf("%s ~ (method + target + itempool_size)^2", DV))
  fit <- lm(eval(m), oo)
  x <- coef(summary(fit))
  f <- sprintf("analysis/main_%s.csv", DV)
  x <- round(x, 10)
  write.csv(x, f)
}
