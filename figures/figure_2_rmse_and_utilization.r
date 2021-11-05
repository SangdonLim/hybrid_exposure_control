library(stringr)

d <- read.csv("tables/averages.csv")

method_idx <- 3:10
legend_labels <- c(
  "No exposure control (n)",
  "a-stratification 2-strata (a2)",
  "a-stratification 3-strata (a3)",
  "Eligibility (e)",
  "Progressive-restricted (p)",
  "Hybrid 2-strata (h2)",
  "Hybrid 3-strata (h3)",
  "Multiple Objective (m)"
)

legend_labels <- str_replace_all(legend_labels, "-", "\uad")

setEPS()
postscript("figures/rmse_and_utilization.eps", width = 9, height = 9)

par(
  mfrow = c(2, 2),
  mai = c(0, 0, 0, 0),
  oma = c(5, 5, 5, 5)
)

for (i in 1:4) {

  plot(
    0, 0,
    type = "n",
    axes = FALSE,
    xlim = c(0, 1),
    ylim = c(0.24, 0.51),
  )
  if (i %in% c(3, 4)) {
    axis(1)
  }
  if (i %in% c(1, 3)) {
    axis(2)
  }
  if (i %in% c(1, 2)) {
    axis(1, labels = NA)
    axis(3)

  }
  if (i %in% c(2, 4)) {
    axis(2, labels = NA)
    axis(4)
  }

  d$X[method_idx]
  method_labels <- c("n", "a2", "a3", "e", "p", "h2", "h3", "m")

  utilization <- d[, 9 + i][method_idx]
  rmse        <- d[, 1 + i][method_idx]

  points(
    utilization,
    rmse,
    pch = 21,
    col = method_idx,
    bg = method_idx
  )
  text(
    utilization +
    c(0, -0.02, 0, 0, 0, -0.02, 0, 0),
    rmse,
    method_labels,
    adj = c(0.5, -1)
  )

  text(
    1.0, 0.25,
    sprintf(
      "(%s) I = %s, r_max = %0.2f",
      letters[i],
      d[1, 1 + i],
      d[2, 1 + i]
    ),
    adj = 1
  )

  if (i == 1) {
    legend(
      "topleft",
      legend_labels,
      pch = 21,
      col = method_idx,
      pt.bg = method_idx,
      bty = "n"
    )
  }

  box(lwd = 1)

}

mtext(
  "Utilization (proportion of items with exposure rates > 0.01)",
  outer = TRUE, side = 1, line = 3
)
mtext(
  "RMSE",
  outer = TRUE, side = 2, line = 3
)

dev.off()
