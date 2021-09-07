library(TestDesign)

cfg <- createStaticTestConfig(
  MIP = list(solver = "gurobi")
)

get_midpoints <- function(n_targets) {
  p <- seq(0, 1, length.out = n_targets * 2 + 1)
  p <- p[seq(2, length(p), 2)]
  return(p)
}

for (itempool_size in c(600, 900)) {
  for (n_strata in c(2, 3)) {

    itempool <- loadItemPool(
      sprintf("data/itempool_%s.csv", itempool_size)
    )

    target_a <- round(quantile(itempool@ipar[, 1], get_midpoints(n_strata)), 3)
    target_b <- round(quantile(itempool@ipar[, 2], get_midpoints(5)), 3)

    idx_stratum <- stratifyItemPool(cfg, itempool, target_a, target_b)

    itemattrib <- loadItemAttrib(
      sprintf("data/itemattrib_%s.csv", itempool_size),
      itempool
    )
    itemattrib@data$STRATUM <- idx_stratum
    itemattrib@data$INDEX <- NULL

    write.csv(
      itemattrib@data,
      sprintf("data/itemattrib_%s_1STR_%s.csv", itempool_size, n_strata),
      row.names = FALSE
    )

  }
}
