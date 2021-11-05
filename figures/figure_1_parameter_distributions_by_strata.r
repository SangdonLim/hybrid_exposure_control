library(TestDesign)

root <- rprojroot::find_rstudio_root_file()

itempool_900     <- loadItemPool("data/itempool_900.csv")
itemattrib_900_2 <- loadItemAttrib("data/itemattrib_900_1STR_2.csv", itempool_900)
itempool_900     <- loadItemPool("data/itempool_900.csv")
itemattrib_900_3 <- loadItemAttrib("data/itemattrib_900_1STR_3.csv", itempool_900)

setEPS()
postscript("figures/parameter_partitions_900.eps", width = 9, height = 8)
par(mfrow = c(2, 2))

# panel 1: a-parameter 3-strata ------------------------------------------------
plot(
  0, 0,
  xlim = c(0, 3), ylim = c(0, 3), type = "n",
  axes = FALSE,
  xlab = "a-parameter",
  ylab = "Stratum"
)
axis(1)
axis(2, at = 0:3, labels = c(NA, 1, 2, 3))
points(itempool_900@ipar[, 1], itemattrib_900_3@data$STRATUM, pch = "|", col = rgb(0, 0, 1))
boxplot(
  itempool_900@ipar[, 1] ~ itemattrib_900_3@data$STRATUM,
  add = TRUE,
  horizontal = TRUE,
  at = 1:3 - 0.5,
  boxwex = 0.25,
  names = NA,
  axes = FALSE
)
box(lwd = 1)

# panel 2: b-parameter 3-strata ------------------------------------------------
plot(
  0, 0,
  xlim = c(-4, 4), ylim = c(0, 3), type = "n",
  axes = FALSE,
  xlab = "b-parameter",
  ylab = "Stratum"
)
axis(1)
axis(2, at = 0:3, labels = c(NA, 1, 2, 3))
points(itempool_900@ipar[, 2], itemattrib_900_3@data$STRATUM, pch = "|", col = "blue")
boxplot(
  itempool_900@ipar[, 2] ~ itemattrib_900_3@data$STRATUM,
  add = TRUE,
  horizontal = TRUE,
  at = 1:3 - 0.5,
  boxwex = 0.25,
  names = NA,
  axes = FALSE
)
box(lwd = 1)

# panel 3: a-parameter 2-strata ------------------------------------------------
plot(
  0, 0,
  xlim = c(0, 3), ylim = c(0, 2), type = "n",
  axes = FALSE,
  xlab = "a-parameter",
  ylab = "Stratum"
)
axis(1)
axis(2, at = 0:2, labels = c(NA, 1, 2))
points(itempool_900@ipar[, 1], itemattrib_900_2@data$STRATUM, pch = "|", col = rgb(0, 0, 1))
boxplot(
  itempool_900@ipar[, 1] ~ itemattrib_900_2@data$STRATUM,
  add = TRUE,
  horizontal = TRUE,
  at = 1:2 - 0.5,
  boxwex = 0.25,
  names = NA,
  axes = FALSE
)
box(lwd = 1)

# panel 4: b-parameter 2-strata ------------------------------------------------
plot(
  0, 0,
  xlim = c(-4, 4), ylim = c(0, 2), type = "n",
  axes = FALSE,
  xlab = "b-parameter",
  ylab = "Stratum"
)
axis(1)
axis(2, at = 0:2, labels = c(NA, 1, 2))
points(itempool_900@ipar[, 2], itemattrib_900_2@data$STRATUM, pch = "|", col = "blue")
boxplot(
  itempool_900@ipar[, 2] ~ itemattrib_900_2@data$STRATUM,
  add = TRUE,
  horizontal = TRUE,
  at = 1:2 - 0.5,
  boxwex = 0.25,
  names = NA,
  axes = FALSE
)
box(lwd = 1)

dev.off()
