library(TestDesign)
library(mvnfast)

itempool_600    <- loadItemPool("data/itempool_600_origin.csv")
itemattrib_600  <- loadItemAttrib("data/itemattrib_600_origin.csv", itempool_600)

# logit-normal transformation
logit <- function(p) {
  return(log(p / (1 - p)))
}
inv_logit <- function(l) {
  return(exp(l) / (1 + exp(l)))
}

itempool_600@ipar[, 3] <- logit(itempool_600@ipar[, 3])

mu    <- apply(itempool_600@ipar, 2, mean)
mu[2] <- mu[2] - 0.7
sigma <- cov(itempool_600@ipar)

# generate main item pool
main_pool_size <- 900

set.seed(1)
invalid <- TRUE
while(invalid) {
  new_ipar <- rmvn(main_pool_size, mu, sigma)
  new_ipar[, 3] <- inv_logit(new_ipar[, 3])
  invalid <-
    any(new_ipar[, 1] < 0) |
    any(new_ipar[, 3] < 0)
}
new_ipar <- as.data.frame(new_ipar)
new_ipar <- round(new_ipar, 10)
item_id <- sprintf("ITEM%s", 1:main_pool_size)
new_ipar <- cbind(item_id, "3PL", new_ipar)
colnames(new_ipar) <- c("ID", "MODEL", "PAR1", "PAR2", "PAR3")

set.seed(1)
new_itemattrib <- itemattrib_600[sample(1:600, main_pool_size, replace = TRUE), ]
new_itemattrib$ID    <- item_id
new_itemattrib$INDEX <- NULL

for (itempool_size in c(600, 900)) {

  write.csv(
    new_ipar[1:itempool_size, ],
    sprintf("data/itempool_%s.csv", itempool_size),
    row.names = FALSE
  )

  write.csv(
    new_itemattrib[1:itempool_size, ],
    sprintf("data/itemattrib_%s.csv", itempool_size),
    row.names = FALSE
  )

}
