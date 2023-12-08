library(TestDesign)
library(doParallel)

# check package version
if (packageVersion("TestDesign") != "1.3.0.9001") {
  # this version is currently private; i am cleaning it up and it will be available soon
  .rs.restartR()
}

append_replication_index <- function(condition_list, replication_index_range) {

  n_conditions <- dim(condition_list)[1]
  condition_list <- cbind(
    condition_list,
    condition_index = 1:n_conditions
  )

  o <- data.frame(replication_index = replication_index_range)
  condition_list <- merge(condition_list, o, by = NULL)

  return(condition_list)

}

for (itempool_size in c(600, 900)) {
  assign(
    sprintf("itempool_%s", itempool_size),
    loadItemPool(sprintf("data/itempool_%s.csv", itempool_size))
  )
  assign(
    sprintf("itemattrib_%s", itempool_size),
    loadItemAttrib(
      sprintf("data/itemattrib_%s.csv", itempool_size),
      eval(parse(
        text = sprintf("itempool_%s", itempool_size)
      ))
    )
  )
  assign(
    sprintf("constraints_%s", itempool_size),
    loadConstraints(
      "data/constraints.csv",
      eval(parse(
        text = sprintf("itempool_%s", itempool_size)
      )),
      eval(parse(
        text = sprintf("itemattrib_%s", itempool_size)
      ))
    )
  )

  for (n_strata in c(2, 3)) {
    assign(
      sprintf("itemattrib_%s_1STR_%s", itempool_size, n_strata),
      loadItemAttrib(
        sprintf("data/itemattrib_%s_1STR_%s.csv", itempool_size, n_strata),
        eval(parse(
          text = sprintf("itempool_%s", itempool_size)
        ))
      )
    )
    assign(
      sprintf("constraints_%s_1STR_%s", itempool_size, n_strata),
      loadConstraints(
        "data/constraints.csv",
        eval(parse(
          text = sprintf("itempool_%s", itempool_size)
        )),
        eval(parse(
          text = sprintf("itemattrib_%s_1STR_%s", itempool_size, n_strata)
        ))
      )
    )
  }

}

conditions <- expand.grid(
  method = c(
    "hybrid-se-1str-2",
    "hybrid-se-1str-3",
    "alpha-stratification-1str-2",
    "alpha-stratification-1str-3",
    "multiple-objective",
    "progressive-restricted",
    "eligibility",
    "no-exposure-control"
  ),
  target_rate = c(0.15, 0.25),
  itempool_size = c(600, 900),
  stringsAsFactors = FALSE
)

replications <- 100
tasks   <- append_replication_index(conditions, 1:100)
# gurobi is deterministic
# but floating point computations are different between architectures

n_tasks <- dim(tasks)[1]

n_cores <- 9
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Run simulation
o <- foreach(
  idx_task = 1:n_tasks,
  .packages = c("TestDesign")
) %dopar% {

  task <- tasks[idx_task, ]
  fp <- sprintf(
    "results/%s_%s_%s_%s.RDS",
    task$method,
    task$target_rate,
    task$itempool_size,
    task$replication_index
  )

  if (file.exists(fp)) {
    return(NULL)
  }

  itempool <- NULL
  if (task$itempool_size == 600) {
    itempool    <- itempool_600
    constraints <- constraints_600
    if (task$method %in% c("hybrid-se-1str-2", "alpha-stratification-1str-2")) {
      constraints <- constraints_600_1STR_2
    }
    if (task$method %in% c("hybrid-se-1str-3", "alpha-stratification-1str-3")) {
      constraints <- constraints_600_1STR_3
    }
  }
  if (task$itempool_size == 900) {
    itempool    <- itempool_900
    constraints <- constraints_900
    if (task$method %in% c("hybrid-se-1str-2", "alpha-stratification-1str-2")) {
      constraints <- constraints_900_1STR_2
    }
    if (task$method %in% c("hybrid-se-1str-3", "alpha-stratification-1str-3")) {
      constraints <- constraints_900_1STR_3
    }
  }

  # Data generation
  set.seed(task$replication_index)
  true_theta <- rep(seq(-2, 2, 1), each = 2000)
  true_theta <- sample(true_theta)

  # Config generation
  cfg <- NULL
  if (task$method %in% c("hybrid-se-1str-2", "hybrid-se-1str-3")) {
    cfg <- createShadowTestConfig(
      MIP = list(solver = "gurobi"),
      exposure_control = list(
        method              = "HYBRID-SE",
        segment_cut         = c(-Inf, seq(-1.5, 1.5, 1), Inf),
        n_segment           = 5,
        max_exposure_rate   = rep(task$target_rate, 5),
        fading_factor       = 0.995,
        acceleration_factor = 2
      )
    )
  }
  if (task$method %in% c("alpha-stratification-1str-2", "alpha-stratification-1str-3")) {
    cfg <- createShadowTestConfig(
      MIP = list(solver = "gurobi"),
      exposure_control = list(
        method              = "ALPHA-STRATIFICATION",
        segment_cut         = c(-Inf, seq(-1.5, 1.5, 1), Inf),
        n_segment           = 5,
        max_exposure_rate   = rep(task$target_rate, 5),
        fading_factor       = 0.995,
        acceleration_factor = 2
      )
    )
  }
  if (task$method == "multiple-objective") {
    cfg <- createShadowTestConfig(
      MIP = list(solver = "gurobi"),
      exposure_control = list(
        method            = "MULTIPLE-OBJECTIVE",
        segment_cut       = c(-Inf, seq(-1.5, 1.5, 1), Inf),
        n_segment         = 5,
        max_exposure_rate = rep(task$target_rate, 5),
        fading_factor       = 0.995,
        acceleration_factor = 2
      )
    )
  }
  if (task$method == "progressive-restricted") {
    cfg <- createShadowTestConfig(
      MIP = list(solver = "gurobi"),
      exposure_control = list(
        method              = "PROGRESSIVE-RESTRICTED",
        segment_cut         = c(-Inf, seq(-1.5, 1.5, 1), Inf),
        n_segment           = 5,
        max_exposure_rate   = rep(task$target_rate, 5),
        fading_factor       = 0.995,
        acceleration_factor = 2
      )
    )
  }
  if (task$method == "eligibility") {
    cfg <- createShadowTestConfig(
      MIP = list(solver = "gurobi"),
      exposure_control = list(
        method              = "ELIGIBILITY",
        segment_cut         = c(-Inf, seq(-1.5, 1.5, 1), Inf),
        n_segment           = 5,
        max_exposure_rate   = rep(task$target_rate, 5),
        fading_factor       = 0.995,
        acceleration_factor = 2
      )
    )
  }
  if (task$method == "no-exposure-control") {
    cfg <- createShadowTestConfig(
      MIP = list(solver = "gurobi"),
      exposure_control = list(
        method              = "NONE",
        segment_cut         = c(-Inf, seq(-1.5, 1.5, 1), Inf),
        n_segment           = 5,
        max_exposure_rate   = rep(task$target_rate, 5),
        fading_factor       = 0.995,
        acceleration_factor = 2
      )
    )
  }

  # Main simulation
  set.seed(task$replication_index)
  solution <- NULL
  solution <- Shadow(cfg, constraints, true_theta = true_theta, seed = task$replication_index)
  if (Sys.info()["machine"] == "arm64") {
    solution@config@MIP$solver <- "gurobi-m1"
  }
  saveRDS(solution, file = fp)

}
