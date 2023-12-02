# result data is very large in size (~ 88 GB total)
# because of this, only hashes of result files are stored in the repo

fp <- "results/raw"

# data (3200 files, ~ 88 GB)

new_snapshot <- fileSnapshot(fp, md5sum = TRUE, file.info = FALSE)
fn <- "results/hash.rds"
if (!file.exists(fn)) {
  saveRDS(new_snapshot, fn)
} else {
  stored_snapshot <- readRDS(fn)
}

changedFiles(before = stored_snapshot, after = new_snapshot, check.file.info = NULL)
