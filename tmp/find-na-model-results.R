proj <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality"

files <- list.files(
  file.path(proj, "data"),
  pattern = "^metafor.*\\.csv$",
  full.names = TRUE
)

for (path in files) {
  x <- read.csv(path)

  needed <- c("exposure", "outcome", "lag", "quantile", "rr", "cil", "ciu")
  existing <- intersect(needed, names(x))

  if (!all(c("rr", "cil", "ciu") %in% names(x))) next

  bad <- x[is.na(x$rr) | is.na(x$cil) | is.na(x$ciu), existing, drop = FALSE]

  if (nrow(bad) > 0) {
    cat("\n==============================\n")
    cat(basename(path), "\n")
    cat("Bad rows:", nrow(bad), "of", nrow(x), "\n")
    print(bad)
  }
}
