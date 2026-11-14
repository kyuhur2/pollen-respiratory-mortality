proj <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality"

files <- c(
  "metafor_noninteractive.csv",
  "metafor_bisection_perc75.csv",
  "metafor_bisection_perc80.csv",
  "metafor_bisection_perc85.csv",
  "metafor_bisection_abs25.csv",
  "metafor_bisection_abs50.csv",
  "metafor_bisection_abs75.csv",
  "metafor_bisection_no2_perc75.csv",
  "metafor_bisection_no2_perc80.csv",
  "metafor_bisection_no2_perc85.csv",
  "metafor_bisection_no2_abs25.csv",
  "metafor_bisection_no2_abs50.csv",
  "metafor_bisection_no2_abs75.csv",
  "metafor_bisection_so2_perc75.csv",
  "metafor_bisection_so2_perc80.csv",
  "metafor_bisection_so2_perc85.csv",
  "metafor_bisection_so2_abs25.csv",
  "metafor_bisection_so2_abs50.csv",
  "metafor_bisection_so2_abs75.csv",
  "noninteractive_aggregated.csv"
)

required_common <- c("exposure", "outcome", "lag", "rr", "cil", "ciu")

for (f in files) {
  path <- file.path(proj, "data", f)

  cat("\n==============================\n")
  cat(f, "\n")

  if (!file.exists(path)) {
    cat("MISSING FILE\n")
    next
  }

  x <- read.csv(path)

  cat("Rows:", nrow(x), " Cols:", ncol(x), "\n")

  if (grepl("^metafor", f)) {
    missing <- setdiff(required_common, names(x))
    cat("Missing required cols:", paste(missing, collapse = ", "), "\n")
  }

  if (nrow(x) == 0) {
    cat("EMPTY FILE\n")
  }

  if ("rr" %in% names(x)) {
    cat("rr NA count:", sum(is.na(x$rr)), "\n")
  }

  if ("cil" %in% names(x)) {
    cat("cil NA count:", sum(is.na(x$cil)), "\n")
  }

  if ("ciu" %in% names(x)) {
    cat("ciu NA count:", sum(is.na(x$ciu)), "\n")
  }

  if ("quantile" %in% names(x)) {
    cat("quantile values:", paste(sort(unique(x$quantile)), collapse = ", "), "\n")
  }

  if ("lag" %in% names(x)) {
    cat("lag values:", paste(sort(unique(x$lag)), collapse = ", "), "\n")
  }
}
