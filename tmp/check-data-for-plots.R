proj <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality"

check_file <- function(path, required_cols) {
  cat("\n==============================\n")
  cat("FILE:", path, "\n")
  cat("==============================\n")

  if (!file.exists(path)) {
    cat("MISSING FILE\n")
    return(invisible(NULL))
  }

  x <- read.csv(path, check.names = TRUE)

  cat("Rows:", nrow(x), "\n")
  cat("Cols:", ncol(x), "\n\n")

  missing <- setdiff(required_cols, names(x))
  present <- intersect(required_cols, names(x))

  cat("Present required columns:\n")
  print(present)

  cat("\nMissing required columns:\n")
  print(missing)

  cat("\nPossible matching columns:\n")
  for (col in missing) {
    pattern <- gsub("\\.", ".*", col)
    hits <- grep(pattern, names(x), ignore.case = TRUE, value = TRUE)
    cat(col, "=>", paste(hits, collapse = ", "), "\n")
  }

  invisible(x)
}

# What plots.R expects from fdata.csv
fdata_required <- c(
  "city", "date", "month",
  "all", "circ", "resp",
  "all.age65", "circ.age65", "resp.age65",
  "SPM", "SuHi", "SO2", "NO2", "Tave", "RHave"
)

fdata <- check_file(
  file.path(proj, "data/fdata.csv"),
  fdata_required
)

cat("\n\nActual fdata.csv names:\n")
print(names(fdata))

cat("\n\nClasses of likely mortality columns:\n")
possible_cols <- c(
  "all", "All",
  "circ", "Cardiovascular",
  "resp", "Respiratory",
  "all.age65", "All..Above.65.",
  "circ.age65", "Cardiovascular..Above.65.",
  "resp.age65", "Respiratory..Above.65."
)

possible_cols <- intersect(possible_cols, names(fdata))
print(sapply(fdata[possible_cols], class))

cat("\n\nFirst few likely mortality columns:\n")
print(head(fdata[possible_cols]))
