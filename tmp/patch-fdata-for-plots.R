proj <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality"
path <- file.path(proj, "data/fdata.csv")

x <- read.csv(path, check.names = TRUE)

rename_if_exists <- function(df, old, new) {
  if (old %in% names(df) && !(new %in% names(df))) {
    names(df)[names(df) == old] <- new
  }
  df
}

x <- rename_if_exists(x, "All", "all")
x <- rename_if_exists(x, "Cardiovascular", "circ")
x <- rename_if_exists(x, "Respiratory", "resp")

x <- rename_if_exists(x, "All..Above.65.", "all.age65")
x <- rename_if_exists(x, "Cardiovascular..Above.65.", "circ.age65")
x <- rename_if_exists(x, "Respiratory..Above.65.", "resp.age65")

required <- c(
  "city", "date", "month",
  "all", "circ", "resp",
  "all.age65", "circ.age65", "resp.age65",
  "SPM", "SuHi", "SO2", "NO2", "Tave", "RHave"
)

missing <- setdiff(required, names(x))

cat("Missing after rename:\n")
print(missing)

if (length(missing) > 0) {
  stop("Still missing required columns. Check names(fdata.csv).")
}

write.csv(x, path, row.names = FALSE)
cat("Patched and saved:", path, "\n")
