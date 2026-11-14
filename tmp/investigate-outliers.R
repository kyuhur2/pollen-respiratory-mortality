
# create new SPMout with top 0.1% removed (each city) ---------------------

library(dplyr)

proj <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality"

old_path <- file.path(proj, "data/lagdata-old.csv")
new_path <- file.path(proj, "data/lagdata.csv")

# 1. Rename original lagdata.csv to lagdata-old.csv
if (file.exists(new_path) && !file.exists(old_path)) {
  file.rename(new_path, old_path)
} else if (file.exists(old_path)) {
  message("lagdata-old.csv already exists. Using it as the source file.")
} else {
  stop("Could not find lagdata.csv or lagdata-old.csv.")
}

# 2. Read the old/original file
lagdata <- read.csv(old_path)

# 3. Recreate SPMout and propagate lags
lagdata_new <- lagdata %>%
  mutate(date = as.Date(date)) %>%
  arrange(city, date) %>%
  group_by(city) %>%
  mutate(
    SPM_p999_city = quantile(SPM, probs = 0.999, na.rm = TRUE),
    
    # Remove top 0.1% of SPM within each city
    SPMout = if_else(
      !is.na(SPM) & SPM > SPM_p999_city,
      NA_real_,
      SPM
    ),
    
    # Recreate lagged cleaned SPM variables
    SPMout1 = lag(SPMout, 1),
    SPMout2 = lag(SPMout, 2),
    SPMout3 = lag(SPMout, 3),
    SPMout4 = lag(SPMout, 4),
    SPMout5 = lag(SPMout, 5),
    SPMout6 = lag(SPMout, 6),
    SPMout7 = lag(SPMout, 7)
  ) %>%
  ungroup() %>%
  select(-SPM_p999_city)

# 4. Check removal count
lagdata_new %>%
  summarise(
    n_spm_observed = sum(!is.na(SPM)),
    n_removed = sum(!is.na(SPM) & is.na(SPMout)),
    pct_removed = 100 * n_removed / n_spm_observed
  ) %>%
  print()

lagdata_new %>%
  group_by(city) %>%
  summarise(
    n_spm_observed = sum(!is.na(SPM)),
    n_removed = sum(!is.na(SPM) & is.na(SPMout)),
    pct_removed = 100 * n_removed / n_spm_observed,
    .groups = "drop"
  ) %>%
  arrange(desc(n_removed)) %>%
  print(n = Inf)

# 5. Write new lagdata.csv
write.csv(lagdata_new, new_path, row.names = FALSE)

message("Done. Original file is now lagdata-old.csv; updated file written to lagdata.csv.")

# recreate subdata with (new) lagdata -------------------------------------

library(dplyr)
library(readr)

proj <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality"

lag_path <- file.path(proj, "data/lagdata.csv")
sub_path <- file.path(proj, "data/subdata.csv")
sub_old_path <- file.path(proj, "data/subdata-old.csv")

# 1. Rename old subdata.csv to subdata-old.csv
if (file.exists(sub_path) && !file.exists(sub_old_path)) {
  file.rename(sub_path, sub_old_path)
} else if (file.exists(sub_old_path)) {
  message("subdata-old.csv already exists. Using it as the source structure.")
} else {
  stop("Could not find subdata.csv or subdata-old.csv.")
}

# 2. Read new lagdata and old subdata
lagdata <- read.csv(lag_path)
subdata_old <- read.csv(sub_old_path)

# 3. Basic investigation of old subdata
cat("\n--- Old subdata dimensions ---\n")
cat("Rows:", nrow(subdata_old), "\n")
cat("Columns:", ncol(subdata_old), "\n")

cat("\n--- Old subdata cities ---\n")
print(sort(unique(subdata_old$city)))

cat("\n--- Old subdata months ---\n")
print(sort(unique(subdata_old$month)))

cat("\n--- Old subdata date range ---\n")
print(range(as.Date(subdata_old$date), na.rm = TRUE))

cat("\n--- Columns in old subdata ---\n")
print(names(subdata_old))

# 4. Check that required key columns exist
required_keys <- c("city", "date")

if (!all(required_keys %in% names(lagdata))) {
  stop("lagdata.csv is missing city/date columns.")
}

if (!all(required_keys %in% names(subdata_old))) {
  stop("subdata-old.csv is missing city/date columns.")
}

# 5. Check that old subdata columns exist in new lagdata
missing_cols <- setdiff(names(subdata_old), names(lagdata))

if (length(missing_cols) > 0) {
  cat("\nThese columns exist in old subdata but not in new lagdata:\n")
  print(missing_cols)
  stop("Cannot recreate subdata because some columns are missing from lagdata.")
}

# 6. Recreate subdata using same city-date rows as old subdata
sub_keys <- subdata_old %>%
  select(city, date) %>%
  distinct()

subdata_new <- lagdata %>%
  semi_join(sub_keys, by = c("city", "date")) %>%
  select(all_of(names(subdata_old))) %>%
  arrange(city, as.Date(date))

# 7. Validation checks
cat("\n--- New subdata dimensions ---\n")
cat("Rows:", nrow(subdata_new), "\n")
cat("Columns:", ncol(subdata_new), "\n")

cat("\n--- Row count comparison ---\n")
cat("Old subdata rows:", nrow(subdata_old), "\n")
cat("New subdata rows:", nrow(subdata_new), "\n")

if (nrow(subdata_new) != nrow(subdata_old)) {
  warning("New subdata row count does not match old subdata row count.")
}

cat("\n--- SPMout removal count in new subdata ---\n")
subdata_new %>%
  summarise(
    n_spm_observed = sum(!is.na(SPM)),
    n_spmout_removed = sum(!is.na(SPM) & is.na(SPMout)),
    pct_removed = 100 * n_spmout_removed / n_spm_observed
  ) %>%
  print()

cat("\n--- SPMout removal count by city in new subdata ---\n")
subdata_new %>%
  group_by(city) %>%
  summarise(
    n_spm_observed = sum(!is.na(SPM)),
    n_spmout_removed = sum(!is.na(SPM) & is.na(SPMout)),
    pct_removed = 100 * n_spmout_removed / n_spm_observed,
    .groups = "drop"
  ) %>%
  arrange(desc(n_spmout_removed)) %>%
  print(n = Inf)

# 8. Write recreated subdata.csv
write.csv(subdata_new, sub_path, row.names = FALSE)

message("\nDone. Old subdata saved as subdata-old.csv; recreated subdata written to subdata.csv.")
