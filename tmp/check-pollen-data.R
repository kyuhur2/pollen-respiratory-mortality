
rm(list = ls())

library(dplyr)
library(tibble)
library(rlang)
library(tidyr)
library(lubridate)

# import functions and set var --------------------------------------------

root_dir <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality"
source(file.path(root_dir, "src/functions.R"))

# row builders (one row per statistic, columns = cities)
make_row_mean_sd <- function(data, var, exposure_label, stat_label = "Mean ± SD") {
  vals <- data %>%
    group_by(city) %>%
    summarise(m = round(mean({{var}}, na.rm = TRUE), 1),
              s = round(sd({{var}},   na.rm = TRUE), 1),
              .groups = "drop") %>%
    arrange(city) %>%
    transmute(val = paste0(m, " ± ", s)) %>%
    pull(val)
  tibble::tibble_row(Exposure = exposure_label, Statistics = stat_label, !!!setNames(as.list(vals), CITIES))
}

make_row_median_iqr <- function(data, var, exposure_label, stat_label = "Median, IQR") {
  vals <- data %>%
    group_by(city) %>%
    summarise(med = round(median({{var}}, na.rm = TRUE), 1),
              iqr = round(IQR({{var}},    na.rm = TRUE), 1),
              .groups = "drop") %>%
    arrange(city) %>%
    transmute(val = paste0(med, ", ", iqr)) %>%
    pull(val)
  tibble::tibble_row(Exposure = exposure_label, Statistics = stat_label, !!!setNames(as.list(vals), CITIES))
}

make_row_max <- function(data, var, exposure_label, stat_label = "Max") {
  vals <- data %>%
    group_by(city) %>%
    summarise(mx = round(max({{var}}, na.rm = TRUE), 1), .groups = "drop") %>%
    arrange(city) %>%
    transmute(val = as.character(mx)) %>%
    pull(val)
  tibble::tibble_row(Exposure = exposure_label, Statistics = stat_label, !!!setNames(as.list(vals), CITIES))
}

make_row_percentile <- function(data, var, p, exposure_label, stat_label_prefix) {
  # p in [0,1], e.g., 0.75 for 75th percentile
  label <- paste0(stat_label_prefix, "th percentile")
  vals <- data %>%
    group_by(city) %>%
    summarise(q = round(as.numeric(quantile({{var}}, probs = p, na.rm = TRUE, type = 7)), 1),
              .groups = "drop") %>%
    arrange(city) %>%
    transmute(val = as.character(q)) %>%
    pull(val)
  tibble::tibble_row(Exposure = exposure_label, Statistics = label, !!!setNames(as.list(vals), CITIES))
}

CITIES <- c(
  "Fukuoka", "Kitakyushu", "Kumamoto", "Kagoshima",
  "Nagasaki", "Oita", "Miyazaki", "Saga"
)

lagdata <- read.csv(file.path(root_dir, "data/lagdata-copy.csv"))
fdata   <- read.csv(file.path(root_dir, "data/fdata-copy.csv"))

# make the check table ----------------------------------------------------

make_check_table <- function(data, variables) {
  data <- data %>%
    filter(city %in% CITIES, month %in% c(2, 3, 4)) %>%
    mutate(city = factor(city, levels = CITIES))
  
  bind_rows(lapply(names(variables), function(label) {
    variable <- variables[[label]]
    if (!variable %in% names(data)) { return(NULL) }
    
    var <- sym(variable)
    bind_rows(
      make_row_mean_sd(data, !!var, label, "Mean ± SD"),
      make_row_median_iqr(data, !!var, label, "Median, IQR"),
      make_row_percentile(data, !!var, 0.75, label, "75"),
      make_row_percentile(data, !!var, 0.80, label, "80"),
      make_row_percentile(data, !!var, 0.85, label, "85"),
      make_row_max(data, !!var, label, "Maximum")
    )
  }))
}

lagdata_check <- make_check_table(
  lagdata,
  c(
    "SPM" = "SPM",
    "SPMout" = "SPMout",
    "SuHi" = "SuHi",
    "SuHiout" = "SuHiout"
  )
)

fdata_check <- make_check_table(
  fdata,
  c(
    "SPM" = "SPM",
    "SuHi" = "SuHi"
  )
)

print(lagdata_check)
print(fdata_check)

write.csv(
  lagdata_check,
  file.path(root_dir, "tables/check_lagdata.csv"),
  row.names = FALSE
)

write.csv(
  fdata_check,
  file.path(root_dir, "tables/check_fdata.csv"),
  row.names = FALSE
)

# check top 0.1% of SPM for lagdata
check_top_01 <- function(data) {
  data %>%
    group_by(city) %>%
    mutate(
      cutoff = quantile(SPM, 0.999, na.rm = TRUE),
      recreated = ifelse(is.na(SPM) | SPM > cutoff, NA, SPM)
    ) %>%
    ungroup() %>%
    summarise(
      actual_removed = sum(!is.na(SPM) & is.na(SPMout)),
      recreated_removed = sum(!is.na(SPM) & is.na(recreated)),
      mismatches = sum(
        xor(is.na(SPMout), is.na(recreated)) |
          (!is.na(SPMout) & !is.na(recreated) & SPMout != recreated)
      )
    )
}

check_top_01(lagdata)
check_top_01(fdata)

# Recreate SPMout by removing the top 0.1% of SPM within each city
recreate_spmout <- function(data) {
  data %>%
    group_by(city) %>%
    mutate(
      cutoff = quantile(SPM, 0.999, na.rm = TRUE, type = 7),
      SPMout = ifelse(is.na(SPM) | SPM > cutoff, NA_real_, SPM)
    ) %>%
    ungroup() %>%
    select(-cutoff)
}

lagdata <- recreate_spmout(lagdata)
fdata   <- recreate_spmout(fdata)

# uhiout_cols <- grep("^SuHiout", names(lagdata), value = TRUE)
# for (old_col in suhiout_cols) {
#   new_col <- sub("^SuHiout", "SuHi", old_col)
#   lagdata[[new_col]] <- lagdata[[old_col]]
# }

# lagdata <- lagdata %>%
#   select(
#     -matches("^SuHiout"),
#     -matches("^SuHi05"),
#     -matches("ma0\\.[67]$")
#   )

# fdata <- fdata %>%
#   select(
#     -matches("^SuHiout"),
#     -matches("^SuHi05"),
#     -matches("ma0\\.[67]$")
#   )

# regenerate the ma columns for lagdata -----------------------------------

recreate_ma <- function(data, prefix, max_lag = 5) {
  for (k in 1:max_lag) {
    cols <- c(prefix, paste0(prefix, 1:k))
    ma_col <- paste0(prefix, "ma0.", k)
    
    data[[ma_col]] <- rowMeans(
      data[cols],
      na.rm = FALSE
    )
  }
  
  data
}

# for (prefix in c("SPM", "SPMout", "SuHi")) {
#   lagdata <- recreate_ma(lagdata, prefix)
# }

# compare fdata and lagdata -----------------------------------------------

vars <- c("SPM", "SPMout", "SuHi", "SuHiout")

# Table-style summaries
lag_check <- lagdata %>%
  mutate(date = as.Date(date, format = "%Y/%m/%d"))

f_check <- fdata %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

vars <- c("SPM", "SPMout", "SuHi")

comparison <- bind_rows(lapply(vars, function(v) {
  x <- inner_join(
    lag_check %>% select(city, date, lag_value = all_of(v)),
    f_check   %>% select(city, date, f_value   = all_of(v)),
    by = c("city", "date")
  )
  
  tibble(
    variable = v,
    matched_rows = nrow(x),
    exact_matches = sum(
      !is.na(x$lag_value) & !is.na(x$f_value) &
        x$lag_value == x$f_value
    ),
    different_values = sum(
      !is.na(x$lag_value) & !is.na(x$f_value) &
        x$lag_value != x$f_value
    ),
    lag_only = sum(!is.na(x$lag_value) & is.na(x$f_value)),
    fdata_only = sum(is.na(x$lag_value) & !is.na(x$f_value)),
    both_na = sum(is.na(x$lag_value) & is.na(x$f_value))
  )
}))

print(comparison)
lagdata <- lag_check
fdata <- f_check

# Define clinic-to-city mapping -------------------------------------------

pollen_raw <- read.csv(
  file.path(root_dir, "data", "kyushu_pollen.csv"),
  as.is = TRUE
) %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),
    ClinicID = as.integer(ClinicID),
    Sugi = as.numeric(Sugi),
    Hinoki = as.numeric(Hinoki)
  )

clinic_city <- data.frame(
  ClinicID = c(
    8, 11, 23,       # Fukuoka
    36, 38,          # Kumamoto
    28,              # Nagasaki
    44, 49,          # Oita
    25, 27,          # Saga
    59,              # Kagoshima
    54,              # Miyazaki
    2, 3, 4, 17      # Kitakyushu
  ),
  city = c(
    rep("Fukuoka", 3),
    rep("Kumamoto", 2),
    "Nagasaki",
    rep("Oita", 2),
    rep("Saga", 2),
    "Kagoshima",
    "Miyazaki",
    rep("Kitakyushu", 4)
  )
)

# Remove sentinel values without imputing ---------------------------------

pollen_clean <- pollen_raw %>%
  filter(ClinicID %in% clinic_city$ClinicID) %>%
  mutate(
    Sugi = ifelse(Sugi %in% c(99998, 99999), NA, Sugi),
    Hinoki = ifelse(Hinoki %in% c(99998, 99999), NA, Hinoki)
  )

cat("\n--- Checkpoint 2: sentinel values removed ---\n")
print(colSums(is.na(pollen_clean[, c("Sugi", "Hinoki")])))
print(
  pollen_clean %>%
    summarise(
      remaining_99998_sugi = sum(Sugi == 99998, na.rm = TRUE),
      remaining_99999_sugi = sum(Sugi == 99999, na.rm = TRUE),
      remaining_99998_hinoki = sum(Hinoki == 99998, na.rm = TRUE),
      remaining_99999_hinoki = sum(Hinoki == 99999, na.rm = TRUE)
    )
)

# Collapse duplicate clinic-date rows, if any -----------------------------

pollen_clean <- pollen_clean %>%
  group_by(Date, ClinicID) %>%
  summarise(
    Sugi = if (all(is.na(Sugi))) NA_real_ else mean(Sugi, na.rm = TRUE),
    Hinoki = if (all(is.na(Hinoki))) NA_real_ else mean(Hinoki, na.rm = TRUE),
    .groups = "drop"
  )

# Create every date for every clinic, 1989–2014 ---------------------------

all_dates <- seq.Date(
  from = as.Date("1989-01-01"),
  to = as.Date("2014-12-31"),
  by = "day"
)

pollen_complete <- tidyr::expand_grid(
  Date = all_dates,
  ClinicID = clinic_city$ClinicID
) %>%
  left_join(
    pollen_clean,
    by = c("Date", "ClinicID")
  ) %>%
  left_join(
    clinic_city,
    by = "ClinicID"
  ) %>%
  mutate(
    SuHi_nonimputed = case_when(
      is.na(Sugi) & is.na(Hinoki) ~ NA_real_,
      TRUE ~ rowSums(
        cbind(Sugi, Hinoki),
        na.rm = TRUE
      )
    )
  )

cat("\n--- Checkpoint 3: complete clinic-date data ---\n")
print(dim(pollen_complete))
print(range(pollen_complete$Date))
print(table(pollen_complete$city))
print(
  pollen_complete %>%
    summarise(
      n_rows = n(),
      n_sugi = sum(!is.na(Sugi)),
      n_hinoki = sum(!is.na(Hinoki)),
      n_suhi = sum(!is.na(SuHi_nonimputed))
    )
)

# Average SuHi across clinics within each city ----------------------------

pollen_city <- pollen_complete %>%
  group_by(Date, city) %>%
  summarise(
    SuHi_nonimputed = if (
      all(is.na(SuHi_nonimputed))
    ) {
      NA_real_
    } else {
      mean(SuHi_nonimputed, na.rm = TRUE)
    },
    n_clinics_available = sum(!is.na(SuHi_nonimputed)),
    .groups = "drop"
  ) %>%
  rename(date = Date)

cat("\n--- Checkpoint 4: city-level pollen data ---\n")
print(dim(pollen_city))
print(head(pollen_city))
print(
  pollen_city %>%
    group_by(city) %>%
    summarise(
      first_date = min(date),
      last_date = max(date),
      nonmissing_days = sum(!is.na(SuHi_nonimputed)),
      total_suhi = sum(SuHi_nonimputed, na.rm = TRUE),
      .groups = "drop"
    )
)

cat("\n--- Checkpoint 5: city names ---\n")
print(sort(unique(pollen_city$city)))
print(sort(unique(lagdata$city)))
print(sort(unique(fdata$city)))

# Merge new SuHi onto lagdata and fdata -----------------------------------

lagdata_nonimputed <- lagdata %>%
  left_join(pollen_city %>% select(date, city, SuHi_nonimputed), by = c("date", "city"))

fdata_nonimputed <- fdata %>%
  left_join(pollen_city %>% select(date, city, SuHi_nonimputed), by = c("date", "city"))

cat("\n--- Checkpoint 6: merged datasets ---\n")
cat(
  "lagdata rows before/after:",
  nrow(lagdata),
  nrow(lagdata_nonimputed),
  "\n"
)

cat(
  "fdata rows before/after:",
  nrow(fdata),
  nrow(fdata_nonimputed),
  "\n"
)

cat(
  "Non-missing new SuHi in lagdata:",
  sum(!is.na(lagdata_nonimputed$SuHi_nonimputed)),
  "\n"
)

cat(
  "Non-missing new SuHi in fdata:",
  sum(!is.na(fdata_nonimputed$SuHi_nonimputed)),
  "\n"
)

lagdata_nonimputed %>%
  summarise(
    existing_nonmissing = sum(!is.na(SuHi)),
    new_nonmissing = sum(!is.na(SuHi_nonimputed)),
    both_nonmissing = sum(!is.na(SuHi) & !is.na(SuHi_nonimputed)),
    existing_only = sum(!is.na(SuHi) & is.na(SuHi_nonimputed)),
    new_only = sum(is.na(SuHi) & !is.na(SuHi_nonimputed))
  )

# Compare existing imputed SuHi against new SuHi --------------------------

compare_suhi <- function(data, dataset_name) {
  comparison <- data %>%
    filter(
      !is.na(SuHi),
      !is.na(SuHi_nonimputed)
    )
  
  data.frame(
    dataset = dataset_name,
    total_existing = sum(comparison$SuHi),
    total_nonimputed = sum(comparison$SuHi_nonimputed),
    total_difference =
      sum(comparison$SuHi_nonimputed) -
      sum(comparison$SuHi),
    percent_difference =
      100 * (
        sum(comparison$SuHi_nonimputed) -
          sum(comparison$SuHi)
      ) / sum(comparison$SuHi),
    overlapping_rows = nrow(comparison),
    correlation = cor(
      comparison$SuHi,
      comparison$SuHi_nonimputed
    ),
    mean_absolute_difference = mean(
      abs(comparison$SuHi - comparison$SuHi_nonimputed)
    )
  )
}

comparison_summary <- bind_rows(
  compare_suhi(lagdata_nonimputed, "lagdata"),
  compare_suhi(fdata_nonimputed, "fdata")
)

cat("\n--- Checkpoint 7: SuHi comparison ---\n")
print(comparison_summary)

comparison_by_city <- lagdata_nonimputed %>%
  group_by(city) %>%
  summarise(
    existing_total = sum(SuHi, na.rm = TRUE),
    nonimputed_total = sum(SuHi_nonimputed, na.rm = TRUE),
    difference = nonimputed_total - existing_total,
    percent_difference = 100 * difference / existing_total,
    overlapping_rows = sum(
      !is.na(SuHi) & !is.na(SuHi_nonimputed)
    ),
    correlation = cor(
      SuHi,
      SuHi_nonimputed,
      use = "complete.obs"
    ),
    .groups = "drop"
  )

print(comparison_by_city)

# create lag and ma columns -----------------------------------------------

add_suhi_lags <- function(data) {
  data %>%
    mutate(.original_order = row_number()) %>%
    arrange(city, date) %>%
    group_by(city) %>%
    mutate(
      SuHi_nonimputed0 = SuHi_nonimputed,
      SuHi_nonimputed1 = lag(SuHi_nonimputed, 1),
      SuHi_nonimputed2 = lag(SuHi_nonimputed, 2),
      SuHi_nonimputed3 = lag(SuHi_nonimputed, 3),
      SuHi_nonimputed4 = lag(SuHi_nonimputed, 4),
      SuHi_nonimputed5 = lag(SuHi_nonimputed, 5),
      
      SuHi_nonimputedma0.1 = rowMeans(
        cbind(SuHi_nonimputed0, SuHi_nonimputed1),
        na.rm = FALSE
      ),
      SuHi_nonimputedma0.2 = rowMeans(
        cbind(SuHi_nonimputed0, SuHi_nonimputed1, SuHi_nonimputed2),
        na.rm = FALSE
      ),
      SuHi_nonimputedma0.3 = rowMeans(
        cbind(
          SuHi_nonimputed0, SuHi_nonimputed1,
          SuHi_nonimputed2, SuHi_nonimputed3
        ),
        na.rm = FALSE
      ),
      SuHi_nonimputedma0.4 = rowMeans(
        cbind(
          SuHi_nonimputed0, SuHi_nonimputed1,
          SuHi_nonimputed2, SuHi_nonimputed3,
          SuHi_nonimputed4
        ),
        na.rm = FALSE
      ),
      SuHi_nonimputedma0.5 = rowMeans(
        cbind(
          SuHi_nonimputed0, SuHi_nonimputed1,
          SuHi_nonimputed2, SuHi_nonimputed3,
          SuHi_nonimputed4, SuHi_nonimputed5
        ),
        na.rm = FALSE
      )
    ) %>%
    ungroup() %>%
    arrange(.original_order) %>%
    select(-.original_order)
}

lagdata_nonimputed <- add_suhi_lags(lagdata_nonimputed)
fdata_nonimputed <- add_suhi_lags(fdata_nonimputed)

# Drop float estimates by rounding by 3 -----------------------------------

# round_columns <- function(data, digits = 3) {
#   cols <- grep(
#     "^(SuHi_nonimputed|SuHi|SPM|SO2|NO2|Tave|Tmax|Tmin|BPave|VPave)",
#     names(data),
#     value = TRUE,
#     ignore.case = TRUE
#   )
  
#   data[cols] <- lapply(data[cols], function(x) {
#     ifelse(
#       is.na(x),
#       NA,
#       sprintf(paste0("%.", digits, "f"), round(x, digits))
#     )
#   })
  
#   data
# }

# lagdata <- round_columns(lagdata_nonimputed, 3)
# fdata   <- round_columns(fdata_nonimputed, 3)
lagdata <- lagdata_nonimputed
fdata <- fdata_nonimputed

# write csv ---------------------------------------------------------------

write.csv(lagdata, file.path(root_dir, "data/lagdata.csv"), row.names = FALSE)
write.csv(fdata, file.path(root_dir, "data/fdata.csv"), row.names = FALSE)
