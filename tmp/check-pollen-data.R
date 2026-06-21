
rm(list = ls())

library(dplyr)
library(tibble)
library(rlang)

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

suhiout_cols <- grep("^SuHiout", names(lagdata), value = TRUE)
for (old_col in suhiout_cols) {
  new_col <- sub("^SuHiout", "SuHi", old_col)
  lagdata[[new_col]] <- lagdata[[old_col]]
}

lagdata <- lagdata %>%
  select(
    -matches("^SuHiout"),
    -matches("^SuHi05"),
    -matches("ma0\\.[67]$")
  )

fdata <- fdata %>%
  select(
    -matches("^SuHiout"),
    -matches("^SuHi05"),
    -matches("ma0\\.[67]$")
  )

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

for (prefix in c("SPM", "SPMout", "SuHi")) {
  lagdata <- recreate_ma(lagdata, prefix)
}

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

# write csv ---------------------------------------------------------------

write.csv(lagdata, file.path(root_dir, "data/lagdata.csv"), row.names = FALSE)
write.csv(fdata, file.path(root_dir, "data/fdata.csv"), row.names = FALSE)
