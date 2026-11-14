rm(list = ls())

library(dplyr)
library(tidyr)
library(lubridate)

root_dir <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality"

# Read raw pollen data ----------------------------------------------------

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

cat("\n--- Checkpoint 1: raw pollen data ---\n")
print(dim(pollen_raw))
print(range(pollen_raw$Date, na.rm = TRUE))
print(head(pollen_raw))
print(summary(pollen_raw[, c("Sugi", "Hinoki")]))

# Define clinic-to-city mapping -------------------------------------------

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

# Read lagdata and fdata --------------------------------------------------

lagdata <- read.csv(file.path(root_dir, "data", "lagdata.csv"), as.is = TRUE) %>%
  mutate(date = as.Date(date), city = as.character(city))

fdata <- read.csv(file.path(root_dir, "data", "fdata.csv"), as.is = TRUE) %>%
  mutate(date = as.Date(date), city = as.character(city))

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

# Comparison by city
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
    arrange(city, date) %>%
    group_by(city) %>%
    mutate(
      SuHi_nonimputed0 = SuHi_nonimputed,
      SuHi_nonimputed1 = lag(SuHi_nonimputed, 1),
      SuHi_nonimputed2 = lag(SuHi_nonimputed, 2),
      SuHi_nonimputed3 = lag(SuHi_nonimputed, 3),
      SuHi_nonimputed4 = lag(SuHi_nonimputed, 4),
      SuHi_nonimputed5 = lag(SuHi_nonimputed, 5),
      
      # Moving averages:
      # ma1 = lag 0–1, ma2 = lag 0–2, etc.
      SuHi_nonimputedma0.1 = rowMeans(
        cbind(
          SuHi_nonimputed0,
          SuHi_nonimputed1
        ),
        na.rm = FALSE
      ),
      
      SuHi_nonimputedma0.2 = rowMeans(
        cbind(
          SuHi_nonimputed0,
          SuHi_nonimputed1,
          SuHi_nonimputed2
        ),
        na.rm = FALSE
      ),
      
      SuHi_nonimputedma0.3 = rowMeans(
        cbind(
          SuHi_nonimputed0,
          SuHi_nonimputed1,
          SuHi_nonimputed2,
          SuHi_nonimputed3
        ),
        na.rm = FALSE
      ),
      
      SuHi_nonimputedma0.4 = rowMeans(
        cbind(
          SuHi_nonimputed0,
          SuHi_nonimputed1,
          SuHi_nonimputed2,
          SuHi_nonimputed3,
          SuHi_nonimputed4
        ),
        na.rm = FALSE
      ),
      
      SuHi_nonimputedma0.5 = rowMeans(
        cbind(
          SuHi_nonimputed0,
          SuHi_nonimputed1,
          SuHi_nonimputed2,
          SuHi_nonimputed3,
          SuHi_nonimputed4,
          SuHi_nonimputed5
        ),
        na.rm = FALSE
      )
    ) %>%
    ungroup()
}

lagdata_nonimputed <- add_suhi_lags(lagdata_nonimputed)
fdata_nonimputed <- add_suhi_lags(fdata_nonimputed)

# Drop float estimates by rounding by 3 -----------------------------------

round_columns <- function(data, digits = 3) {
  variable_pattern <- "^(SuHi_nonimputed|SuHi|SPM|SO2|NO2|Tave|Tmax|Tmin|BPave|VPave)"
  
  columns_to_round <- grep(
    variable_pattern,
    names(data),
    value = TRUE,
    ignore.case = TRUE
  )
  
  data[columns_to_round] <- lapply(
    data[columns_to_round],
    function(x) round(x, digits)
  )
  
  data
}

lagdata_nonimputed <- round_columns(lagdata_nonimputed, 3)
fdata_nonimputed <- round_columns(fdata_nonimputed, 3)
