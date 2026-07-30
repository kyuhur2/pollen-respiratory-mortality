library(dplyr)

proj <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality"

lagdata <- read.csv(file.path(proj, "data/lagdata.csv"))

# Ensure date is date-like if needed
lagdata$date <- as.Date(lagdata$date)

# Table-style summary function
summarise_by_city <- function(df, var) {
  if (!var %in% names(df)) {
    warning(paste("Column not found:", var))
    return(NULL)
  }

  df %>%
    filter(month %in% c(2, 3, 4)) %>%
    group_by(city) %>%
    summarise(
      variable = var,
      mean = mean(.data[[var]], na.rm = TRUE),
      sd = sd(.data[[var]], na.rm = TRUE),
      median = median(.data[[var]], na.rm = TRUE),
      iqr = IQR(.data[[var]], na.rm = TRUE),
      p75 = quantile(.data[[var]], 0.75, na.rm = TRUE),
      p80 = quantile(.data[[var]], 0.80, na.rm = TRUE),
      p85 = quantile(.data[[var]], 0.85, na.rm = TRUE),
      max = max(.data[[var]], na.rm = TRUE),
      n = sum(!is.na(.data[[var]])),
      n_na = sum(is.na(.data[[var]])),
      .groups = "drop"
    ) %>%
    mutate(across(
      c(mean, sd, median, iqr, p75, p80, p85, max),
      ~ round(.x, 1)
    ))
}

# Variables to check
vars_to_check <- c(
  "SPM",
  "SPMout",
  "SuHi",
  "SuHiout",
)

# Create one combined summary table
summary_table <- bind_rows(
  lapply(vars_to_check, function(v) summarise_by_city(lagdata, v))
)

print(summary_table, n = Inf)

# Optional: export to CSV
write.csv(
  summary_table,
  file.path(proj, "tmp/spm-pollen-summaries.csv"),
  row.names = FALSE
)