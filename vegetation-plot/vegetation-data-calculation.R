rm(list = ls())

library(dplyr)
library(readr)
library(tidyr)

# Setup -------------------------------------------------------------------

os <- Sys.info()[["sysname"]]

if ("Darwin" %in% os) {
  root_dir <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality/vegetation-plot"
} else {
  root_dir <- "C:/Users/kyuhu/OneDrive/Documents/Github/pollen-respiratory-mortality/vegetation-plot"
}

out_dir <- file.path(root_dir, "output")

input_file <- file.path(out_dir, "forest-coverage-kyushu.csv")

# Read and clean data ------------------------------------------------------

forest_raw <- read_csv(
  input_file,
  col_types = cols(
    year = col_integer(),
    prefecture = col_integer(),
    name = col_character(),
    forest_area = col_character(),
    artificial_forest_area = col_character(),
    national_land_area = col_character(),
    forest_rate = col_character(),
    planted_forest_rate = col_character()
  )
)

forest_clean <- forest_raw %>%
  mutate(
    forest_area_ha = parse_number(forest_area),
    planted_forest_area_ha = parse_number(artificial_forest_area),
    total_land_area_ha = parse_number(national_land_area),
    
    # kept only for checking against the source table
    forest_rate_reported_pct = parse_number(forest_rate),
    planted_forest_rate_reported_pct = parse_number(planted_forest_rate)
  )

# Combined totals by year --------------------------------------------------

forest_by_year <- forest_clean %>%
  group_by(year) %>%
  summarise(
    n_prefectures = n_distinct(prefecture),
    total_land_area_ha = sum(total_land_area_ha, na.rm = TRUE),
    forest_area_ha = sum(forest_area_ha, na.rm = TRUE),
    planted_forest_area_ha = sum(planted_forest_area_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    forest_coverage_pct =
      100 * forest_area_ha / total_land_area_ha,
    
    planted_forest_share_of_forest_pct =
      100 * planted_forest_area_ha / forest_area_ha,
    
    planted_forest_share_of_total_land_pct =
      100 * planted_forest_area_ha / total_land_area_ha
  ) %>%
  arrange(year)

write_csv(
  forest_by_year,
  file.path(out_dir, "forest-coverage-kyushu-combined-by-year.csv")
)

print(forest_by_year)

# Average across years -----------------------------------------------------

# Important:
# The percentage values below are calculated from the raw area totals across
# all years, not from a simple average of percentage columns.
#
# forest_coverage_pct =
#   sum(forest area across years) / sum(total land area across years)
#
# planted_forest_share_of_forest_pct =
#   sum(planted forest area across years) / sum(forest area across years)
#
# planted_forest_share_of_total_land_pct =
#   sum(planted forest area across years) / sum(total land area across years)

forest_average <- forest_by_year %>%
  summarise(
    year = "Average 2007-2020",
    n_years = n(),
    average_total_land_area_ha = mean(total_land_area_ha, na.rm = TRUE),
    average_forest_area_ha = mean(forest_area_ha, na.rm = TRUE),
    average_planted_forest_area_ha = mean(planted_forest_area_ha, na.rm = TRUE),
    
    forest_coverage_pct =
      100 * sum(forest_area_ha, na.rm = TRUE) /
      sum(total_land_area_ha, na.rm = TRUE),
    
    planted_forest_share_of_forest_pct =
      100 * sum(planted_forest_area_ha, na.rm = TRUE) /
      sum(forest_area_ha, na.rm = TRUE),
    
    planted_forest_share_of_total_land_pct =
      100 * sum(planted_forest_area_ha, na.rm = TRUE) /
      sum(total_land_area_ha, na.rm = TRUE)
  )

write_csv(
  forest_average,
  file.path(out_dir, "forest-coverage-kyushu-average-2007-to-2020.csv")
)

print(forest_average)

# Combined table with an average row --------------------------------------

forest_by_year_with_average <- forest_by_year %>%
  mutate(
    year = as.character(year),
    n_years = NA_integer_
  ) %>%
  rename(
    average_total_land_area_ha = total_land_area_ha,
    average_forest_area_ha = forest_area_ha,
    average_planted_forest_area_ha = planted_forest_area_ha
  ) %>%
  select(
    year,
    n_years,
    n_prefectures,
    average_total_land_area_ha,
    average_forest_area_ha,
    average_planted_forest_area_ha,
    forest_coverage_pct,
    planted_forest_share_of_forest_pct,
    planted_forest_share_of_total_land_pct
  ) %>%
  bind_rows(
    forest_average %>%
      mutate(n_prefectures = NA_integer_) %>%
      select(
        year,
        n_years,
        n_prefectures,
        average_total_land_area_ha,
        average_forest_area_ha,
        average_planted_forest_area_ha,
        forest_coverage_pct,
        planted_forest_share_of_forest_pct,
        planted_forest_share_of_total_land_pct
      )
  )

write_csv(
  forest_by_year_with_average,
  file.path(out_dir, "forest-coverage-kyushu-by-year.csv")
)

print(forest_by_year_with_average)

# Rounded manuscript-friendly table ----------------------------------------

forest_by_year_with_average_rounded <- forest_by_year_with_average %>%
  mutate(
    across(
      c(
        average_total_land_area_ha,
        average_forest_area_ha,
        average_planted_forest_area_ha
      ),
      ~ round(.x, 0)
    ),
    across(
      c(
        forest_coverage_pct,
        planted_forest_share_of_forest_pct,
        planted_forest_share_of_total_land_pct
      ),
      ~ round(.x, 1)
    )
  )

write_csv(
  forest_by_year_with_average_rounded,
  file.path(out_dir, "forest-coverage-kyushu-by-year-rounded.csv")
)

print(forest_by_year_with_average_rounded)

# Sanity check against reported percentage columns -------------------------

# checks whether the reported prefecture-level percentages roughly match
# percentages recomputed from the raw area columns.
rate_check <- forest_clean %>%
  mutate(
    forest_rate_computed_pct =
      100 * forest_area_ha / total_land_area_ha,
    
    planted_forest_rate_computed_pct =
      100 * planted_forest_area_ha / forest_area_ha,
    
    forest_rate_diff =
      forest_rate_computed_pct - forest_rate_reported_pct,
    
    planted_forest_rate_diff =
      planted_forest_rate_computed_pct - planted_forest_rate_reported_pct
  )

write_csv(
  rate_check,
  file.path(out_dir, "forest-coverage-kyushu-rate-check.csv")
)