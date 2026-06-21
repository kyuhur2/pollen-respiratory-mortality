
tmp <- read.csv(paste0(root_dir, "/data/lagdata.csv"))

tmp <- tmp %>%
  filter(city %in% CITIES, month %in% c(2, 3, 4)) %>%
  mutate(city = factor(city, levels = CITIES))

# tmp table
table2_new <- bind_rows(
  make_row_mean_sd(tmp, SPMout, "SPM (μg/m3)", "Mean ± SD"),
  make_row_median_iqr(tmp, SPMout, "SPM (μg/m3)", "Median, IQR"),
  make_row_max(tmp, SPMout, "SPM (μg/m3)", "Max"),
  make_row_mean_sd(tmp, SuHiout, "Pollen count", "Mean ± SD"),
  make_row_median_iqr(tmp, SuHiout, "Pollen count", "Median, IQR"),
  make_row_percentile(tmp, SuHiout, 0.75, "Pollen count", "75"),
  make_row_percentile(tmp, SuHiout, 0.80, "Pollen count", "80"),
  make_row_percentile(tmp, SuHiout, 0.85, "Pollen count", "85"),
  make_row_max(tmp, SuHiout, "Pollen count", "Maximum"),
  make_row_mean_sd(tmp, SO2, "SO2 (ppb)", "Mean ± SD"),
  make_row_mean_sd(tmp, NO2, "NO2 (ppb)", "Mean ± SD"),
  make_row_mean_sd(tmp, Tave, "Mean Temperature (°C)", "Mean ± SD"),
  make_row_mean_sd(tmp, RHave, "Relative Humidity (%)", "Mean ± SD")
)

write.csv(table2_new, file = file.path(root_dir, "tables/table2.csv"), row.names = FALSE)

# separation