
library(ggplot2)
library(ggtext)
library(dplyr)
library(cowplot)
library(tibble)

# setup -----------------------------------------------------------------------------------------------------------

rm(list = ls())  # reset
root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
source(paste0(root_dir, "/proj/functions.R"))

# main ------------------------------------------------------------------------------------------------------------

# parameters
data_0 <- {
  read.csv(file = paste0(root_dir, "/data/metafor_noninteractive.csv"))
}
data_1 <- list(
  transform(
    read.csv(file.path(root_dir, "data/metafor_bisection_perc75.csv")),
    bisection_method = "perc75"
  ),
  transform(
    read.csv(file.path(root_dir, "data/metafor_bisection_perc80.csv")),
    bisection_method = "perc80"
  ),
  transform(
    read.csv(file.path(root_dir, "data/metafor_bisection_perc85.csv")),
    bisection_method = "perc85"
  )
)
data_2 <- list(
  transform(
    read.csv(file.path(root_dir, "data/metafor_bisection_abs25.csv")),
    bisection_method = "abs25"
  ),
  transform(
    read.csv(file.path(root_dir, "data/metafor_bisection_abs50.csv")),
    bisection_method = "abs50"
  ),
  transform(
    read.csv(file.path(root_dir, "data/metafor_bisection_abs75.csv")),
    bisection_method = "abs75"
  )
)

# no2 conf
data_3 <- {
  list(
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_no2_perc75.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_no2_perc80.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_no2_perc85.csv"
    ))
  )
}
data_4 <- {
  list(
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_no2_abs25.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_no2_abs50.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_no2_abs75.csv"
    ))
  )
}

# so2 conf
data_5 <- {
  list(
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_so2_perc75.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_so2_perc80.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_so2_perc85.csv"
    ))
  )
}
data_6 <- {
  list(
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_so2_abs25.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_so2_abs50.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_so2_abs75.csv"
    ))
  )
}

# sensitivity analysis
data_7 <- read.csv(file = paste0(root_dir, "/data/noninteractive_aggregated.csv"))

# params
CITIES <- c(
  "Fukuoka",
  "Kitakyushu",
  "Kumamoto",
  "Kagoshima",
  "Nagasaki",
  "Oita",
  "Miyazaki",
  "Saga"
)

# figure 1 --------------------------------------------------------------------------------------------------------

# moved to misc.R because code no longer works (one of the libraries is pulling data from a location that doesn't
# exist)

# figure 2 --------------------------------------------------------------------------------------------------------

# params
tmp <- data_0[data_0$exposure %in% c("spm") &
              data_0$outcome %in% c("all", "circ", "resp") &
                data_0$lag %in% c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"), ]
y_lower_bound <- min(tmp$cil, na.rm = TRUE)
y_upper_bound <- max(tmp$ciu, na.rm = TRUE)
point_color = "red"
point_shape = 18

# plots
a <- {
  create_noninteractive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data = data_0,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    point_color = point_color,
    point_shape = point_shape
  )
}
b <- {
  create_noninteractive_plot(
    exposure = "spm",
    outcome = "resp",
    lags = c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data = data_0,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    point_color = point_color,
    point_shape = point_shape
  )
}
c <- {
  create_noninteractive_plot(
    exposure = "spm",
    outcome = "circ",
    lags = c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data = data_0,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    point_color = point_color,
    point_shape = point_shape
  )
}
blank_plot <- ggplot() + theme_void()
top_row <- plot_grid(
  blank_plot,
  a + ggtitle("[A] All-cause") + theme(plot.title = element_text(hjust = 0.5, face = "bold")),
  blank_plot,
  labels = NULL,
  ncol = 3,
  rel_widths = c(1, 2, 1),
  align = 'h',
  axis = 'tb'
)
bottom_row <- plot_grid(
  b + ggtitle("[B] Respiratory") + theme(plot.title = element_text(hjust = 0.5, face = "bold")),
  c + ggtitle("[C] Cardiovascular") + theme(plot.title = element_text(hjust = 0.5, face = "bold")),
  labels = NULL,
  ncol = 2,
  align = 'hv',
  axis = 'tblr'
)
plot2 <- {
  plot_grid(
    top_row,
    bottom_row,
    ncol = 1,
    rel_heights = c(1, 1)
  )
}
{
  ggsave(
    filename = paste0(root_dir, "/plots/figure2.pdf"),
    plot = plot2,
    device = "pdf",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )
}

# figure 3 --------------------------------------------------------------------------------------------------------

# params
tmp <- do.call(rbind, c(data_1, data_2))
tmp <- tmp[tmp$exposure %in% c("spm") &
             tmp$outcome %in% c("all", "circ", "resp") &
             tmp$lag %in% c("0", "1", "2"), ]
y_lower_bound <- min(tmp$cil, na.rm = TRUE)
y_upper_bound <- max(tmp$ciu, na.rm = TRUE)
cutoffs_vec_perc <- c("75th", "80th", "85th")
cutoffs_vec_abs <- c(25, 50, 75)
debug <- FALSE

# plots
a <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
b <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
c <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
d <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
e <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
f <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
plot3 <- {
  plot_grid(
    a,
    b,
    c,
    d,
    e,
    f,
    nrow = 3,
    ncol = 2,
    labels = c(
      "[A] All-cause",
      "[D] All-cause",
      "[B] Respiratory",
      "[E] Respiratory",
      "[C] Cardiovascular",
      "[F] Cardiovascular"
    ),
    label_x = 0.05,
    hjust = 0
  )
}
{
  ggsave(
    filename = paste0(root_dir, "/plots/figure3.pdf"),
    plot = plot3,
    device = "pdf",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )
}

# table 1 ---------------------------------------------------------------------------------------------------------

tmp <- read.csv(paste0(root_dir, "/data/fdata.csv"))

# filter for feb, mar, apr
tmp <- tmp %>%
  filter(city %in% CITIES, month %in% c(2, 3, 4)) %>%
  mutate(city = factor(city, levels = CITIES))

# per-city totals + daily mean/median
city_summary <- tmp %>%
  group_by(city) %>%
  summarise(
    total_all  = sum(all,  na.rm = TRUE),
    total_circ = sum(circ, na.rm = TRUE),
    total_resp = sum(resp, na.rm = TRUE),
    mean_all   = round(mean(all,  na.rm = TRUE), 1),
    sd_all     = round(sd(all,   na.rm = TRUE), 1),
    mean_circ  = round(mean(circ,  na.rm = TRUE), 1),
    sd_circ    = round(sd(circ,   na.rm = TRUE), 1),
    mean_resp  = round(mean(resp,  na.rm = TRUE), 1),
    sd_resp    = round(sd(resp,   na.rm = TRUE), 1),
    .groups = "drop"
  )

# sum by date -> compute totals/means/medians across days
combined_daily <- tmp %>%
  group_by(date) %>%
  summarise(
    all  = sum(all,  na.rm = TRUE),
    circ = sum(circ, na.rm = TRUE),
    resp = sum(resp, na.rm = TRUE),
    .groups = "drop"
  )

combined_summary <- combined_daily %>%
  summarise(
    city = "Combined",
    total_all  = sum(all,  na.rm = TRUE),
    total_circ = sum(circ, na.rm = TRUE),
    total_resp = sum(resp, na.rm = TRUE),
    mean_all   = round(mean(all,  na.rm = TRUE), 1),
    sd_all     = round(sd(all,   na.rm = TRUE), 1),
    mean_circ  = round(mean(circ,  na.rm = TRUE), 1),
    sd_circ    = round(sd(circ,   na.rm = TRUE), 1),
    mean_resp  = round(mean(resp,  na.rm = TRUE), 1),
    sd_resp    = round(sd(resp,   na.rm = TRUE), 1)
  )

# bind everything together
summary_all <- bind_rows(
  combined_summary,
  city_summary %>% mutate(city = as.character(city)),
)

# format
table1 <- summary_all %>%
  transmute(
    City = city,
    `All-cause Total`                 = formatC(total_all,  format = "d", big.mark = ","),
    `Cardiovascular Total`            = formatC(total_circ, format = "d", big.mark = ","),
    `Respiratory Total`               = formatC(total_resp, format = "d", big.mark = ","),
    `All-cause Daily (Mean, SD)`      = sprintf("%.1f ± %.1f", mean_all, sd_all),
    `Cardiovascular Daily (Mean, SD)` = sprintf("%.1f ± %.1f", mean_circ, sd_circ),
    `Respiratory Daily (Mean, SD)`    = sprintf("%.1f ± %.1f", mean_resp, sd_resp)
  ) %>%
  mutate(City = factor(City, levels = c("Combined", CITIES))) %>%
  arrange(City)

write.csv(table1, file = file.path(root_dir, "tables/table1.csv"), row.names = FALSE)

# table 2 ---------------------------------------------------------------------------------------------------------

tmp <- read.csv(paste0(root_dir, "/data/fdata.csv"))

tmp <- tmp %>%
  filter(city %in% CITIES, month %in% c(2, 3, 4)) %>%
  mutate(city = factor(city, levels = CITIES))

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

# build table
table2 <- bind_rows(
  make_row_mean_sd(tmp, SPM, "SPM (μg/m3)", "Mean ± SD"),
  make_row_median_iqr(tmp, SPM, "SPM (μg/m3)", "Median, IQR"),
  make_row_max(tmp, SPM, "SPM (μg/m3)", "Max"),
  make_row_mean_sd(tmp, SuHi, "Pollen count", "Mean ± SD"),
  make_row_median_iqr(tmp, SuHi, "Pollen count", "Median, IQR"),
  make_row_percentile(tmp, SuHi, 0.75, "Pollen count", "75"),
  make_row_percentile(tmp, SuHi, 0.80, "Pollen count", "80"),
  make_row_percentile(tmp, SuHi, 0.85, "Pollen count", "85"),
  make_row_max(tmp, SuHi, "Pollen count", "Maximum"),
  make_row_mean_sd(tmp, SO2, "SO2 (ppb)", "Mean ± SD"),
  make_row_mean_sd(tmp, NO2, "NO2 (ppb)", "Mean ± SD"),
  make_row_mean_sd(tmp, Tave, "Mean Temperature (°C)", "Mean ± SD"),
  make_row_mean_sd(tmp, RHave, "Relative Humidity (%)", "Mean ± SD")
)

write.csv(table2, file = file.path(root_dir, "tables/table2.csv"), row.names = FALSE)

# table S1 --------------------------------------------------------------------------------------------------------

tmp <- read.csv(paste0(root_dir, "/data/fdata.csv"))

# filter for feb, mar, apr
tmp <- tmp %>%
  filter(city %in% CITIES, month %in% c(2, 3, 4)) %>%
  mutate(city = factor(city, levels = CITIES))

# per-city totals + daily mean/median but by age >=65
city_summary_65 <- tmp %>%
  group_by(city) %>%
  summarise(
    total_all65  = sum(all.age65,  na.rm = TRUE),
    total_circ65 = sum(circ.age65, na.rm = TRUE),
    total_resp65 = sum(resp.age65, na.rm = TRUE),
    mean_all65   = round(mean(all.age65,  na.rm = TRUE), 1),
    sd_all65     = round(sd(all.age65,   na.rm = TRUE), 1),
    mean_circ65  = round(mean(circ.age65,  na.rm = TRUE), 1),
    sd_circ65    = round(sd(circ.age65,   na.rm = TRUE), 1),
    mean_resp65  = round(mean(resp.age65,  na.rm = TRUE), 1),
    sd_resp65    = round(sd(resp.age65,   na.rm = TRUE), 1),
    .groups = "drop"
  )

# sum by date -> compute totals/means/medians across days
combined_daily_65 <- tmp %>%
  group_by(date) %>%
  summarise(
    all.age65  = sum(all.age65,  na.rm = TRUE),
    circ.age65 = sum(circ.age65, na.rm = TRUE),
    resp.age65 = sum(resp.age65, na.rm = TRUE),
    .groups = "drop"
  )

combined_summary_65 <- combined_daily_65 %>%
  summarise(
    city = "Combined",
    total_all65  = sum(all.age65,  na.rm = TRUE),
    total_circ65 = sum(circ.age65, na.rm = TRUE),
    total_resp65 = sum(resp.age65, na.rm = TRUE),
    mean_all65   = round(mean(all.age65,  na.rm = TRUE), 1),
    sd_all65     = round(sd(all.age65,   na.rm = TRUE), 1),
    mean_circ65  = round(mean(circ.age65,  na.rm = TRUE), 1),
    sd_circ65    = round(sd(circ.age65,   na.rm = TRUE), 1),
    mean_resp65  = round(mean(resp.age65,  na.rm = TRUE), 1),
    sd_resp65    = round(sd(resp.age65,   na.rm = TRUE), 1)
  )

# bind everything together
summary_all_65 <- bind_rows(
  combined_summary_65,
  city_summary_65 %>% mutate(city = as.character(city))
)

# format
tableS1 <- summary_all_65 %>%
  transmute(
    City = city,
    `All-cause Total (65+)`      = formatC(total_all65,  format = "d", big.mark = ","),
    `Cardiovascular Total (65+)` = formatC(total_circ65, format = "d", big.mark = ","),
    `Respiratory Total (65+)`    = formatC(total_resp65, format = "d", big.mark = ","),
    `All-cause Daily (Mean, SD)`      = sprintf("%.1f ± %.1f", mean_all65,  sd_all65),
    `Cardiovascular Daily (Mean, SD)` = sprintf("%.1f ± %.1f", mean_circ65, sd_circ65),
    `Respiratory Daily (Mean, SD)`    = sprintf("%.1f ± %.1f", mean_resp65, sd_resp65)
  ) %>%
  mutate(City = factor(City, levels = c("Combined", CITIES))) %>%
  arrange(City)

write.csv(tableS1, file = file.path(root_dir, "tables/tableS1.csv"), row.names = FALSE)

# table S2 --------------------------------------------------------------------------------------------------------

tmp <- read.csv(paste0(root_dir, "/data/fdata.csv"))

tmp <- tmp %>%
  filter(city %in% CITIES, month %in% c(2, 3, 4)) %>%
  mutate(city = factor(city, levels = CITIES))

# row builders (one row per statistic, columns = cities)
make_row_median_iqr <- function(data, var, exposure_label, stat_label = "Median, IQR") {
  vals <- data %>%
    group_by(city) %>%
    summarise(
      med = round(median({{var}}, na.rm = TRUE), 1),
      iqr = round(IQR({{var}},    na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(city) %>%
    transmute(val = paste0(med, ", ", iqr)) %>%
    pull(val)
  tibble::tibble_row(Exposure = exposure_label, Statistics = stat_label,
             !!!setNames(as.list(vals), CITIES))
}

make_row_max <- function(data, var, exposure_label, stat_label = "Max") {
  vals <- data %>%
    group_by(city) %>%
    summarise(mx = round(max({{var}}, na.rm = TRUE), 1), .groups = "drop") %>%
    arrange(city) %>%
    transmute(val = as.character(mx)) %>%
    pull(val)
  tibble::tibble_row(Exposure = exposure_label, Statistics = stat_label,
             !!!setNames(as.list(vals), CITIES))
}

# build table
tableS2 <- dplyr::bind_rows(
  make_row_median_iqr(tmp, SO2,  "SO2 (ppb)", "Median, IQR"),
  make_row_max(tmp, SO2,  "SO2 (ppb)", "Max"),
  make_row_median_iqr(tmp, NO2,  "NO2 (ppb)", "Median, IQR"),
  make_row_max(tmp, NO2,  "NO2 (ppb)", "Max"),
  make_row_median_iqr(tmp, Tave, "Mean Temperature (°C)", "Median, IQR"),
  make_row_max(tmp, Tave, "Mean Temperature (°C)", "Max"),
  make_row_median_iqr(tmp, RHave, "Relative Humidity (%)", "Median, IQR"),
  make_row_max(tmp, RHave, "Relative Humidity (%)", "Max")
)

write.csv(tableS2, file = file.path(root_dir, "tables/tableS2.csv"), row.names = FALSE)

# table S3 --------------------------------------------------------------------------------------------------------

# manually created

# table S4 --------------------------------------------------------------------------------------------------------

# manually created

# table S5 --------------------------------------------------------------------------------------------------------

tmp <- do.call(rbind, c(data_1, data_2))

# filter to spm / all | circ | resp / lag 0, 1, 2 and select the desired columns
tableS5_1 <- tmp %>%
  filter(
    exposure == "spm",
    outcome %in% c("all", "circ", "resp"),
    lag %in% c("0", "1", "2")
  ) %>%
  select(
    I2,
    Q,
    p.Qtest,
    rr,
    cil,
    ciu,
    exposure,
    outcome,
    lag,
    quantile,
    iqrm,
    bisection_method
  ) %>%
  mutate(
    I2 = round(I2, 3),
    Q = round(Q, 3),
    p.Qtest = round(p.Qtest, 3),
    rr = round(rr, 4),
    cil = round(cil, 4),
    ciu = round(ciu, 4),
    iqrm = round(iqrm, 2)
  )

write.csv(tableS5_1, file = file.path(root_dir, "tables/tableS5-1.csv"), row.names = FALSE)

tableS5 <- tableS5_1 %>%
  filter(
    outcome == "resp",
    bisection_method %in% c("perc75", "perc80", "perc85")
  ) %>%
  select(I2, p.Qtest, outcome, lag, quantile, bisection_method)

write.csv(tableS5, file = file.path(root_dir, "tables/tableS5.csv"), row.names = FALSE)

# table S6 --------------------------------------------------------------------------------------------------------

# pooled city-specific coefficients of the interaction term between daily SPM concentration and pollen levels

tmp <- do.call(rbind, c(data_1, data_2))

# filter to spm / all | circ | resp / lag 0, 1, 2 and select the desired columns
tmp <- tmp %>%
  filter(
    exposure == "spm",
    outcome %in% c("all", "circ", "resp"),
    lag %in% c("0", "1", "2")
  ) %>%
  select(
    I2,
    Q,
    p.Qtest,
    rr,
    cil,
    ciu,
    exposure,
    outcome,
    lag,
    quantile,
    iqrm,
    bisection_method
  ) %>%
  mutate(
    I2 = round(I2, 3),
    Q = round(Q, 3),
    p.Qtest = round(p.Qtest, 3),
    rr = round(rr, 4),
    cil = round(cil, 4),
    ciu = round(ciu, 4),
    iqrm = round(iqrm, 2)
  )

tableS6_A <- tmp %>%
  filter(outcome == "all") %>%
  select(rr, cil, ciu, outcome, lag, quantile, bisection_method)

tableS6_B <- tmp %>%
  filter(outcome == "circ") %>%
  select(rr, cil, ciu, outcome, lag, quantile, bisection_method)

tableS6_C <- tmp %>%
  filter(outcome == "resp") %>%
  select(rr, cil, ciu, outcome, lag, quantile, bisection_method)

write.csv(tableS6_A, file = file.path(root_dir, "tables/tableS6-A.csv"), row.names = FALSE)
write.csv(tableS6_B, file = file.path(root_dir, "tables/tableS6-B.csv"), row.names = FALSE)
write.csv(tableS6_C, file = file.path(root_dir, "tables/tableS6-C.csv"), row.names = FALSE)

# figure S1 -------------------------------------------------------------------------------------------------------

# Skip -- Map of Japan

# figure S2 -------------------------------------------------------------------------------------------------------

# params
tmp <- data_0[data_0$exposure %in% c("spm") &
                data_0$outcome %in% c("all65", "circ65", "resp65") &
                data_0$lag %in% c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"), ]
y_lower_bound <- min(tmp$cil, na.rm = TRUE)
y_upper_bound <- max(tmp$ciu, na.rm = TRUE)
point_color = "red"
point_shape = 18

# plots
a <- {
  create_noninteractive_plot(
    exposure = "spm",
    outcome = "all65",
    lags = c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data = data_0,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    point_color = point_color,
    point_shape = point_shape
  )
}
b <- {
  create_noninteractive_plot(
    exposure = "spm",
    outcome = "resp65",
    lags = c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data = data_0,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    point_color = point_color,
    point_shape = point_shape
  )
}
c <- {
  create_noninteractive_plot(
    exposure = "spm",
    outcome = "circ65",
    lags = c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data = data_0,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    point_color = point_color,
    point_shape = point_shape
  )
}
blank_plot <- ggplot() + theme_void()
top_row <- plot_grid(
  blank_plot,
  a + ggtitle("[A] All-cause Ages >=65") + theme(plot.title = element_text(hjust = 0.5, face = "bold")),
  blank_plot,
  labels = NULL,
  ncol = 3,
  rel_widths = c(1, 2, 1),
  align = 'h',
  axis = 'tb'
)
bottom_row <- plot_grid(
  b + ggtitle("[B] Respiratory Ages >=65") + theme(plot.title = element_text(hjust = 0.5, face = "bold")),
  c + ggtitle("[C] Cardiovascular Ages >=65") + theme(plot.title = element_text(hjust = 0.5, face = "bold")),
  labels = NULL,
  ncol = 2,
  align = 'hv',
  axis = 'tblr'
)
plotS2 <- {
  plot_grid(
    top_row,
    bottom_row,
    ncol = 1,
    rel_heights = c(1, 1)
  )
}
{ 
  ggsave(
    filename = paste0(root_dir, "/plots/figureS2.pdf"),
    plot = plotS2,
    device = "pdf",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )
}

# figure S3 -------------------------------------------------------------------------------------------------------

# params
tmp <- do.call(rbind, c(data_1, data_2))
tmp <- tmp[tmp$exposure %in% c("spm") &
             tmp$outcome %in% c("all65", "circ65", "resp65") &
             tmp$lag %in% c("0", "1", "2"), ]
y_lower_bound <- min(tmp$cil, na.rm = TRUE)
y_upper_bound <- max(tmp$ciu, na.rm = TRUE)
cutoffs_vec_perc <- c(75, 80, 85)
cutoffs_vec_abs <- c(25, 50, 75)
debug <- FALSE

# plots
a <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all65",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
b <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all65",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
c <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp65",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
d <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp65",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
e <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ65",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
f <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ65",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
plotS3 <- {
  plot_grid(
    a,
    b,
    c,
    d,
    e,
    f,
    nrow = 3,
    ncol = 2,
    labels = c(
      "[A] All-cause Ages >=65",
      "[D] All-cause Ages >=65",
      "[B] Respiratory Ages >=65",
      "[E] Respiratory Ages >=65",
      "[C] Cardiovascular Ages >=65",
      "[F] Cardiovascular Ages >=65"
    ),
    label_x = 0.05,
    hjust = 0
  )
}
{ 
  ggsave(
    filename = paste0(root_dir, "/plots/figureS3.pdf"),
    plot = plotS3,
    device = "pdf",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )
}

# figure S4 -------------------------------------------------------------------------------------------------------

tmp <- do.call(rbind, c(data_3, data_4))
tmp <- tmp[tmp$exposure %in% c("spm") &
             tmp$outcome %in% c("all", "circ", "resp") &
             tmp$lag %in% c("0", "1", "2"), ]
y_lower_bound <- min(tmp$cil, na.rm = TRUE)
y_upper_bound <- max(tmp$ciu, na.rm = TRUE) * 1.005
cutoffs_vec_perc <- c(75, 80, 85)
cutoffs_vec_abs <- c(25, 50, 75)
debug <- FALSE

# plots
a <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c(0, 1, 2),
    data = data_3,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
b <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c(0, 1, 2),
    data = data_4,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
c <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp",
    lags = c(0, 1, 2),
    data = data_3,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
d <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp",
    lags = c(0, 1, 2),
    data = data_4,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
e <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ",
    lags = c(0, 1, 2),
    data = data_3,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
f <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ",
    lags = c(0, 1, 2),
    data = data_4,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
plotS4 <- {
  plot_grid(
    a,
    b,
    c,
    d,
    e,
    f,
    nrow = 3,
    ncol = 2,
    labels = c(
      "[A] All-cause (NO2 Confounding)",
      "[D] All-cause (NO2 Confounding)",
      "[B] Respiratory (NO2 Confounding)",
      "[E] Respiratory (NO2 Confounding)",
      "[C] Cardiovascular (NO2 Confounding)",
      "[F] Cardiovascular (NO2 Confounding)"
    ),
    label_x = 0.05,
    hjust = 0
  )
}
{ 
  ggsave(
    filename = paste0(root_dir, "/plots/figureS4.pdf"),
    plot = plotS4,
    device = "pdf",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )
}

# figure S5 -------------------------------------------------------------------------------------------------------

# params
tmp <- do.call(rbind, c(data_5, data_6))
tmp <- tmp[tmp$exposure %in% c("spm") &
             tmp$outcome %in% c("all", "circ", "resp") &
             tmp$lag %in% c("0", "1", "2"), ]
y_lower_bound <- min(tmp$cil, na.rm = TRUE)
y_upper_bound <- max(tmp$ciu, na.rm = TRUE)
cutoffs_vec_perc <- c(75, 80, 85)
cutoffs_vec_abs <- c(25, 50, 75)
debug <- FALSE

# plots
a <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c(0, 1, 2),
    data = data_5,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
b <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c(0, 1, 2),
    data = data_6,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
c <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp",
    lags = c(0, 1, 2),
    data = data_5,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
d <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp",
    lags = c(0, 1, 2),
    data = data_6,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
e <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ",
    lags = c(0, 1, 2),
    data = data_5,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
f <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ",
    lags = c(0, 1, 2),
    data = data_6,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
plotS5 <- {
  plot_grid(
    a,
    b,
    c,
    d,
    e,
    f,
    nrow = 3,
    ncol = 2,
    labels = c(
      "[A] All-cause (SO2 Confounding)",
      "[D] All-cause (SO2 Confounding)",
      "[B] Respiratory (SO2 Confounding)",
      "[E] Respiratory (SO2 Confounding)",
      "[C] Cardiovascular (SO2 Confounding)",
      "[F] Cardiovascular (SO2 Confounding)"
    ),
    label_x = 0.05,
    hjust = 0
  )
}
{ 
  ggsave(
    filename = paste0(root_dir, "/plots/figureS5.pdf"),
    plot = plotS5,
    device = "pdf",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )
}

# figure S6 -------------------------------------------------------------------------------------------------------

# params
seasonal_df_range <- 2:7
data_seasonal <- data.frame()
for (i in seasonal_df_range) {
  tmp <- data_7[data_7$seasonal_df == i, ]
  meta_tmp <- run_meta_analysis(tmp)
  data_seasonal <- rbind(
    data_seasonal,
    data.frame(
      seasonal_df = mean(tmp$seasonal_df),
      qaic = sum(tmp$qaic),
      B = meta_tmp$B,
      se = meta_tmp$se,
      cil = meta_tmp$cil,
      ciu = meta_tmp$ciu,
      rr = meta_tmp$rr,
      iqrm = meta_tmp$iqrm
    )
  )
}

temperature_df_range <- 2:7
data_temperature <- data.frame()
for (i in temperature_df_range) {
  tmp <- data_7[data_7$temperature_df == i, ]
  meta_tmp <- run_meta_analysis(tmp)
  data_temperature <- rbind(
    data_temperature,
    data.frame(
      temperature_df = mean(tmp$temperature_df),
      qaic = sum(tmp$qaic),
      B = meta_tmp$B,
      se = meta_tmp$se,
      cil = meta_tmp$cil,
      ciu = meta_tmp$ciu,
      rr = meta_tmp$rr,
      iqrm = meta_tmp$iqrm
    )
  )
}

y_lower_bound <- min(data_seasonal$cil * 0.9995, na.rm = TRUE)
y_upper_bound <- max(data_seasonal$ciu * 1.0005, na.rm = TRUE)

# Plot 1: seasonal_df vs qaic
a <- ggplot(data_seasonal, aes(x = seasonal_df, y = qaic)) +
  geom_point(size = 2.5, color = "blue") +
  labs(x = "Degrees of Freedom", y = "qAIC", title = " ") +
  theme_classic()

# Plot 2: seasonal_df vs rr with error bars (cil and ciu)
b <- ggplot(data_seasonal, aes(x = seasonal_df, y = rr)) +
  geom_point(size = 2.5, color = "red") +
  scale_y_continuous(limits = c(y_lower_bound, y_upper_bound)) +
  geom_errorbar(aes(ymin = cil, ymax = ciu), width = 0.2) +
  labs(x = "Degrees of Freedom", y = "Relative Risk", title = " ") +
  theme_classic()

# Plot 3: temperature_df vs qaic
c <- ggplot(data_temperature, aes(x = temperature_df, y = qaic)) +
  geom_point(size = 2.5, color = "blue") +
  labs(x = "Degrees of Freedom", y = "qAIC", title = " ") +
  theme_classic()

# Plot 4: temperature_df vs rr with error bars (cil and ciu)
d <- ggplot(data_temperature, aes(x = temperature_df, y = rr)) +
  geom_point(size = 2.5, color = "red") +
  scale_y_continuous(limits = c(y_lower_bound, y_upper_bound)) +
  geom_errorbar(aes(ymin = cil, ymax = ciu), width = 0.2) +
  labs(x = "Degrees of Freedom", y = "Relative Risk", title = " ") +
  theme_classic()

plotS6 <- plot_grid(
  a,
  b,
  c,
  d,
  ncol = 2,
  labels = c(
    "[A] qAIC for varying Seasonal DF",
    "[B] RR for varying Seasonal DF",
    "[C] qAIC for varying Temperature DF",
    "[D] RR for varying Temperature DF"
  ),
  label_x = 0.05,
  hjust = 0
)
{ 
  ggsave(
    filename = paste0(root_dir, "/plots/figureS6.pdf"),
    plot = plotS6,
    device = "pdf",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )
}
