
# main ----------------------------------------------------------------------------------------

# initialize project state
rm(list = ls())
root_dir <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality"
source(paste0(root_dir, "/src/functions.R")) # import functions

# import data
lagdata <- read.csv(file = paste0(root_dir, "/data/lagdata.csv"))

# data transformations
lagdata[, "SuHi0"] <- lagdata[, "SuHi"]
lagdata[, "date"] <- as.Date(lagdata[, "date"])

# constants
SUHI <- "SuHiout" # "SuHiout", "SuHi" <- normally this one
CITIES <- c(
  "Fukuoka", "Kumamoto", "Nagasaki", "Oita", "Saga", "Kagoshima", "Miyazaki", "Kitakyushu"
)
POL_COLUMNS_TO_PROCESS <- c(
  "pol_0", "pol_1", "pol_2", "pol_3", "pol_4", "pol_5", "pol_6", "pol_7",
  "pol_ma1", "pol_ma2", "pol_ma3", "pol_ma4", "pol_ma5", "pol_ma6", "pol_ma7"
)

# rewrite lagdata
columns_to_keep <- c(
  "city",
  "All",
  "Cardiovascular",
  "Respiratory",
  "All..Above.65.",
  "Cardiovascular..Above.65.",
  "Respiratory..Above.65.",
  "ad",
  "Holiday",
  "dow",
  "doy",
  "year",
  "Tave07",
  "date",
  "RHave",
  "month",
  
  "SPMoutma0.1",
  "SPMoutma0.2",
  "SPMoutma0.3",
  "SPMoutma0.4",
  "SPMoutma0.5",
  "SPMoutma0.6",
  "SPMoutma0.7",
  "SPMout",
  "SPMout1",
  "SPMout2",
  "SPMout3",
  "SPMout4",
  "SPMout5",
  "SPMout6",
  "SPMout7",
  
  "NO2outma0.1",
  "NO2outma0.2",
  "NO2outma0.3",
  "NO2outma0.4",
  "NO2outma0.5",
  "NO2outma0.6",
  "NO2outma0.7",
  "NO2out",
  "NO2out1",
  "NO2out2",
  "NO2out3",
  "NO2out4",
  "NO2out5",
  "NO2out6",
  "NO2out7",
  
  "SO2outma0.1",
  "SO2outma0.2",
  "SO2outma0.3",
  "SO2outma0.4",
  "SO2outma0.5",
  "SO2outma0.6",
  "SO2outma0.7",
  "SO2out",
  "SO2out1",
  "SO2out2",
  "SO2out3",
  "SO2out4",
  "SO2out5",
  "SO2out6",
  "SO2out7",
  
  paste0(SUHI, "ma0.1"),
  paste0(SUHI, "ma0.2"),
  paste0(SUHI, "ma0.3"),
  paste0(SUHI, "ma0.4"),
  paste0(SUHI, "ma0.5"),
  paste0(SUHI, "ma0.6"),
  paste0(SUHI, "ma0.7"),
  paste0(SUHI, ""),
  paste0(SUHI, "1"),
  paste0(SUHI, "2"),
  paste0(SUHI, "3"),
  paste0(SUHI, "4"),
  paste0(SUHI, "5")
  paste0(SUHI, "6"),
  paste0(SUHI, "7")
)

columns_to_rename <- c(
  city = "city",
  all = "All",
  circ = "Cardiovascular",
  resp = "Respiratory",
  all65 = "All..Above.65.",
  circ65 = "Cardiovascular..Above.65.",
  resp65 = "Respiratory..Above.65.",
  ad = "ad",
  holiday = "Holiday",
  dow = "dow",
  doy = "doy",
  year = "year",
  tave07 = "Tave07",
  date = "date",
  rhave = "RHave",
  month = "month",
  
  spm_ma1 = "SPMoutma0.1",
  spm_ma2 = "SPMoutma0.2",
  spm_ma3 = "SPMoutma0.3",
  spm_ma4 = "SPMoutma0.4",
  spm_ma5 = "SPMoutma0.5",
  spm_ma6 = "SPMoutma0.6",
  spm_ma7 = "SPMoutma0.7",
  spm_0 = "SPMout",
  spm_1 = "SPMout1",
  spm_2 = "SPMout2",
  spm_3 = "SPMout3",
  spm_4 = "SPMout4",
  spm_5 = "SPMout5",
  spm_6 = "SPMout6",
  spm_7 = "SPMout7",
  
  no2_ma1 = "NO2outma0.1",
  no2_ma2 = "NO2outma0.2",
  no2_ma3 = "NO2outma0.3",
  no2_ma4 = "NO2outma0.4",
  no2_ma5 = "NO2outma0.5",
  no2_ma6 = "NO2outma0.6",
  no2_ma7 = "NO2outma0.7",
  no2_0 = "NO2out",
  no2_1 = "NO2out1",
  no2_2 = "NO2out2",
  no2_3 = "NO2out3",
  no2_4 = "NO2out4",
  no2_5 = "NO2out5",
  no2_6 = "NO2out6",
  no2_7 = "NO2out7",
  
  so2_ma1 = "SO2outma0.1",
  so2_ma2 = "SO2outma0.2",
  so2_ma3 = "SO2outma0.3",
  so2_ma4 = "SO2outma0.4",
  so2_ma5 = "SO2outma0.5",
  so2_ma6 = "SO2outma0.6",
  so2_ma7 = "SO2outma0.7",
  so2_0 = "SO2out",
  so2_1 = "SO2out1",
  so2_2 = "SO2out2",
  so2_3 = "SO2out3",
  so2_4 = "SO2out4",
  so2_5 = "SO2out5",
  so2_6 = "SO2out6",
  so2_7 = "SO2out7",
  
  pol_ma1 = paste0(SUHI, "ma0.1"),
  pol_ma2 = paste0(SUHI, "ma0.2"),
  pol_ma3 = paste0(SUHI, "ma0.3"),
  pol_ma4 = paste0(SUHI, "ma0.4"),
  pol_ma5 = paste0(SUHI, "ma0.5"),
  pol_ma6 = paste0(SUHI, "ma0.6"),
  pol_ma7 = paste0(SUHI, "ma0.7"),
  pol_0 = paste0(SUHI, ""),
  pol_1 = paste0(SUHI, "1"),
  pol_2 = paste0(SUHI, "2"),
  pol_3 = paste0(SUHI, "3"),
  pol_4 = paste0(SUHI, "4"),
  pol_5 = paste0(SUHI, "5")
  pol_6 = paste0(SUHI, "6"),
  pol_7 = paste0(SUHI, "7")
)
data <- lagdata %>%
  dplyr::select(all_of(columns_to_keep)) %>%
  dplyr::rename(!!!columns_to_rename)

# create different quantile data ----------------------------------------------------------------------------------

# bool_perc_cutoff = TRUE, bisection_cutoff = 75%
{
  data_perc75 <- data

  # create quantiles
  for (col in POL_COLUMNS_TO_PROCESS) {
    data_perc75 <- add_quantile_column(data_perc75, CITIES, col, TRUE, 0.75)
  }

  # subset feb to apr
  data_perc75 <- data_perc75 %>%
    filter(month %in% c(2, 3, 4))

  # export data
  write.csv(data_perc75, file = paste0(root_dir, "/data/data_perc75.csv"), row.names = FALSE)
  rm(data_perc75)
}

# bool_perc_cutoff = TRUE, bisection_cutoff = 80%
{
  data_perc80 <- data

  # create quantiles
  for (col in POL_COLUMNS_TO_PROCESS) {
    data_perc80 <- add_quantile_column(data_perc80, CITIES, col, TRUE, 0.80)
  }

  # subset feb to apr
  data_perc80 <- data_perc80 %>%
    filter(month %in% c(2, 3, 4))

  # export data
  write.csv(data_perc80, file = paste0(root_dir, "/data/data_perc80.csv"), row.names = FALSE)
  rm(data_perc80)
}

# bool_perc_cutoff = TRUE, bisection_cutoff = 85%
{
  data_perc85 <- data

  # create quantiles
  for (col in POL_COLUMNS_TO_PROCESS) {
    data_perc85 <- add_quantile_column(data_perc85, CITIES, col, TRUE, 0.85)
  }

  # subset feb to apr
  data_perc85 <- data_perc85 %>%
    filter(month %in% c(2, 3, 4))

  # export data
  write.csv(data_perc85, file = paste0(root_dir, "/data/data_perc85.csv"), row.names = FALSE)
  rm(data_perc85)
}

# bool_perc_cutoff = FALSE, bisection_cutoff = 25 pollen count
{
  data_abs25 <- data

  # create quantiles
  for (col in POL_COLUMNS_TO_PROCESS) {
    data_abs25 <- add_quantile_column(data_abs25, CITIES, col, FALSE, 25)
  }

  # subset feb to apr
  data_abs25 <- data_abs25 %>%
    filter(month %in% c(2, 3, 4))

  # export data
  write.csv(data_abs25, file = paste0(root_dir, "/data/data_abs25.csv"), row.names = FALSE)
  rm(data_abs25)
}

# bool_perc_cutoff = FALSE, bisection_cutoff = 50 pollen count
{
  data_abs50 <- data

  # create quantiles
  for (col in POL_COLUMNS_TO_PROCESS) {
    data_abs50 <- add_quantile_column(data_abs50, CITIES, col, FALSE, 50)
  }

  # subset feb to apr
  data_abs50 <- data_abs50 %>%
    filter(month %in% c(2, 3, 4))

  # export data
  write.csv(data_abs50, file = paste0(root_dir, "/data/data_abs50.csv"), row.names = FALSE)
  rm(data_abs50)
}

# bool_perc_cutoff = FALSE, bisection_cutoff = 75 pollen count
{
  data_abs75 <- data

  # create quantiles
  for (col in POL_COLUMNS_TO_PROCESS) {
    data_abs75 <- add_quantile_column(data_abs75, CITIES, col, FALSE, 75)
  }

  # subset feb to apr
  data_abs75 <- data_abs75 %>%
    filter(month %in% c(2, 3, 4))

  # export data
  write.csv(data_abs75, file = paste0(root_dir, "/data/data_abs75.csv"), row.names = FALSE)
  rm(data_abs75)
}

