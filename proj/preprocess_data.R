
library(dplyr)

# functions -----------------------------------------------------------------------------------

rm(list=ls())

# assign quartiles, (absolute) terciles, bisection (high/low @ 50%)
add_quantile_column <- function(data, CITIES, column_name) {
  results <- list()
  for (city in CITIES) {
    if (column_name %in% names(data)) {
      intermediary_data <- data[data["city"] == city, ]
  
      # determine breaks for each category
      quartile_breaks <- quantile(intermediary_data[[column_name]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
      tercile_breaks <- c(-Inf, 10, 30, Inf)
      bisection_breaks <- quantile(intermediary_data[[column_name]], probs = c(0, 0.75, 1), na.rm = TRUE)
      # bisection_breaks <- c(-Inf, 80, Inf)
  
      # Add categorized columns to data
      intermediary_data <- intermediary_data %>%
        mutate(
          !!paste0(column_name, "_quartile") := factor(cut(!!sym(column_name), breaks = quartile_breaks, include.lowest = TRUE, labels = FALSE)),
          !!paste0(column_name, "_tercile") := factor(cut(!!sym(column_name), breaks = tercile_breaks, include.lowest = TRUE, labels = FALSE)),
          !!paste0(column_name, "_bisection") := factor(cut(!!sym(column_name), breaks = bisection_breaks, include.lowest = TRUE, labels = FALSE))
        )
    } else {
      stop(paste("Column", column_name, "does not exist in the dataframe."))
    }
    results[[city]] <- intermediary_data
  }
  data <- dplyr::bind_rows(results)
  return(data)
}

calculate_exposure_percent_cutoffs <- function(data, column, CITIES, quantiles) {
  quantiles <- c(0.75, 0.80, 0.85, 0.90)
  CITIES <- c("Fukuoka", "Kumamoto", "Nagasaki", "Oita", "Saga", "Kagoshima", "Miyazaki", "Kitakyushu")
  data <- data[, c("date", "city", "SuHi")]
  results <- list()
  for (city in CITIES) {
    city_data <- data %>% filter(city == !!city)
    city_quantiles <- quantile(city_data$SuHi, probs = quantiles, na.rm=TRUE)
    results[[city]] <- round(city_quantiles, 2)
  }
  results <- do.call(cbind, results)
  rownames(results) <- paste0(quantiles * 100, "%")
  return(results)
}

calculate_exposure_absolute_cutoffs <- function(data, column, CITIES, quantiles) {
  quantiles <- c(20, 40, 60, 80)
  CITIES <- c("Fukuoka", "Kumamoto", "Nagasaki", "Oita", "Saga", "Kagoshima", "Miyazaki", "Kitakyushu")
  data <- data[, c("date", "city", "SuHi")]
  data$SuHi <- as.numeric(as.character(data$SuHi))
  data <- data %>% filter(!is.na(SuHi))
  results <- list()
  for (city in CITIES) {
    city_data <- data %>% filter(city == !!city)
    
    city_counts <- sapply(values, function(x) {
      below <- sum(city_data$SuHi < x, na.rm = TRUE)
      above <- sum(city_data$SuHi >= x, na.rm = TRUE)
      c(below, above)
    })
    
    results[[city]] <- city_counts
  }
  result_df <- do.call(cbind, results)
  rownames(result_df) <- values
  return(results)
}

# main ----------------------------------------------------------------------------------------

# initialize project state
root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
source(paste0(root_dir, "/proj/functions.R"))  # import functions

fdata <- read.csv(file=paste0(root_dir, "/data/fdata.csv"))
lagdata <- read.csv(file=paste0(root_dir, "/data/lagdata.csv"))
subdata <- read.csv(file=paste0(root_dir, "/data/subdata.csv"))

# data transformations
fdata[, "SuHi0"] <- fdata[, "SuHi"]
fdata[, "date"] <- as.Date(fdata[, "date"])
lagdata[, "SuHi0"] <- lagdata[, "SuHi"]
lagdata[, "date"] <- as.Date(lagdata[, "date"])
subdata[, "SuHi0"] <- subdata[, "SuHi"]
subdata[, "date"] <- as.Date(subdata[, "date"])

# rewrite subdata -----------------------------------------------------------------------------

columns_to_keep <- c(
  "city",
  "all",
  "circ",
  "resp",
  "all.age65",
  "circ.age65",
  "resp.age65",
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
  "SPMout0",
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
  "NO2out0",
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
  "SO2out0",
  "SO2out1",
  "SO2out2",
  "SO2out3",
  "SO2out4",
  "SO2out5",
  "SO2out6",
  "SO2out7",
  "SuHioutma0.1",
  "SuHioutma0.2",
  "SuHioutma0.3",
  "SuHioutma0.4",
  "SuHioutma0.5",
  "SuHioutma0.6",
  "SuHioutma0.7",
  "SuHiout0",
  "SuHiout1",
  "SuHiout2",
  "SuHiout3",
  "SuHiout4",
  "SuHiout5",
  "SuHiout6",
  "SuHiout7"
)

# define the columns you want to keep and rename
columns_to_rename <- list(
  city = "city",
  all = "all",
  circ = "circ",
  resp = "resp",
  all65 = "all.age65",
  circ65 = "circ.age65",
  resp65 = "resp.age65",
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
  spm_0 = "SPMout0",
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
  no2_0 = "NO2out0",
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
  so2_0 = "SO2out0",
  so2_1 = "SO2out1",
  so2_2 = "SO2out2",
  so2_3 = "SO2out3",
  so2_4 = "SO2out4",
  so2_5 = "SO2out5",
  so2_6 = "SO2out6",
  so2_7 = "SO2out7",
  pol_ma1 = "SuHioutma0.1",
  pol_ma2 = "SuHioutma0.2",
  pol_ma3 = "SuHioutma0.3",
  pol_ma4 = "SuHioutma0.4",
  pol_ma5 = "SuHioutma0.5",
  pol_ma6 = "SuHioutma0.6",
  pol_ma7 = "SuHioutma0.7",
  pol_0 = "SuHiout0",
  pol_1 = "SuHiout1",
  pol_2 = "SuHiout2",
  pol_3 = "SuHiout3",
  pol_4 = "SuHiout4",
  pol_5 = "SuHiout5",
  pol_6 = "SuHiout6",
  pol_7 = "SuHiout7"
)

# Applying the renaming
data <- lagdata %>%
  dplyr::select(all_of(columns_to_keep)) %>%
  dplyr::rename(!!!columns_to_rename)

# create quantiles ----------------------------------------------------------------------------

CITIES <- c(
  "Fukuoka", "Kumamoto", "Nagasaki", "Oita", "Saga", "Kagoshima", "Miyazaki", "Kitakyushu"
)

data <- add_quantile_column(data, CITIES, "pol_0")
data <- add_quantile_column(data, CITIES, "pol_1")
data <- add_quantile_column(data, CITIES, "pol_2")
data <- add_quantile_column(data, CITIES, "pol_3")
data <- add_quantile_column(data, CITIES, "pol_4")
data <- add_quantile_column(data, CITIES, "pol_5")
data <- add_quantile_column(data, CITIES, "pol_6")
data <- add_quantile_column(data, CITIES, "pol_7")
data <- add_quantile_column(data, CITIES, "pol_ma1")
data <- add_quantile_column(data, CITIES, "pol_ma2")
data <- add_quantile_column(data, CITIES, "pol_ma3")
data <- add_quantile_column(data, CITIES, "pol_ma4")
data <- add_quantile_column(data, CITIES, "pol_ma5")
data <- add_quantile_column(data, CITIES, "pol_ma6")
data <- add_quantile_column(data, CITIES, "pol_ma7")

# take only months 2, 3, 4 --------------------------------------------------------------------

data <- data %>%
  filter(month %in% c(2, 3, 4))

# export --------------------------------------------------------------------------------------

write.csv(data, file=paste0(root_dir, "/data/data.csv"), row.names=FALSE)

