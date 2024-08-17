
# main ----------------------------------------------------------------------------------------

# initialize project state
rm(list = ls())
root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
source(paste0(root_dir, "/proj/functions.R")) # import functions

fdata <- read.csv(file = paste0(root_dir, "/data/fdata.csv"))
lagdata <- read.csv(file = paste0(root_dir, "/data/lagdata.csv"))
subdata <- read.csv(file = paste0(root_dir, "/data/subdata.csv"))

# data transformations
fdata[, "SuHi0"] <- fdata[, "SuHi"]
fdata[, "date"] <- as.Date(fdata[, "date"])
lagdata[, "SuHi0"] <- lagdata[, "SuHi"]
lagdata[, "date"] <- as.Date(lagdata[, "date"])
subdata[, "SuHi0"] <- subdata[, "SuHi"]
subdata[, "date"] <- as.Date(subdata[, "date"])

# constant
CITIES <- c(
  "Fukuoka", "Kumamoto", "Nagasaki", "Oita", "Saga", "Kagoshima", "Miyazaki", "Kitakyushu"
)

# rewrite subdata
columns_to_keep <- {
  c(
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
}
columns_to_rename <- {
  list(
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
}
data <- lagdata %>%
  dplyr::select(all_of(columns_to_keep)) %>%
  dplyr::rename(!!!columns_to_rename)

# create different quantile data ------------------------------------------

# bool_perc_cutoff = TRUE, bisection_cutoff = 75%
{
  data_perc75 <- data

  # create quantiles
  data_perc75 <- add_quantile_column(data_perc75, CITIES, "pol_0", TRUE, 0.75)
  data_perc75 <- add_quantile_column(data_perc75, CITIES, "pol_1", TRUE, 0.75)
  data_perc75 <- add_quantile_column(data_perc75, CITIES, "pol_2", TRUE, 0.75)
  data_perc75 <- add_quantile_column(data_perc75, CITIES, "pol_3", TRUE, 0.75)
  data_perc75 <- add_quantile_column(data_perc75, CITIES, "pol_4", TRUE, 0.75)
  data_perc75 <- add_quantile_column(data_perc75, CITIES, "pol_5", TRUE, 0.75)
  data_perc75 <- add_quantile_column(data_perc75, CITIES, "pol_6", TRUE, 0.75)
  data_perc75 <- add_quantile_column(data_perc75, CITIES, "pol_7", TRUE, 0.75)
  data_perc75 <- add_quantile_column(data_perc75, CITIES, "pol_ma1", TRUE, 0.75)
  data_perc75 <- add_quantile_column(data_perc75, CITIES, "pol_ma2", TRUE, 0.75)
  data_perc75 <- add_quantile_column(data_perc75, CITIES, "pol_ma3", TRUE, 0.75)
  data_perc75 <- add_quantile_column(data_perc75, CITIES, "pol_ma4", TRUE, 0.75)
  data_perc75 <- add_quantile_column(data_perc75, CITIES, "pol_ma5", TRUE, 0.75)
  data_perc75 <- add_quantile_column(data_perc75, CITIES, "pol_ma6", TRUE, 0.75)
  data_perc75 <- add_quantile_column(data_perc75, CITIES, "pol_ma7", TRUE, 0.75)
  
  # subset feb to apr
  data_perc75 <- data_perc75 %>%
    filter(month %in% c(2, 3, 4))
  
  # export data
  write.csv(data_perc75, file = paste0(root_dir, "/data/data_perc75.csv"), row.names = FALSE)
}

# bool_perc_cutoff = TRUE, bisection_cutoff = 80%
{
  data_perc80 <- data
  
  # create quantiles
  data_perc80 <- add_quantile_column(data_perc80, CITIES, "pol_0", TRUE, 0.80)
  data_perc80 <- add_quantile_column(data_perc80, CITIES, "pol_1", TRUE, 0.80)
  data_perc80 <- add_quantile_column(data_perc80, CITIES, "pol_2", TRUE, 0.80)
  data_perc80 <- add_quantile_column(data_perc80, CITIES, "pol_3", TRUE, 0.80)
  data_perc80 <- add_quantile_column(data_perc80, CITIES, "pol_4", TRUE, 0.80)
  data_perc80 <- add_quantile_column(data_perc80, CITIES, "pol_5", TRUE, 0.80)
  data_perc80 <- add_quantile_column(data_perc80, CITIES, "pol_6", TRUE, 0.80)
  data_perc80 <- add_quantile_column(data_perc80, CITIES, "pol_7", TRUE, 0.80)
  data_perc80 <- add_quantile_column(data_perc80, CITIES, "pol_ma1", TRUE, 0.80)
  data_perc80 <- add_quantile_column(data_perc80, CITIES, "pol_ma2", TRUE, 0.80)
  data_perc80 <- add_quantile_column(data_perc80, CITIES, "pol_ma3", TRUE, 0.80)
  data_perc80 <- add_quantile_column(data_perc80, CITIES, "pol_ma4", TRUE, 0.80)
  data_perc80 <- add_quantile_column(data_perc80, CITIES, "pol_ma5", TRUE, 0.80)
  data_perc80 <- add_quantile_column(data_perc80, CITIES, "pol_ma6", TRUE, 0.80)
  data_perc80 <- add_quantile_column(data_perc80, CITIES, "pol_ma7", TRUE, 0.80)
  
  # subset feb to apr
  data_perc80 <- data_perc80 %>%
    filter(month %in% c(2, 3, 4))
  
  # export data
  write.csv(data_perc80, file = paste0(root_dir, "/data/data_perc80.csv"), row.names = FALSE)
}

# bool_perc_cutoff = TRUE, bisection_cutoff = 85%
{
  data_perc85 <- data
  
  # create quantiles
  data_perc85 <- add_quantile_column(data_perc85, CITIES, "pol_0", TRUE, 0.85)
  data_perc85 <- add_quantile_column(data_perc85, CITIES, "pol_1", TRUE, 0.85)
  data_perc85 <- add_quantile_column(data_perc85, CITIES, "pol_2", TRUE, 0.85)
  data_perc85 <- add_quantile_column(data_perc85, CITIES, "pol_3", TRUE, 0.85)
  data_perc85 <- add_quantile_column(data_perc85, CITIES, "pol_4", TRUE, 0.85)
  data_perc85 <- add_quantile_column(data_perc85, CITIES, "pol_5", TRUE, 0.85)
  data_perc85 <- add_quantile_column(data_perc85, CITIES, "pol_6", TRUE, 0.85)
  data_perc85 <- add_quantile_column(data_perc85, CITIES, "pol_7", TRUE, 0.85)
  data_perc85 <- add_quantile_column(data_perc85, CITIES, "pol_ma1", TRUE, 0.85)
  data_perc85 <- add_quantile_column(data_perc85, CITIES, "pol_ma2", TRUE, 0.85)
  data_perc85 <- add_quantile_column(data_perc85, CITIES, "pol_ma3", TRUE, 0.85)
  data_perc85 <- add_quantile_column(data_perc85, CITIES, "pol_ma4", TRUE, 0.85)
  data_perc85 <- add_quantile_column(data_perc85, CITIES, "pol_ma5", TRUE, 0.85)
  data_perc85 <- add_quantile_column(data_perc85, CITIES, "pol_ma6", TRUE, 0.85)
  data_perc85 <- add_quantile_column(data_perc85, CITIES, "pol_ma7", TRUE, 0.85)
  
  # subset feb to apr
  data_perc85 <- data_perc85 %>%
    filter(month %in% c(2, 3, 4))
  
  # export data
  write.csv(data_perc85, file = paste0(root_dir, "/data/data_perc85.csv"), row.names = FALSE)
}

# bool_perc_cutoff = TRUE, bisection_cutoff = 90%
{
  data_perc90 <- data
  
  # create quantiles
  data_perc90 <- add_quantile_column(data_perc90, CITIES, "pol_0", TRUE, 0.90)
  data_perc90 <- add_quantile_column(data_perc90, CITIES, "pol_1", TRUE, 0.90)
  data_perc90 <- add_quantile_column(data_perc90, CITIES, "pol_2", TRUE, 0.90)
  data_perc90 <- add_quantile_column(data_perc90, CITIES, "pol_3", TRUE, 0.90)
  data_perc90 <- add_quantile_column(data_perc90, CITIES, "pol_4", TRUE, 0.90)
  data_perc90 <- add_quantile_column(data_perc90, CITIES, "pol_5", TRUE, 0.90)
  data_perc90 <- add_quantile_column(data_perc90, CITIES, "pol_6", TRUE, 0.90)
  data_perc90 <- add_quantile_column(data_perc90, CITIES, "pol_7", TRUE, 0.90)
  data_perc90 <- add_quantile_column(data_perc90, CITIES, "pol_ma1", TRUE, 0.90)
  data_perc90 <- add_quantile_column(data_perc90, CITIES, "pol_ma2", TRUE, 0.90)
  data_perc90 <- add_quantile_column(data_perc90, CITIES, "pol_ma3", TRUE, 0.90)
  data_perc90 <- add_quantile_column(data_perc90, CITIES, "pol_ma4", TRUE, 0.90)
  data_perc90 <- add_quantile_column(data_perc90, CITIES, "pol_ma5", TRUE, 0.90)
  data_perc90 <- add_quantile_column(data_perc90, CITIES, "pol_ma6", TRUE, 0.90)
  data_perc90 <- add_quantile_column(data_perc90, CITIES, "pol_ma7", TRUE, 0.90)
  
  # subset feb to apr
  data_perc90 <- data_perc90 %>%
    filter(month %in% c(2, 3, 4))
  
  # export data
  write.csv(data_perc90, file = paste0(root_dir, "/data/data_perc90.csv"), row.names = FALSE)
}

# bool_perc_cutoff = FALSE, bisection_cutoff = 20
{
  data_abs20 <- data
  
  # create quantiles
  data_abs20 <- add_quantile_column(data_abs20, CITIES, "pol_0", FALSE, 20)
  data_abs20 <- add_quantile_column(data_abs20, CITIES, "pol_1", FALSE, 20)
  data_abs20 <- add_quantile_column(data_abs20, CITIES, "pol_2", FALSE, 20)
  data_abs20 <- add_quantile_column(data_abs20, CITIES, "pol_3", FALSE, 20)
  data_abs20 <- add_quantile_column(data_abs20, CITIES, "pol_4", FALSE, 20)
  data_abs20 <- add_quantile_column(data_abs20, CITIES, "pol_5", FALSE, 20)
  data_abs20 <- add_quantile_column(data_abs20, CITIES, "pol_6", FALSE, 20)
  data_abs20 <- add_quantile_column(data_abs20, CITIES, "pol_7", FALSE, 20)
  data_abs20 <- add_quantile_column(data_abs20, CITIES, "pol_ma1", FALSE, 20)
  data_abs20 <- add_quantile_column(data_abs20, CITIES, "pol_ma2", FALSE, 20)
  data_abs20 <- add_quantile_column(data_abs20, CITIES, "pol_ma3", FALSE, 20)
  data_abs20 <- add_quantile_column(data_abs20, CITIES, "pol_ma4", FALSE, 20)
  data_abs20 <- add_quantile_column(data_abs20, CITIES, "pol_ma5", FALSE, 20)
  data_abs20 <- add_quantile_column(data_abs20, CITIES, "pol_ma6", FALSE, 20)
  data_abs20 <- add_quantile_column(data_abs20, CITIES, "pol_ma7", FALSE, 20)
  
  # subset feb to apr
  data_abs20 <- data_abs20 %>%
    filter(month %in% c(2, 3, 4))
  
  # export data
  write.csv(data_abs20, file = paste0(root_dir, "/data/data_abs20.csv"), row.names = FALSE)
}

# bool_perc_cutoff = FALSE, bisection_cutoff = 40
{
  data_abs40 <- data
  
  # create quantiles
  data_abs40 <- add_quantile_column(data_abs40, CITIES, "pol_0", FALSE, 40)
  data_abs40 <- add_quantile_column(data_abs40, CITIES, "pol_1", FALSE, 40)
  data_abs40 <- add_quantile_column(data_abs40, CITIES, "pol_2", FALSE, 40)
  data_abs40 <- add_quantile_column(data_abs40, CITIES, "pol_3", FALSE, 40)
  data_abs40 <- add_quantile_column(data_abs40, CITIES, "pol_4", FALSE, 40)
  data_abs40 <- add_quantile_column(data_abs40, CITIES, "pol_5", FALSE, 40)
  data_abs40 <- add_quantile_column(data_abs40, CITIES, "pol_6", FALSE, 40)
  data_abs40 <- add_quantile_column(data_abs40, CITIES, "pol_7", FALSE, 40)
  data_abs40 <- add_quantile_column(data_abs40, CITIES, "pol_ma1", FALSE, 40)
  data_abs40 <- add_quantile_column(data_abs40, CITIES, "pol_ma2", FALSE, 40)
  data_abs40 <- add_quantile_column(data_abs40, CITIES, "pol_ma3", FALSE, 40)
  data_abs40 <- add_quantile_column(data_abs40, CITIES, "pol_ma4", FALSE, 40)
  data_abs40 <- add_quantile_column(data_abs40, CITIES, "pol_ma5", FALSE, 40)
  data_abs40 <- add_quantile_column(data_abs40, CITIES, "pol_ma6", FALSE, 40)
  data_abs40 <- add_quantile_column(data_abs40, CITIES, "pol_ma7", FALSE, 40)
  
  # subset feb to apr
  data_abs40 <- data_abs40 %>%
    filter(month %in% c(2, 3, 4))
  
  # export data
  write.csv(data_abs40, file = paste0(root_dir, "/data/data_abs40.csv"), row.names = FALSE)
}

# bool_perc_cutoff = FALSE, bisection_cutoff = 60
{
  data_abs60 <- data
  
  # create quantiles
  data_abs60 <- add_quantile_column(data_abs60, CITIES, "pol_0", FALSE, 60)
  data_abs60 <- add_quantile_column(data_abs60, CITIES, "pol_1", FALSE, 60)
  data_abs60 <- add_quantile_column(data_abs60, CITIES, "pol_2", FALSE, 60)
  data_abs60 <- add_quantile_column(data_abs60, CITIES, "pol_3", FALSE, 60)
  data_abs60 <- add_quantile_column(data_abs60, CITIES, "pol_4", FALSE, 60)
  data_abs60 <- add_quantile_column(data_abs60, CITIES, "pol_5", FALSE, 60)
  data_abs60 <- add_quantile_column(data_abs60, CITIES, "pol_6", FALSE, 60)
  data_abs60 <- add_quantile_column(data_abs60, CITIES, "pol_7", FALSE, 60)
  data_abs60 <- add_quantile_column(data_abs60, CITIES, "pol_ma1", FALSE, 60)
  data_abs60 <- add_quantile_column(data_abs60, CITIES, "pol_ma2", FALSE, 60)
  data_abs60 <- add_quantile_column(data_abs60, CITIES, "pol_ma3", FALSE, 60)
  data_abs60 <- add_quantile_column(data_abs60, CITIES, "pol_ma4", FALSE, 60)
  data_abs60 <- add_quantile_column(data_abs60, CITIES, "pol_ma5", FALSE, 60)
  data_abs60 <- add_quantile_column(data_abs60, CITIES, "pol_ma6", FALSE, 60)
  data_abs60 <- add_quantile_column(data_abs60, CITIES, "pol_ma7", FALSE, 60)
  
  # subset feb to apr
  data_abs60 <- data_abs60 %>%
    filter(month %in% c(2, 3, 4))
  
  # export data
  write.csv(data_abs60, file = paste0(root_dir, "/data/data_abs60.csv"), row.names = FALSE)
}

# bool_perc_cutoff = FALSE, bisection_cutoff = 80
{
  data_abs80 <- data
  
  # create quantiles
  data_abs80 <- add_quantile_column(data_abs80, CITIES, "pol_0", FALSE, 80)
  data_abs80 <- add_quantile_column(data_abs80, CITIES, "pol_1", FALSE, 80)
  data_abs80 <- add_quantile_column(data_abs80, CITIES, "pol_2", FALSE, 80)
  data_abs80 <- add_quantile_column(data_abs80, CITIES, "pol_3", FALSE, 80)
  data_abs80 <- add_quantile_column(data_abs80, CITIES, "pol_4", FALSE, 80)
  data_abs80 <- add_quantile_column(data_abs80, CITIES, "pol_5", FALSE, 80)
  data_abs80 <- add_quantile_column(data_abs80, CITIES, "pol_6", FALSE, 80)
  data_abs80 <- add_quantile_column(data_abs80, CITIES, "pol_7", FALSE, 80)
  data_abs80 <- add_quantile_column(data_abs80, CITIES, "pol_ma1", FALSE, 80)
  data_abs80 <- add_quantile_column(data_abs80, CITIES, "pol_ma2", FALSE, 80)
  data_abs80 <- add_quantile_column(data_abs80, CITIES, "pol_ma3", FALSE, 80)
  data_abs80 <- add_quantile_column(data_abs80, CITIES, "pol_ma4", FALSE, 80)
  data_abs80 <- add_quantile_column(data_abs80, CITIES, "pol_ma5", FALSE, 80)
  data_abs80 <- add_quantile_column(data_abs80, CITIES, "pol_ma6", FALSE, 80)
  data_abs80 <- add_quantile_column(data_abs80, CITIES, "pol_ma7", FALSE, 80)
  
  # subset feb to apr
  data_abs80 <- data_abs80 %>%
    filter(month %in% c(2, 3, 4))
  
  # export data
  write.csv(data_abs80, file = paste0(root_dir, "/data/data_abs80.csv"), row.names = FALSE)
}
