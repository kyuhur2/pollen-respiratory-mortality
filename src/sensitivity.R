
library(splines)
library(ggplot2)

# set up ----------------------------------------------------------------------------------------------------------

# initialize project state
{
  rm(list = ls())
  os <- Sys.info()[["sysname"]]
  if ("Darwin" %in% os) {
    root_dir <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality"
  } else {
    root_dir <- "C:/Users/kyuhu/OneDrive/Documents/Github/pollen-respiratory-mortality"
  }
  source(paste0(root_dir, "/src/functions.R"))  # import functions
}

# import data
{
  bisection_variation <- "perc75"  # perc75, perc80, perc85, abs25, abs50, abs75
  data <- read.csv(file = paste0(
    root_dir,
    paste0("/data/data_", bisection_variation, ".csv")
  ))
  data[, "date"] <- as.Date(data[, "date"])
}

# init data (Fukuoka) and params
{
  outcome <- "resp"
  exposure <- "spm_1"
  interactive = "pol_1"
  asian_dust_days = "ad"
  holiday = "holiday"
  day_of_week = "dow"
  day_of_year = "doy"
  year = "year"
  temp_mean = "tave07"
  date = "date"
  relative_humidity_mean = "rhave"
  
  required_columns <- c(
    outcome,
    exposure,
    interactive,
    asian_dust_days,
    holiday,
    relative_humidity_mean,
    day_of_week,
    day_of_year,
    year,
    temp_mean,
    date
  )
}

# non-interactive df sensitivity analysis -------------------------------------------------------------------------

# params
temperature_df_range <- 2:7  # range for df of temp_mean
seasonal_df_range <- 2:7  # range for df of season
cities <- c(
  "Fukuoka",
  "Kumamoto",
  "Nagasaki",
  "Oita",
  "Saga",
  "Kagoshima",
  "Miyazaki",
  "Kitakyushu"
)
noninteractive <- data.frame(
  temperature_df = integer(),
  seasonal_df = integer(),
  city = character(),
  qaic = numeric(),
  B = numeric(),
  se = numeric(),
  cil = numeric(),
  ciu = numeric(),
  rr = numeric()
)

# progress bar
progress <- progress::progress_bar$new(
  format = "  [:bar] :percent :current/:total :elapsed",
  total = (max(temperature_df_range) - min(temperature_df_range) + 1) *
    (max(seasonal_df_range) - min(seasonal_df_range) + 1) * length(cities),
  width = 100
)

# run models, number of combinations: temperature_df_range * seasonal_df_range * cities
for (temperature_df in temperature_df_range) {
  for (seasonal_df in seasonal_df_range) {
    for (city in cities) {
      progress$tick()

      # update the ns() terms with new df values for date, temp_mean, season
      temperature_term <- paste0("ns(data[, '", temp_mean, "'], df = ", temperature_df, ")")
      seasonal_term <- paste0(
        "ns(data[, '",
        day_of_year,
        "'], df = ",
        seasonal_df,
        "):factor(data[, '",
        year,
        "']) + "
      )
      
      # full non-interactive model formula with updated df for date, temp_mean, season
      formula <- paste0(
        "data[, '",
        outcome,
        "'] ~ data[, '",
        exposure,
        "'] + ",
        "ns(data[, '", date, "'], df = 1)",
        " + ",
        temperature_term,
        " + ",
        seasonal_term,
        " + ",
        "data[, '",
        relative_humidity_mean,
        "'] + ",
        "factor(data[, '",
        asian_dust_days,
        "']) + ",
        "factor(data[, '",
        holiday,
        "']) + ",
        "data[, '",
        day_of_week,
        "']"
      )
      
      # fit the model with the updated formula
      model <- glm(as.formula(formula),
                   data = data[data["city"] == city & complete.cases(data[, required_columns]), ],
                   family = quasipoisson)
      
      # calculate metrics
      qaic <- calculate_qaic(model)
      iqr <- stats::IQR(data[, paste0(exposure)], na.rm = TRUE)
      B <- coef(model)['data[, "spm_1"]']
      se <- Epi::ci.lin(model, subset = '"spm_1"')[1, "StdErr"]
      cil <- Epi::ci.exp(model, subset = '"spm_1"')[1, "2.5%"]
      ciu <- Epi::ci.exp(model, subset = '"spm_1"')[1, "97.5%"]
      rr <- exp(B)
      
      # store df values and metrics
      noninteractive <- rbind(
        noninteractive,
        data.frame(
          temperature_df = temperature_df,
          seasonal_df = seasonal_df,
          city = city,
          qaic = qaic,
          iqr = iqr,
          B = B,
          se = se,
          cil <- cil,
          ciu <- ciu,
          rr = rr
        )
      )
    }
  }
}

# export noninteractive_full
write.csv(noninteractive, paste0(root_dir, "/data/noninteractive_full.csv"))
noninteractive <- read.csv(paste0(root_dir, "/data/noninteractive_full.csv"))

# aggregated sum(qaic), pooled rr/ciu/cil across cities
noninteractive_aggregated <- data.frame(
  temperature_df = integer(),
  seasonal_df = integer(),
  qaic = numeric(),
  B = numeric(),
  se = numeric(),
  cil = numeric(),
  ciu = numeric(),
  rr = numeric()
)

for (temperature_df in temperature_df_range) {
  for (seasonal_df in seasonal_df_range) {
    # subset dataframe by temperature_df and seasonal_df
    tmp <- noninteractive[
      noninteractive["temperature_df"] == temperature_df &
      noninteractive["seasonal_df"] == seasonal_df,
    ]
    iqrm <- mean(tmp[, "iqr"])

    # aggregated metrics
    if (length(tmp$qaic) == length(cities)) {
      qaic_aggregated <- sum(tmp$qaic)
    } else {
      qaic_aggregated <- 0
      error(paste0("Cities are missing. Defaulting to 0. Present cities: ", tmp$city))
    }
    meta_analysis <- metafor::rma(
      yi = tmp$B,
      sei = tmp$se,
      data = cbind(tmp$B, tmp$se),
      method = "REML"
    )
    B <- meta_analysis$b
    se <- meta_analysis$se
    rr <- exp(meta_analysis$b * iqrm)
    cil <- exp((meta_analysis$b - 1.96 * meta_analysis$se) * iqrm)
    ciu <- exp((meta_analysis$b + 1.96 * meta_analysis$se) * iqrm)
    
    # store aggregated metrics
    noninteractive_aggregated <- rbind(
      noninteractive_aggregated,
      data.frame(
        temperature_df = temperature_df,
        seasonal_df = seasonal_df,
        qaic = qaic_aggregated,
        B = B,
        se = se,
        cil = cil,
        ciu = ciu,
        rr = rr,
        iqrm = iqrm
      )
    )
  }
}

# export noninteractive_aggregated
write.csv(noninteractive_aggregated, paste0(root_dir, "/data/noninteractive_aggregated.csv"))

