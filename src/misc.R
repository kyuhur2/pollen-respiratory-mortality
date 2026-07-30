
library(lubridate)
library(Epi)
library(tsModel)
library(splines)
library(mgcv)
library(dplyr)
library(mixmeta)
library(metafor)
library(data.table)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

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

# set params from R args
{
  params <- parse_args(commandArgs(trailingOnly = TRUE))
  bisection_variation <- params["bisection_variation"]
  seasonal_df <- params["seasonal_df"]
  temperature_df <- params["temperature_df"]
  bisection_variation <- "abs75"  # perc75, perc80, perc85, abs25, abs50, abs75
  seasonal_df <- 4
  temperature_df <- 3
}

# import data
{
  tmp <- read.csv(file = paste0(root_dir, "/data/fdata.csv"))
  data <- read.csv(file = paste0(
    root_dir,
    paste0("/data/data_", bisection_variation, ".csv")
  ))
  data[, "date"] <- as.Date(data[, "date"])
}

# set params and import data
{
  root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
  source(paste0(root_dir, "/src/functions.R"))
  
  metafor_noninteractive <- read.csv(file = paste0(root_dir, "/data/metafor_noninteractive.csv"))
  
  metafor_bisection <- read.csv(file = paste0(
    root_dir,
    "/data/metafor_bisection_",
    bisection_variation,
    ".csv"
  ))
  
  metafor_bisection_no2 <- read.csv(file = paste0(
    root_dir,
    "/data/metafor_bisection_no2_",
    bisection_variation,
    ".csv"
  ))

  metafor_bisection_so2 <- read.csv(file = paste0(
    root_dir,
    "/data/metafor_bisection_so2_",
    bisection_variation,
    ".csv"
  ))
}

# non-interactive qaic --------------------------------------------------------------------------------------------

# set params
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
}

# define the terms with constants
terms <- list(
  paste0("factor(data[, '", asian_dust_days, "'])")
)

# intermediary data structures to hold formulas and qaics
formulas <- character()
qaics <- numeric()
p_values <- logical()

for (city in cities) {
  # naive model
  formula <- paste0(
    "data[, '", outcome, "'] ~ ",
    "data[, '", exposure, "']",
    "ns(data[, '", date, "'], df = 1) + ",  # long-term trend
    "ns(data[, '", day_of_year, "'], df = 4):factor(data[, '", year, "']) + ",  # seasonal trend
    "data[, '", day_of_week, "'] + ",  # day of week

    "ns(data[, '", temp_mean, "'], df = 4) + ",  # temperature

    # -- optional --
    "data[, '", relative_humidity_mean, "'] + ",  # relative humidity
    "factor(data[, '", holiday, "']) + ",  # holiday
    "factor(data[, '", asian_dust_days, "'])"  # asian dust day
  )
  model <- glm(
    as.formula(formula),
    data = data[data["city"] == city & complete.cases(data[, required_columns]), ],
    family = quasipoisson
  )
  qaic <- calculate_qaic(model)
  
  # update intermediary data structures
  formulas[1] <- formula
  qaics[1] <- qaic
  p_values[1] <- NA
  
  # loop through the terms and calculate qAIC after adding each one
  for (i in 1:length(terms)) {
    # update the formula by adding the new term
    formula <- paste(formula, "+", terms[i])
    updated_model <- glm(
      as.formula(formula),
      data = data[data["city"] == city & complete.cases(data[, required_columns]), ],
      family = quasipoisson
    )
    qaic <- calculate_qaic(updated_model)
    p_value <- calculate_model_p_value(model, updated_model)
  
    # update intermediary data structures
    formulas[i + 1] <- terms[[i]]
    qaics[i + 1] <- qaic
    p_values[i + 1] <- p_value
    model <- updated_model
  }
}

noninteractive <- data.frame(formulas = formulas,
                             qaics = qaics,
                             p_value = p_values)

# export data
write.csv(noninteractive,
          paste0(root_dir, "/data/noninteractive_qaic.csv"),
          row.names = FALSE)

# plot -----------------------------------------------------------------------------------------------------------

# non-interactive
{
  exposure <- "so2"
  outcome <- "circ"
  lags <- c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5")
  dot_color <- "red"
  data <- metafor_noninteractive[metafor_noninteractive["exposure"] == exposure &
                                   metafor_noninteractive["outcome"] == outcome &
                                   metafor_noninteractive$lag %in% lags, ]
}

{
  ggplot(data, aes(x = lag, y = rr)) +
    geom_point(color = dot_color) +
    geom_errorbar(aes(ymin = cil, ymax = ciu),
                  color = "black",
                  width = 0.2) +
    geom_hline(yintercept = 1.0,
               linetype = "dashed",
               color = "gray") +
    labs(
      x = "Lag",
      y = "Relative Risk (RR)",
      title = paste(
        "Noninteractive RR across Lag 0-5, ma1-ma5 for",
        exposure,
        outcome
      )
    ) +
    theme_classic() # theme_light(), theme_dark(), theme_gray()
}

# interactive
{
  exposure <- "spm"
  outcome <- "resp"
  lags <- c(0, 1, 2)
  dot_color <- "blue"
  data <- metafor_bisection[metafor_bisection["exposure"] == exposure &
                              metafor_bisection["outcome"] == outcome &
                              metafor_bisection$lag %in% lags, ]
}

{
  ggplot(data, aes(x = interaction(quantile, lag), y = rr)) +
    geom_point(color = dot_color) +
    geom_errorbar(aes(ymin = cil, ymax = ciu),
                  color = "black",
                  width = 0.2) +
    geom_hline(yintercept = 1.0,
               linetype = "dashed",
               color = "gray") +
    geom_vline(xintercept = 2.5,
               linetype = "dashed",
               color = "gray") +
    geom_vline(xintercept = 4.5,
               linetype = "dashed",
               color = "gray") +
    labs(
      x = "Quantile and Lag",
      y = "Relative Risk (RR)",
      title =  paste("Interactive RR across Lag 0-2 for", exposure, outcome)
    ) +
    theme_classic() +
    scale_x_discrete(
      labels = function(x)
        paste(
          "Q",
          gsub("\\..*", "", as.integer(x)),
          "\nLag",
          gsub(".*\\.", "", x)
        )
    )
}

# gaseous confounding
{
  exposure <- "spm"
  outcome <- "resp"
  confounding <- "no2"
  lags <- c(0, 1, 2)
  dot_color <- "blue"
  data <- metafor_bisection[metafor_bisection["exposure"] == exposure &
                              metafor_bisection["outcome"] == outcome &
                              metafor_bisection["confounding"] == confounding &
                              metafor_bisection$lag %in% lags, ]
}

{
  ggplot(data, aes(x = interaction(quantile, lag), y = rr)) +
    geom_point(color = dot_color) +
    geom_errorbar(aes(ymin = cil, ymax = ciu),
                  color = "black",
                  width = 0.2) +
    geom_hline(yintercept = 1.0,
               linetype = "dashed",
               color = "gray") +
    geom_vline(xintercept = 2.5,
               linetype = "dashed",
               color = "gray") +
    geom_vline(xintercept = 4.5,
               linetype = "dashed",
               color = "gray") +
    labs(
      x = "Quantile and Lag",
      y = "Relative Risk (RR)",
      title =  paste("Interactive RR across Lag 0-2 for", exposure, outcome)
    ) +
    theme_classic() +
    scale_x_discrete(
      labels = function(x)
        paste(
          "Q",
          gsub("\\..*", "", as.integer(x)),
          "\nLag",
          gsub(".*\\.", "", x)
        )
    )
}

# figure 1 --------------------------------------------------------------------------------------------------------


{
  root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
  
  # Save as PDF instead of TIFF
  pdf(
    paste0(root_dir, "/plots/study_map.pdf"),
    width = 10.67,  # Width in inches
    height = 6.00   # Height in inches
  )
  
  par(mfrow = c(1, 2), mar = c(4, 5, 0.1, 0.1))
  japan_geodata <- getData("GADM", country = "JPN", level = 1)
  
  xdegrees = seq(129, 132, 1)
  ydegrees = seq(31, 34, 1)
  xdegrees_ = sapply(xdegrees, function(x)
    bquote(.(x) * degree ~ E))
  ydegrees_ = sapply(ydegrees, function(x)
    bquote(.(x) * degree ~ N))
  
  # city plot
  cexcity <- 1.5
  pchcity <- c(seq(7, 10, 1), seq(21, 24, 1))
  colcity <- rep(c(1, 2, 3, 4), 2)
  
  plot(
    japan_geodata,
    xlim = c(130, 131),
    ylim = c(30.75, 34.25),
    xlab = "Latitude",
    col = "white",
    border = "gray50",
    axes = F,
    las = 1,
    bty = "n",
    box = F
  )
  axis(1, at = xdegrees, labels = do.call(expression, xdegrees_))
  axis(
    2,
    at = ydegrees,
    labels = do.call(expression, ydegrees_),
    las = 1
  )
  mtext(side = 2, text = "Longitude", line = 3.5)
  
  city_codes <- read.csv(paste(root_dir, "/data/city_geocodes.csv", sep =
                                 ""))
  CITIES <- c(
    "Fukuoka",
    "Kumamoto",
    "Nagasaki",
    "Oita",
    "Saga",
    "Kagoshima",
    "Miyazaki",
    "Kitakyushu"
  )
  
  for (i in seq(nrow(city_codes))) {
    points(
      city_codes[i, 3],
      city_codes[i, 2],
      col = colcity[i],
      cex = cexcity,
      pch = pchcity[i]
    )
  }
  
  legend(
    "bottomright",
    CITIES,
    pch = pchcity,
    col = colcity,
    cex = .8,
    bty = 'n'
  )
  
  # Plot 2 - Measurement stations and clinics plot
  
  cexval <- 1.2
  pch1 <- 17
  pch2 <- 7
  col1 <- 4
  col2 <- 2
  
  plot(
    japan_geodata,
    xlim = c(130, 131),
    ylim = c(30.75, 34.25),
    xlab = "Latitude",
    col = "white",
    border = "gray50",
    axes = F,
    las = 1,
    bty = "n",
    box = F
  )
  axis(1, at = xdegrees, labels = do.call(expression, xdegrees_))
  axis(
    2,
    at = ydegrees,
    labels = do.call(expression, ydegress_),
    las = 1
  )
  mtext(side = 2, text = "Longitude", line = 3.5)
  
  airpcodes <- read.csv(paste(root_dir, "/data/airp_station_geocodes.csv", sep =
                                ""))
  airpcodes <- airpcodes[, c(1:2, 5:6)]
  
  for (i in seq(nrow(airpcodes))) {
    points(airpcodes[i, 4],
           airpcodes[i, 3],
           col = col1,
           cex = cexval,
           pch = pch1)
  }
  
  pollencodes <- read.csv(paste(root_dir, "/data/clinic_geocodes.csv", sep =
                                  ""))
  pollencodes <- pollencodes[, c(1, 4:5)]
  names(pollencodes) <- c("clinicid", "latitude", 'longitude')
  pollencodes <- pollencodes[pollencodes$clinicid %in% c(2, 3, 4, 8, 11, 17, 23, 25, 27, 28, 36, 38, 44, 49, 54, 59), ]
  
  for (i in seq(nrow(pollencodes))) {
    points(
      pollencodes[i, 3],
      pollencodes[i, 2],
      col = col2,
      cex = cexval,
      pch = pch2
    )
  }
  
  legend(
    "bottomright",
    c("Pollen Clinics", "Air Pollution \nStations"),
    pch = c(pch1, pch2),
    col = c(col1, col2),
    cex = .8,
    bty = 'n'
  )
  
  dev.off()
}

