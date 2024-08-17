
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
  root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
  source(paste0(root_dir, "/proj/functions.R"))  # import functions
}

# import data
{
  tmp <- read.csv(file = paste0(root_dir, "/data/fdata.csv"))
  bisection_variation <- "perc75"
  data <- read.csv(file = paste0(
    root_dir,
    paste0("/data/data_", bisection_variation, ".csv")
  ))
  data[, "date"] <- as.Date(data[, "date"])
}

# run glm model for non-interactive models ------------------------------------------------------------------------

# set params
{
  i <- 0
  CITIES <- {
    c(
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
  OUTCOMES <- c("all", "circ", "resp", "all65", "circ65", "resp65")
  EXPOSURES <- c("spm", "no2", "so2", "pol")
  LAGS <- c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5")
}

# run non-interactive model; three for loops, creating a matrix of (outcomes * exposures * city)
{
  noninteractive <- list()  # temp list to append data.frames
  for (outcome in OUTCOMES) {
    for (exposure in EXPOSURES) {
      intermediary <- list()
      for (city in CITIES) {
        i = i + 1
        cat(
          i,
          "/",
          length(OUTCOMES) * length(EXPOSURES) * length(CITIES),
          "(outcome, exposure, city):",
          outcome,
          exposure,
          city,
          "\n"
        )
        results <- run_noninteractive_glm_model(
          city = city,
          data = data[data["city"] == city, ],
          outcome = outcome,
          exposure = exposure,
          lags = LAGS,
          asian_dust_days = "ad",
          holiday = "holiday",
          day_of_week = "dow",
          day_of_year = "doy",
          year = "year",
          temp_mean = "tave07",
          date = "date",
          season_df = 4,
          relative_humidity_mean = "rhave"
        )
        
        results[, "city"] <- city
        intermediary <- append(intermediary, list(results))
      }
      noninteractive <- append(noninteractive, list(do.call(rbind, intermediary)))
    }
  }
  noninteractive <- do.call(rbind, noninteractive)  # collapsing list into data.frame
}

# run glm model for interactive models ----------------------------------------------------------------------------

# set params
{
  EXPOSURES <- c("spm", "no2", "so2")  # drop "pol" as it's interactive
}

# run interactive model; three for loops, creating a matrix of (outcomes * exposures * city)
{
  i <- 0
  interactive_bisection <- list()
  for (outcome in OUTCOMES) {
    for (exposure in EXPOSURES) {
      intermediary <- list()
      for (city in CITIES) {
        i = i + 1
        cat(
          i,
          "/",
          length(OUTCOMES) * length(EXPOSURES) * length(CITIES),
          "(outcome, exposure, city):",
          outcome,
          exposure,
          city,
          "\n"
        )
        results <- run_interactive_glm_model(
          city = city,
          data = data[data["city"] == city, ],
          outcome = outcome,
          exposure = exposure,
          interactive = "pol",
          quantile_type = "bisection",
          lags = LAGS,
          asian_dust_days = "ad",
          holiday = "holiday",
          day_of_week = "dow",
          day_of_year = "doy",
          year = "year",
          temp_mean = "tave07",
          date = "date",
          season_df = 4,
          relative_humidity_mean = "rhave"
        )
        
        results[, "city"] <- city
        intermediary <- append(intermediary, list(results))
      }
      
      all_columns <- unique(unlist(lapply(intermediary, names)))  # identify unique column names
      intermediary <- lapply(intermediary, function(df) {
        missing_columns <- setdiff(all_columns, names(df))
        if (length(missing_columns) > 0) {
          df[missing_columns] <- NA
        }
        return(df[, all_columns])  # ensure consistent column order across all data frames
      })
      interactive_bisection <- append(interactive_bisection, list(do.call(rbind, intermediary)))
    }
  }
  interactive_bisection <- do.call(rbind, interactive_bisection)  # collapsing list into data.frame
}

# checkpoint 1 ----------------------------------------------------------------------------------------------------

# export data
{
  write.csv(noninteractive,
            paste0(root_dir, "/data/noninteractive.csv"),
            row.names = FALSE)
  write.csv(interactive_bisection,
            paste0(
              root_dir,
              paste0("/data/interactive_bisection", bisection_variation, ".csv")
            ),
            row.names = FALSE)
}

# set params and import data
{
  rm(list = ls())  # reset
  root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
  source(paste0(root_dir, "/proj/functions.R"))
  bisection_variation <- "perc75"
  noninteractive <- read.csv(file = paste0(root_dir, "/data/noninteractive.csv"))
  interactive_bisection <- read.csv(file = paste0(
    root_dir,
    paste0("/data/interactive_bisection", bisection_variation, ".csv")
  ))
}

# metafor: pool all city results ----------------------------------------------------------------------------------

# set params
{
  CITIES <- {
    c(
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
  OUTCOMES <- c("all", "circ", "resp", "all65", "circ65", "resp65")
  EXPOSURES <- c("spm", "no2", "so2")  # no "pol" here as it's interactive
  LAGS <- c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5")
}

# noninteractive
{
  metafor_noninteractive <- list()
  for (outcome in OUTCOMES) {
    for (exposure in  c("spm", "no2", "so2", "pol")) {
      results <- run_metafor_noninteractive(
        data = noninteractive,
        exposure = exposure,
        outcome = outcome,
        lags = LAGS
      )
      metafor_noninteractive <- append(metafor_noninteractive, list(results))
    }
  }
  metafor_noninteractive <- do.call(rbind, metafor_noninteractive)
}

# interactive
{
  metafor_bisection <- list()
  for (outcome in OUTCOMES) {
    for (exposure in EXPOSURES) {
      # for bisection
      results <- run_metafor_bisection(
        data = interactive_bisection,
        exposure = exposure,
        outcome = outcome,
        quantile_type = "bisection",
        lags = LAGS
      )
      metafor_bisection <- append(metafor_bisection, list(results))
    }
  }
  metafor_bisection <- do.call(rbind, metafor_bisection)
}

# checkpoint 2 ---------------------------------------------------------------------------------------------------

# export data
{
  write.csv(
    metafor_noninteractive,
    paste0(root_dir, "/data/metafor_noninteractive.csv"),
    row.names = FALSE
  )
  write.csv(metafor_bisection, paste0(
    root_dir,
    paste0("/data/metafor_bisection", bisection_variation, ".csv")
  ), row.names = FALSE)
}

# set params and import data
{
  rm(list = ls())  # reset
  root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
  source(paste0(root_dir, "/proj/functions.R"))
  bisection_variation <- "perc75"
  metafor_noninteractive <- read.csv(file = paste0(root_dir, "/data/metafor_noninteractive.csv"))
  metafor_bisection <- read.csv(file = paste0(
    root_dir,
    "/data/metafor_bisection",
    bisection_variation,
    ".csv"
  ))
}

# plot -----------------------------------------------------------------------------------------------------------

# non-interactive parameters
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

# interactive parameters
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
