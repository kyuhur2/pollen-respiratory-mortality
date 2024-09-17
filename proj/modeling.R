
library(lubridate)
library(Epi)
library(tsModel)
library(splines)
library(mgcv)
library(dplyr)
library(metafor)
library(data.table)

# set up ----------------------------------------------------------------------------------------------------------

# initialize project state
{
  rm(list = ls())
  root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
  source(paste0(root_dir, "/proj/functions.R"))  # import functions
}

# set params from R args
{
  params <- parse_args(commandArgs(trailingOnly = TRUE))
  bisection_variation <- params[["bisection_variation"]]  # perc75, perc80, perc85, abs25, abs50, abs75
  seasonal_df <- as.numeric(params[["seasonal_df"]])
  temperature_df <- as.numeric(params[["temperature_df"]])
  debug(paste0("Args: ", bisection_variation, " ", seasonal_df, " ", temperature_df))
}

# import data
{
  data <- read.csv(file = paste0(
    root_dir,
    paste0("/data/data_", bisection_variation, ".csv")
  ))
  data[, "date"] <- as.Date(data[, "date"])
}

# run glm model for non-interactive models ------------------------------------------------------------------------

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
  EXPOSURES <- c("spm", "no2", "so2", "pol")
  LAGS <- c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5")
  
  progress_noninteractive <- progress_bar(
    total = length(OUTCOMES) * length(EXPOSURES) * length(CITIES)
  )
}

# run non-interactive model; three for loops, creating a matrix of (outcomes * exposures * city)
# run non-interactive model; three for loops, creating a matrix of (outcomes * exposures * city)
{
  noninteractive <- list()  # temp list to append data.frames
  for (outcome in OUTCOMES) {
    for (exposure in EXPOSURES) {
      intermediary <- list()
      for (city in CITIES) {
        progress_noninteractive$tick()
        
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
          relative_humidity_mean = "rhave",
          seasonal_df = seasonal_df,
          temperature_df = temperature_df
        )
        
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
  EXPOSURES <- c("spm", "no2", "so2")  # drop "pol"
  LAGS <- c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5")

  # progress bar
  progress_interactive <- progress_bar(
    total = length(OUTCOMES) * length(EXPOSURES) * length(CITIES)
  )
}

# run interactive model; three for loops, creating a matrix of (outcomes * exposures * city)
{
  i <- 0
  interactive_bisection <- list()
  for (outcome in OUTCOMES) {
    for (exposure in EXPOSURES) {
      intermediary <- list()
      for (city in CITIES) {
        progress_interactive$tick()
        
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
          relative_humidity_mean = "rhave",
          seasonal_df = seasonal_df,
          temperature_df = temperature_df
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

# run glm model for interactive confounding model -----------------------------------------------------------------

# set params
{
  exposure <- "spm"  # no EXPOSURES required--always "spm"
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
  LAGS <- c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5")
  CONFOUNDING <- c("no2", "so2")
  
  # progress bar
  progress_confounding <- progress_bar(
    total = length(OUTCOMES) * length(CONFOUNDING) * length(CITIES)
  )
}

# run interactive gaseous confounding model; three for loops, creating a matrix of (outcomes * confounding * city)
{
  i <- 0
  confounding_bisection <- list()
  for (outcome in OUTCOMES) {
    for (confounding in CONFOUNDING) {
      intermediary <- list()
      for (city in CITIES) {
        progress_confounding$tick()
        
        results <- run_gaseous_conf_glm_model(
          city = city,
          data = data[data["city"] == city, ],
          outcome = outcome,
          exposure = exposure,
          interactive = "pol",
          confounding = confounding,
          quantile_type = "bisection",
          lags = LAGS,
          asian_dust_days = "ad",
          holiday = "holiday",
          day_of_week = "dow",
          day_of_year = "doy",
          year = "year",
          temp_mean = "tave07",
          date = "date",
          relative_humidity_mean = "rhave",
          seasonal_df = seasonal_df,
          temperature_df = temperature_df
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
      confounding_bisection <- append(confounding_bisection, list(do.call(rbind, intermediary)))
    }
  }
  confounding_bisection <- do.call(rbind, confounding_bisection)  # collapsing list into data.frame
}

# checkpoint 1 ----------------------------------------------------------------------------------------------------

# export data
{
  if (exists("noninteractive")) {
    write.csv(noninteractive,
            paste0(root_dir, "/data/noninteractive.csv"),
            row.names = FALSE)
  } else {
    error("Object 'noninteractive' not found.")
  }
  
  if (exists("interactive_bisection")) {
    write.csv(interactive_bisection,
              paste0(root_dir,
                     paste0("/data/interactive_bisection_", bisection_variation, ".csv")),
              row.names = FALSE)
  } else {
    error("Object 'interactive_bisection' not found.")
  }
  
  if (exists("confounding_bisection")) {
    write.csv(confounding_bisection,
              paste0(root_dir,
                     paste0("/data/confounding_bisection_", bisection_variation, ".csv")),
              row.names = FALSE)
  } else {
    error("Object 'confounding_bisection' not found.")
  }
}

# set params and import data
{
  root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
  source(paste0(root_dir, "/proj/functions.R"))
  noninteractive <- read.csv(file = paste0(root_dir, "/data/noninteractive.csv"))
  interactive_bisection <- read.csv(file = paste0(
    root_dir,
    paste0("/data/interactive_bisection_", bisection_variation, ".csv")
  ))
  confounding_bisection <- read.csv(file=paste0(
    root_dir,
    paste0("/data/confounding_bisection_", bisection_variation, ".csv")
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

# gaseous confounding
{
  metafor_bisection_no2 <- list()
  for (outcome in OUTCOMES) {
    # for bisection
    results <- run_metafor_bisection_conf(
      data = confounding_bisection,
      exposure = "spm",
      outcome = outcome,
      confounding = "no2",
      quantile_type = "bisection",
      lags = LAGS
    )
    metafor_bisection_no2 <- append(metafor_bisection_no2, list(results))
  }
  metafor_bisection_no2 <- do.call(rbind, metafor_bisection_no2)
  
  metafor_bisection_so2 <- list()
  for (outcome in OUTCOMES) {
    # for bisection
    results <- run_metafor_bisection_conf(
      data = confounding_bisection,
      exposure = "spm",
      outcome = outcome,
      confounding = "so2",
      quantile_type = "bisection",
      lags = LAGS
    )
    metafor_bisection_so2 <- append(metafor_bisection_so2, list(results))
  }
  metafor_bisection_so2 <- do.call(rbind, metafor_bisection_so2)
}

# checkpoint 2 ---------------------------------------------------------------------------------------------------

# export data
{
  if (exists("metafor_noninteractive")) {
    write.csv(
      metafor_noninteractive,
      paste0(root_dir, "/data/metafor_noninteractive.csv"),
      row.names = FALSE
    )
  } else {
    error("Object 'metafor_noninteractive' not found.")
  } 
  
  if (exists("metafor_bisection")) {
    write.csv(metafor_bisection, paste0(
      root_dir,
      paste0("/data/metafor_bisection_", bisection_variation, ".csv")
    ), row.names = FALSE)
  } else {
    error("Object 'metafor_bisection' not found.")
  } 

  if (exists("metafor_bisection_no2")) {
    write.csv(metafor_bisection_no2, paste0(
      root_dir,
      paste0("/data/metafor_bisection_no2_", bisection_variation, ".csv")
    ), row.names = FALSE)
  } else {
    error("Object 'metafor_bisection_no2' not found.")
  } 

  if (exists("metafor_bisection_so2")) {
    write.csv(metafor_bisection_so2, paste0(
      root_dir,
      paste0("/data/metafor_bisection_so2_", bisection_variation, ".csv")
    ), row.names = FALSE)
  } else {
    error("Object 'metafor_bisection_so2' not found.")
  } 
}
