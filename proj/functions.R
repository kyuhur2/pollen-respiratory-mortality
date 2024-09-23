
library(dplyr)
library(ggplot2)
library(scales)

# constants -------------------------------------------------------------------------------------------------------

options(error = traceback)

# testing  ----------------------------------------------------------------

TEST <- FALSE
if (TEST == TRUE) {
  root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
  data <- read.csv(file = paste0(
    root_dir,
    paste0("/data/data_", bisection_variation, ".csv")
  ))
  data[, "date"] <- as.Date(data[, "date"])
  
  outcome <- "resp65"
  exposure <- "spm"
  city <- "Miyazaki"
  data <- data[data["city"] == city, ]
  interactive <- "pol"
  quantile_type <- "quartile"
  lags <- c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5")
  asian_dust_days <- "ad"
  holiday <- "holiday"
  day_of_week <- "dow"
  day_of_year <- "doy"
  year <- "year"
  temp_mean <- "tave07"
  date <- "date"
  relative_humidity_mean <- "rhave"
  seasonal_df <- 4
  temperature_df <- 3
  k <- "ma5"
}
if (TEST == TRUE) {
  root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
  quantile_type <- "bisection"
  data <- read.csv(file = paste0(root_dir, "/data/interactive_", quantile_type, ".csv"))
  exposure <- "spm"
  outcome <- "resp65"
  lags <- c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5")
  q <- 1
  k <- 1
}

# misc functions --------------------------------------------------------------------------------------------------

parse_args <- function(args) {
  parsed <- list()
  
  for (arg in args) {
    # split argument into name and value at '='
    split_arg <- strsplit(arg, "=")[[1]]
    if (length(split_arg) == 2) {
      name <- split_arg[1]
      value <- split_arg[2]
      
      # assign values to a list with appropriate type conversion
      parsed[[name]] <- value
    }
  }
  
  return(parsed)
}

progress_bar <- function(total) {
  progress::progress_bar$new(
    format = "\033[38;5;214m[:bar] :percent :current/:total :elapsed\033[0m",
    total = total,
    width = 100,
    clear = FALSE
  )
}

# logging functions -----------------------------------------------------------------------------------------------

debug <- function(message) {
  cat("\033[32m [DEBUG] ", message, "\033[39m\n", sep = "")
}

warn <- function(message) {
  cat("\033[33m [WARNING] ", message, "\033[39m\n", sep = "")
}

error <- function(message) {
  cat("\033[31m [ERROR] ", message, "\033[39m\n", sep = "")
}

# data processing functions -----------------------------------------------

add_quantile_column <- function(data,
                                CITIES,
                                column_name,
                                bool_perc_cutoff,
                                bisection_cutoff) {
  results <- list()
  for (city in CITIES) {
    if (column_name %in% names(data)) {
      intermediary_data <- data[data["city"] == city, ]
      
      # determine breaks for each category
      quartile_breaks <- quantile(intermediary_data[[column_name]],
                                  probs = c(0, 0.25, 0.5, 0.75, 1),
                                  na.rm = TRUE)
      tercile_breaks <- c(-Inf, 10, 30, Inf)
      
      if (bool_perc_cutoff) {
        bisection_breaks <- quantile(
          intermediary_data[[column_name]],
          probs = c(0, bisection_cutoff, 1),
          na.rm = TRUE
        )
      } else {
        bisection_breaks <- c(-Inf, bisection_cutoff, Inf)
      }
      
      # Add categorized columns to data
      intermediary_data <- intermediary_data %>%
        mutate(
          !!paste0(column_name, "_quartile") := factor(
            cut(
              !!sym(column_name),
              breaks = quartile_breaks,
              include.lowest = TRUE,
              labels = FALSE
            )
          ),
          !!paste0(column_name, "_tercile") := factor(
            cut(
              !!sym(column_name),
              breaks = tercile_breaks,
              include.lowest = TRUE,
              labels = FALSE
            )
          ),
          !!paste0(column_name, "_bisection") := factor(
            cut(
              !!sym(column_name),
              breaks = bisection_breaks,
              include.lowest = TRUE,
              labels = FALSE
            )
          )
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
  data <- data[, c("date", "city", "SuHi")]
  results <- list()
  for (city in CITIES) {
    city_data <- data %>% filter(city == !!city)
    city_quantiles <- quantile(city_data$SuHi, probs = quantiles, na.rm = TRUE)
    results[[city]] <- round(city_quantiles, 2)
  }
  results <- do.call(cbind, results)
  rownames(results) <- paste0(quantiles * 100, "%")
  return(results)
}

calculate_exposure_absolute_cutoffs <- function(data, column, CITIES, quantiles) {
  quantiles <- c(20, 40, 60, 80)
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

# sensitivity functions -------------------------------------------------------------------------------------------

calculate_qaic <- function(model) {
  residual_deviance <- summary(model)$deviance
  chat <- summary(model)$dispersion
  p <- length(coef(model))
  qaic <- residual_deviance + 2 * p * chat
  return(qaic)
}

calculate_model_p_value <- function(model1, model2) {
  anova_result <- anova(model1, model2, test="Chisq")
  p_value <- anova_result[2, "Pr(>Chi)"]
  return(round(p_value, 4))
}

# modeling functions ------------------------------------------------------

run_noninteractive_glm_model <- function(city,
                                         data,
                                         outcome,
                                         exposure,
                                         lags,
                                         asian_dust_days,
                                         holiday,
                                         day_of_week,
                                         day_of_year,
                                         year,
                                         temp_mean,
                                         date,
                                         relative_humidity_mean,
                                         seasonal_df,
                                         temperature_df) {
  
  model_result_collector <- list()
  
  for (k in lags) {
    # Prepare lagged exposure
    lagged_exposure <- data[, paste0(exposure, "_", k)]
    
    # Try to fit the model and extract results
    result <- tryCatch({
      model_result <- glm(
        data[, outcome] ~
          lagged_exposure +
          factor(data[, asian_dust_days]) +
          factor(data[, holiday]) +
          data[, relative_humidity_mean] +
          data[, day_of_week] +
          ns(data[, day_of_year], df = seasonal_df):factor(data[, year]) +
          ns(data[, temp_mean], df = temperature_df) +
          ns(data[, date], df = 1),
        family = quasipoisson
      )
      
      # Extract results
      ci <- ci.exp(model_result, subset = "lagged_exposure")
      rr <- ci[1]
      cil <- ci[2]
      ciu <- ci[3]
      B <- coef(summary(model_result))["lagged_exposure", "Estimate"]
      se <- coef(summary(model_result))["lagged_exposure", "Std. Error"]
      p_value <- coef(summary(model_result))["lagged_exposure", "Pr(>|t|)"]
      iqr <- IQR(lagged_exposure, na.rm = TRUE)
      
      # Collect results
      data.frame(
        rr = rr,
        cil = cil,
        ciu = ciu,
        B = B,
        se = se,
        p_value = p_value,
        lag = k,
        iqr = iqr,
        city = city,
        exposure = exposure,
        outcome = outcome,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(result)) {
      model_result_collector[[length(model_result_collector) + 1]] <- result
    }
    # If result is NULL due to error, skip collecting results
  }
  
  # Combine results into a dataframe if there are any
  if (length(model_result_collector) > 0) {
    model_result_df <- do.call(rbind, model_result_collector)
    return(model_result_df)
  } else {
    # Return NULL if no models were successfully fitted
    return(NULL)
  }
}


run_interactive_glm_model <- function(city,
                                      data,
                                      outcome,
                                      exposure,
                                      interactive,
                                      quantile_type,
                                      lags,
                                      asian_dust_days,
                                      holiday,
                                      day_of_week,
                                      day_of_year,
                                      year,
                                      temp_mean,
                                      date,
                                      relative_humidity_mean,
                                      seasonal_df,
                                      temperature_df) {
  tryCatch({
    # run glm model for all exposure x lags pairs; collect data into model_result_collector
    model_result_collector <- list()
    for (k in lags) {
      lagged_exposure <- data[, paste0(exposure, "_", k)]
      lagged_interactive <- as.factor(data[, paste0(interactive, "_", k, "_", quantile_type)])
      
      # run model
      if (city == "Miyazaki" & exposure == "spm" & outcome == "resp65" & k == "ma3") {
        model_result <- NULL
      } else {
        model_result <- glm(
          data[, outcome] ~
            lagged_interactive +
            lagged_exposure:lagged_interactive +
            factor(data[, asian_dust_days]) +
            factor(data[, holiday]) +
            data[, relative_humidity_mean] +
            data[, day_of_week] +
            ns(data[, day_of_year], df = seasonal_df):factor(data[, year]) +
            ns(data[, temp_mean], df = temperature_df) +
            ns(data[, date], df = 1),
          family = quasipoisson
        )
      }
      
      # collect metrics
      if (is.null(model_result)) {
        # handle the error by filling in NA values
        idx <- length(model_result_collector) + 1
        model_result_collector[[idx]] <- model_result_collector[[1]]  # assumes that [[1]] exists
        model_result_collector[[idx]][] <- NA
      } else {
        # collect results based on adjusted_lagged_exposure
        ci_vector <- as.vector(t(ci.exp(model_result, subset = "lagged_exposure")))  # results
        
        # extract B, se, p_value
        coeff_matrix <- summary(model_result)$coefficients
        B_vector <- coeff_matrix[grepl("lagged_exposure", rownames(coeff_matrix)), ][, "Estimate"]
        se_vector <- coeff_matrix[grepl("lagged_exposure", rownames(coeff_matrix)), ][, "Std. Error"]
        p_value_vector <- coeff_matrix[grepl("lagged_exposure", rownames(coeff_matrix)), ][, "Pr(>|t|)"]
        
        # attach to model_result_collector
        model_result_collector[[length(model_result_collector) + 1]] <- c(ci_vector, B_vector, se_vector, p_value_vector)
      }
    }
    
    x <- model_result_collector
    
    # convert to data.frame
    model_result_collector <- do.call(rbind,
                                      lapply(model_result_collector, function(x)
                                        data.frame(t(x), stringsAsFactors = FALSE)))
    
    # column names: rows, separator, columns
    row_names <- rownames(ci.exp(model_result, subset = "lagged_exposure"))
    row_names <- gsub("lagged_exposure", "exposure", row_names)
    row_names <- gsub("lagged_interactive", "interactive", row_names)
    colnames(model_result_collector) <- c(paste0(c("rr", "cil", "ciu"), "_", rep(row_names, each = 3)),
                                          paste0(c("B"), "_", rep(row_names, each = 1)),
                                          paste0(c("se"), "_", rep(row_names, each = 1)),
                                          paste0(c("p_value"), "_", rep(row_names, each = 1)))

    # add column "city", "exposure", "outcome" to the results
    model_result_collector["city"] <- city
    model_result_collector["exposure"] <- exposure
    model_result_collector[, "lag"] <- paste0(lags)
    model_result_collector["iqr"] <- stats::IQR(lagged_exposure, na.rm = TRUE)
    model_result_collector["outcome"] <- outcome
    model_result_collector["quantile_type"] <- quantile_type

  # end of tryCatch block
  }, error = function(e) {
    error(paste("error at: ", city, outcome, exposure, k, sep=" "))
    error(paste0("error output: ", e))
  })
  
  return(model_result_collector)
}

run_gaseous_conf_glm_model <- function(city,
                                       data,
                                       outcome,
                                       exposure,
                                       interactive,
                                       confounding,
                                       quantile_type,
                                       lags,
                                       asian_dust_days,
                                       holiday,
                                       day_of_week,
                                       day_of_year,
                                       year,
                                       temp_mean,
                                       date,
                                       relative_humidity_mean,
                                       seasonal_df,
                                       temperature_df) {
  tryCatch({
    # run glm model for all exposure x lags pairs; collect data into model_result_collector
    model_result_collector <- list()
    for (k in lags) {
      lagged_exposure <- data[, paste0(exposure, "_", k)]
      lagged_interactive <- as.factor(data[, paste0(interactive, "_", k, "_", quantile_type)])
      lagged_confounding <- data[, paste0(confounding, "_", k)]
      
      # run model
      if (
        (city == "Miyazaki" & exposure == "spm" & outcome == "resp65" & k == "ma3") || 
        (city == "Miyazaki" & exposure == "spm" & outcome == "resp" & k == "ma3")
      ) {
        model_result <- NULL
      } else {
        model_result <- glm(
          data[, outcome] ~
            lagged_interactive +
            lagged_exposure:lagged_interactive +
            lagged_confounding +
            factor(data[, asian_dust_days]) +
            factor(data[, holiday]) +
            data[, relative_humidity_mean] +
            data[, day_of_week] +
            ns(data[, day_of_year], df = seasonal_df):factor(data[, year]) +
            ns(data[, temp_mean], df = temperature_df) +
            ns(data[, date], df = 1),
          family = quasipoisson
        )
      }
      
      # collect metrics
      if (is.null(model_result)) {
        # handle the error by filling in NA values
        idx <- length(model_result_collector) + 1
        model_result_collector[[idx]] <- model_result_collector[[1]]
        model_result_collector[[idx]][] <- NA
      } else {
        # collect results based on adjusted_lagged_exposure
        ci_vector <- as.vector(t(ci.exp(model_result, subset = "lagged_exposure"))) # results
        
        # extract B, se, p_value
        coeff_matrix <- summary(model_result)$coefficients
        B_vector <- coeff_matrix[grepl("lagged_exposure", rownames(coeff_matrix)), ][, "Estimate"]
        se_vector <- coeff_matrix[grepl("lagged_exposure", rownames(coeff_matrix)), ][, "Std. Error"]
        p_value_vector <- coeff_matrix[grepl("lagged_exposure", rownames(coeff_matrix)), ][, "Pr(>|t|)"]
        
        # attach to model_result_collector
        model_result_collector[[length(model_result_collector) + 1]] <- c(ci_vector, B_vector, se_vector, p_value_vector)
      }
    }
    
    # convert to data.frame
    model_result_collector <- do.call(rbind,
                                      lapply(model_result_collector, function(x)
                                        data.frame(t(x), stringsAsFactors = FALSE)))
    
    # column names: rows, separator, columns
    row_names <- rownames(ci.exp(model_result, subset = "lagged_exposure"))
    row_names <- gsub("lagged_exposure", "exposure", row_names)
    row_names <- gsub("lagged_interactive", "interactive", row_names)
    colnames(model_result_collector) <- c(paste0(c("rr", "cil", "ciu"), "_", rep(row_names, each = 3)),
                                          paste0(c("B"), "_", rep(row_names, each = 1)),
                                          paste0(c("se"), "_", rep(row_names, each = 1)),
                                          paste0(c("p_value"), "_", rep(row_names, each = 1)))
  
    # add column "city", "exposure", "outcome" to the results
    model_result_collector["city"] <- city
    model_result_collector["outcome"] <- outcome
    model_result_collector["exposure"] <- exposure
    model_result_collector["confounding"] <- confounding
    model_result_collector[, "lag"] <- paste0(lags)
    model_result_collector["iqr"] <- stats::IQR(lagged_exposure, na.rm = TRUE)
    model_result_collector["quantile_type"] <- quantile_type

  # end of tryCatch block
  }, error = function(e) {
    error(paste("error at: ", city, outcome, exposure, k, sep=" "))
    error(paste0("error output: ", e))
  })
  
  return(model_result_collector)
}

# meta-analysis functions -------------------------------------------------

run_metafor_noninteractive <- function(data, exposure, outcome, lags) {
  # start by subsetting data
  data <- data[data["exposure"] == exposure &
                 data["outcome"] == outcome, ]
  iqrm <- mean(data[, "iqr"]) # calculate iqr for each column related to "exposure" in data
  
  # initialize matrix for results
  matrix_columns <- c("B", "se", "I2", "p.Qtest", "rr", "cil", "ciu")
  results <- matrix(
    numeric(0),
    nrow = length(lags),
    ncol = length(matrix_columns),
    dimnames = list(NULL, matrix_columns)
  )
  
  # loop through each lag and perform meta-analysis
  for (k in seq(lags)) {
    est_vector <- as.numeric(data[data["lag"] == lags[k], ]$B)
    se_vector <- as.numeric(data[data["lag"] == lags[k], ]$se)
    
    # random effects meta-analysis
    meta_analysis <- metafor::rma(
      yi = est_vector,
      sei = se_vector,
      data = cbind(est_vector, se_vector),
      method = "REML"
    )
    
    # store results from meta-analysis in matrix
    results[k, 1:4] <- c(meta_analysis$b,
                         meta_analysis$se,
                         meta_analysis$I2,
                         meta_analysis$QEp)
    
    # calculate relative risk (RR) and confidence intervals (CI)
    results[k, "rr"] <- exp(meta_analysis$b * iqrm)
    results[k, "cil"] <- exp((meta_analysis$b - 1.96 * meta_analysis$se) * iqrm)
    results[k, "ciu"] <- exp((meta_analysis$b + 1.96 * meta_analysis$se) * iqrm)
  }
  
  # Convert matrix to data frame
  df_results <- as.data.frame(results)
  df_results$exposure <- exposure
  df_results$outcome <- outcome
  df_results$lag <- paste0(lags)
  df_results$iqrm <- iqrm
  
  return(df_results)
}

run_metafor_quartile <- function(data,
                                 exposure,
                                 outcome,
                                 quantile_type,
                                 lags) {
  # start by subsetting data
  data <- data[data["exposure"] == exposure &
                 data["outcome"] == outcome &
                 data["quantile_type"] == quantile_type, ]
  iqrm <- mean(data[, "iqr"]) # calculate iqr for each column related to "exposure" in data
  
  # initialize matrix for results
  quantile_names <- c(
    "interactive1.exposure",
    "interactive2.exposure",
    "interactive3.exposure",
    "interactive4.exposure"
  )
  matrix_columns <- c("B", "se", "I2", "p.Qtest", "rr", "cil", "ciu", "quantile")
  results <- matrix(
    numeric(0),
    nrow = (length(lags)) * length(quantile_names),
    ncol = length(matrix_columns),
    dimnames = list(NULL, matrix_columns)
  )
  
  # loop over quantiles and lags
  for (q in 1:length(quantile_names)) {
    for (k in seq(lags)) {
      est_vector <- as.numeric(data[data["lag"] == lags[k], paste0("B_", quantile_names[q])])
      se_vector <- as.numeric(data[data["lag"] == lags[k], paste0("se_", quantile_names[q])])
      row_number <- k + (length(lags)) * (q - 1) # adjust row number for each quantile
      
      if (length(est_vector) > 0 &&
          !any(is.na(est_vector)) && !any(is.na(se_vector))) {
        tryCatch({
          meta_analysis <- metafor::rma(yi = est_vector,
                                        sei = se_vector,
                                        method = "REML")
          
          # store results from meta-analysis in matrix
          results[row_number, 1:4] <- c(meta_analysis$b,
                                        meta_analysis$se,
                                        meta_analysis$I2,
                                        meta_analysis$QEp)
          results[row_number, "rr"] <- exp(meta_analysis$b * iqrm)
          results[row_number, "cil"] <- exp((meta_analysis$b - 1.96 * meta_analysis$se) * iqrm)
          results[row_number, "ciu"] <- exp((meta_analysis$b + 1.96 * meta_analysis$se) * iqrm)
          results[row_number, "quantile"] <- q
        }, error = function(e) {
          cat("Error processing lag",
              k,
              "quantile",
              q,
              ":",
              e$message,
              "\n")
          results[row_number, ] <- rep(NA, length(matrix_columns))
        })
      } else {
        cat("Data missing or NA for lag", k, "quantile", q, "\n")
        results[row_number, ] <- rep(NA, length(matrix_columns))
      }
    }
  }
  
  # convert matrix to data frame
  df_results <- as.data.frame(results)
  df_results$exposure <- exposure
  df_results$outcome <- outcome
  df_results$quantile_type <- quantile_type
  df_results$lag <- paste0(rep(lags, length(quantile_names)))
  df_results$iqrm <- iqrm
  
  return(df_results)
}

run_metafor_tercile <- function(data,
                                exposure,
                                outcome,
                                quantile_type,
                                lags) {
  # start by subsetting data
  data <- data[data["exposure"] == exposure &
                 data["outcome"] == outcome &
                 data["quantile_type"] == quantile_type, ]
  iqrm <- mean(data[, "iqr"]) # calculate iqr for each column related to "exposure" in data
  
  # initialize matrix for results
  quantile_names <- c("interactive1.exposure",
                      "interactive2.exposure",
                      "interactive3.exposure")
  matrix_columns <- c("B", "se", "I2", "p.Qtest", "rr", "cil", "ciu", "quantile")
  results <- matrix(
    numeric(0),
    nrow = (length(lags)) * length(quantile_names),
    ncol = length(matrix_columns),
    dimnames = list(NULL, matrix_columns)
  )
  
  # loop over quantiles and lags
  for (q in 1:length(quantile_names)) {
    for (k in seq(lags)) {
      est_vector <- as.numeric(data[data["lag"] == lags[k], paste0("B_", quantile_names[q])])
      se_vector <- as.numeric(data[data["lag"] == lags[k], paste0("se_", quantile_names[q])])
      row_number <- k + (length(lags)) * (q - 1) # adjust row number for each quantile
      
      if (length(est_vector) > 0 &&
          !any(is.na(est_vector)) && !any(is.na(se_vector))) {
        tryCatch({
          meta_analysis <- metafor::rma(yi = est_vector,
                                        sei = se_vector,
                                        method = "REML")
          
          # store results from meta-analysis in matrix
          results[row_number, 1:4] <- c(meta_analysis$b,
                                        meta_analysis$se,
                                        meta_analysis$I2,
                                        meta_analysis$QEp)
          results[row_number, "rr"] <- exp(meta_analysis$b * iqrm)
          results[row_number, "cil"] <- exp((meta_analysis$b - 1.96 * meta_analysis$se) * iqrm)
          results[row_number, "ciu"] <- exp((meta_analysis$b + 1.96 * meta_analysis$se) * iqrm)
          results[row_number, "quantile"] <- q
        }, error = function(e) {
          cat("Error processing lag",
              k,
              "quantile",
              q,
              ":",
              e$message,
              "\n")
          results[row_number, ] <- rep(NA, length(matrix_columns))
        })
      } else {
        cat("Data missing or NA for lag", k, "quantile", q, "\n")
        results[row_number, ] <- rep(NA, length(matrix_columns))
      }
    }
  }
  
  # convert matrix to data frame
  df_results <- as.data.frame(results)
  df_results$exposure <- exposure
  df_results$outcome <- outcome
  df_results$quantile_type <- quantile_type
  df_results$lag <- paste0(rep(lags, length(quantile_names)))
  df_results$iqrm <- iqrm
  
  return(df_results)
}

run_metafor_bisection <- function(data,
                                  exposure,
                                  outcome,
                                  quantile_type,
                                  lags) {
  # start by subsetting data
  data <- data[data["exposure"] == exposure &
                 data["outcome"] == outcome &
                 data["quantile_type"] == quantile_type, ]
  iqrm <- mean(data[, "iqr"]) # calculate iqr for each column related to "exposure" in data
  
  # initialize matrix for results
  quantile_names <- c("interactive1.exposure", "interactive2.exposure")
  matrix_columns <- c("B", "se", "I2", "p.Qtest", "rr", "cil", "ciu", "quantile")
  results <- matrix(
    numeric(0),
    nrow = (length(lags)) * length(quantile_names),
    ncol = length(matrix_columns),
    dimnames = list(NULL, matrix_columns)
  )
  
  # loop over quantiles * lags
  for (q in 1:length(quantile_names)) {
    for (k in seq(lags)) {
      est_vector <- as.numeric(data[data["lag"] == lags[k], paste0("B_", quantile_names[q])])
      se_vector <- as.numeric(data[data["lag"] == lags[k], paste0("se_", quantile_names[q])])
      row_number <- k + (length(lags)) * (q - 1) # adjust row number for each quantile
      
      if (length(est_vector) > 0 &&
          !any(is.na(est_vector)) && !any(is.na(se_vector))) {
        tryCatch({
          meta_analysis <- metafor::rma(yi = est_vector,
                                        sei = se_vector,
                                        method = "REML")
          
          # store results from meta-analysis in matrix
          results[row_number, 1:4] <- c(meta_analysis$b,
                                        meta_analysis$se,
                                        meta_analysis$I2,
                                        meta_analysis$QEp)
          results[row_number, "rr"] <- exp(meta_analysis$b * iqrm)
          results[row_number, "cil"] <- exp((meta_analysis$b - 1.96 * meta_analysis$se) * iqrm)
          results[row_number, "ciu"] <- exp((meta_analysis$b + 1.96 * meta_analysis$se) * iqrm)
          results[row_number, "quantile"] <- q
        }, error = function(e) {
          cat("Error processing lag",
              k,
              "quantile",
              q,
              ":",
              e$message,
              "\n")
          results[row_number, ] <- rep(NA, length(matrix_columns))
        })
      } else {
        cat("Data missing or NA for lag", k, "quantile", q - 1, "\n")
        results[row_number, ] <- rep(NA, length(matrix_columns))
      }
    }
  }
  
  # convert matrix to data frame
  df_results <- as.data.frame(results)
  df_results$exposure <- exposure
  df_results$outcome <- outcome
  df_results$quantile_type <- quantile_type
  df_results$lag <- paste0(rep(lags, length(quantile_names)))
  df_results$iqrm <- iqrm
  
  return(df_results)
}

run_metafor_bisection_conf <- function(data,
                                  exposure,
                                  outcome,
                                  confounding,
                                  quantile_type,
                                  lags) {
  # start by subsetting data
  data <- data[data["exposure"] == exposure &
                 data["outcome"] == outcome &
                 data["confounding"] == confounding &
                 data["quantile_type"] == quantile_type, ]
  iqrm <- mean(data[, "iqr"]) # calculate iqr for each column related to "exposure" in data
  
  # initialize matrix for results
  quantile_names <- c("interactive1.exposure", "interactive2.exposure")
  matrix_columns <- c("B", "se", "I2", "p.Qtest", "rr", "cil", "ciu", "quantile")
  results <- matrix(
    numeric(0),
    nrow = (length(lags)) * length(quantile_names),
    ncol = length(matrix_columns),
    dimnames = list(NULL, matrix_columns)
  )
  
  # loop over quantiles * lags
  for (q in 1:length(quantile_names)) {
    for (k in seq(lags)) {
      est_vector <- as.numeric(data[data["lag"] == lags[k], paste0("B_", quantile_names[q])])
      se_vector <- as.numeric(data[data["lag"] == lags[k], paste0("se_", quantile_names[q])])
      row_number <- k + (length(lags)) * (q - 1) # adjust row number for each quantile
      
      if (length(est_vector) > 0 &&
          !any(is.na(est_vector)) && !any(is.na(se_vector))) {
        tryCatch({
          meta_analysis <- metafor::rma(yi = est_vector,
                                        sei = se_vector,
                                        method = "REML")
          
          # store results from meta-analysis in matrix
          results[row_number, 1:4] <- c(meta_analysis$b,
                                        meta_analysis$se,
                                        meta_analysis$I2,
                                        meta_analysis$QEp)
          results[row_number, "rr"] <- exp(meta_analysis$b * iqrm)
          results[row_number, "cil"] <- exp((meta_analysis$b - 1.96 * meta_analysis$se) * iqrm)
          results[row_number, "ciu"] <- exp((meta_analysis$b + 1.96 * meta_analysis$se) * iqrm)
          results[row_number, "quantile"] <- q
        }, error = function(e) {
          cat("Error processing lag",
              k,
              "quantile",
              q,
              ":",
              e$message,
              "\n")
          results[row_number, ] <- rep(NA, length(matrix_columns))
        })
      } else {
        cat("Data missing or NA for lag", k, "quantile", q - 1, "\n")
        results[row_number, ] <- rep(NA, length(matrix_columns))
      }
    }
  }
  
  # convert matrix to data frame
  df_results <- as.data.frame(results)
  df_results$exposure <- exposure
  df_results$outcome <- outcome
  df_results$quantile_type <- quantile_type
  df_results$lag <- paste0(rep(lags, length(quantile_names)))
  df_results$iqrm <- iqrm
  
  return(df_results)
}

# plotting functions ----------------------------------------------------------------------------------------------

create_noninteractive_plot <- function(exposure,
                                       outcome,
                                       lags,
                                       data,
                                       y_lower_bound,
                                       y_upper_bound,
                                       point_color,
                                       point_shape,
                                       debug) {
  # subset data
  data <- data[data["exposure"] == exposure &
                 data["outcome"] == outcome & data$lag %in% lags, ]
  data$facet_group <- ifelse(data$lag %in% c("0", "1", "2", "3", "4", "5"),
                             "Lags",
                             "Moving Averages")
  data$lag <- recode(
    data$lag,
    "ma1" = "0-1",
    "ma2" = "0-2",
    "ma3" = "0-3",
    "ma4" = "0-4",
    "ma5" = "0-5"
  )
  data$lag <- factor(data$lag,
                     levels = c("0", "1", "2", "3", "4", "5", "0-1", "0-2", "0-3", "0-4", "0-5"))

  # plot
  x <- ggplot(data, aes(x = lag, y = rr)) +
    geom_errorbar(aes(ymin = cil, ymax = ciu),
                  color = "black",
                  width = 0.4) +
    geom_hline(yintercept = 1.0,
               linetype = "dotted",
               color = "black") +
    geom_point(size = 2.5,
               color = point_color,
               shape = point_shape) +
    labs(x = NULL, y = "RR per IQR Increase", title = " ") +
    theme_classic() +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      panel.spacing = unit(0.5, "lines")
    ) +
    scale_y_continuous(limits = c(y_lower_bound, y_upper_bound),
                       labels = label_number(accuracy = 0.01)) +
    facet_grid(~ facet_group, scales = "free_x", space = "free")
  
  return(x)
}

create_interactive_plot <- function(exposure,
                                    outcome,
                                    lags,
                                    data,
                                    cutoffs_vec,
                                    y_lower_bound,
                                    y_upper_bound,
                                    point_shapes,
                                    point_colors,
                                    legend_label,
                                    debug) {
  # params
  levels <- c("Low L0", "High L0", "Low L1", "High L1", "Low L2", "High L2")
  
  # construct dataframe with the 4 cutoffs
  data_frames <- list()
  for (i in seq(length(data))) {
    tmp <- data[[i]]
    tmp <- tmp[tmp["exposure"] == exposure &
                 tmp["outcome"] == outcome & tmp$lag %in% lags, ]
    tmp$cutoffs <- cutoffs_vec[i]
    data_frames <- append(data_frames, list(tmp))
  }
  data <- do.call(rbind, data_frames) # collapse into one dataframe
  
  # create the levels for x_axis without spaces
  x_axis_levels <- paste0(rep(cutoffs_vec, times = length(levels)),
                          "Q",
                          rep(rep(1:2, each = 3), times = 3),
                          "L",
                          rep(0:2, each = 6))
  if (debug) { debug(paste("x_axis_levels:", x_axis_levels)) }

  # mutate combination of cutoffs * quantile * lags
  if (debug) { debug(paste("cutoffs * quantile * lags:", data$cutoffs, "Q", data$quantile, "L", data$lag)) }
  data <- data %>%
    arrange(lag, quantile, cutoffs) %>%
    mutate(x_axis = factor(paste0(cutoffs, "Q", quantile, "L", lag), levels = x_axis_levels))
  
  # create grouping variable that groups every 3 elements together
  if (debug) { debug(paste("levels:", levels)) }
  data$group <- factor(rep(levels, each = length(cutoffs_vec)), levels = levels)
  
  # labellers for facet()
  group_labeller <- function(variable, value) {
    return(
      c(
        "<b>Low</b>",
        "<b>High</b>",
        "<b>Low</b>",
        "<b>High</b>",
        "<b>Low</b>",
        "<b>High</b>"
      )
    )
  }
  
  # plot using facet_wrap to create side-by-side plots
  x <- ggplot(data,
              aes(
                x = x_axis,
                y = rr,
                shape = as.factor(cutoffs),
                color = as.factor(cutoffs)
              )) +
    geom_errorbar(aes(ymin = cil, ymax = ciu),
                  color = "black",
                  width = 0.75) +
    geom_hline(yintercept = 1.0,
               linetype = "dotted",
               color = "black") +
    geom_point(size = 2.5) +
    labs(
      x = "Lag 0      Lag 0      Lag 1      Lag 1      Lag 2      Lag 2",
      y = "RR per IQR Increase",
      title = " ",
      shape = legend_label,
      color = legend_label
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0.5, "lines"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-5, 5, -5, -5),
      legend.title = element_text(size = 10, face = "bold"),
      strip.text = element_markdown()
    ) +
    scale_y_continuous(limits = c(y_lower_bound, y_upper_bound),
                       labels = label_number(accuracy = 0.01)) +
    facet_wrap(~ group,
               scales = "free_x",
               nrow = 1,
               labeller = group_labeller) +
    scale_shape_manual(values = point_shapes) +
    scale_color_manual(values = point_colors)
  
  return(x)
}

run_meta_analysis <- function(data) {
  iqrm <- mean(data$iqrm)
  meta_analysis <- metafor::rma(
    yi = data$B,
    sei = data$se,
    data = cbind(data$B, data$se),
    method = "REML"
  )
  
  B <- meta_analysis$b
  se <- meta_analysis$se
  rr <- exp(meta_analysis$b * iqrm)
  cil <- exp((meta_analysis$b - 1.96 * meta_analysis$se) * iqrm)
  ciu <- exp((meta_analysis$b + 1.96 * meta_analysis$se) * iqrm)
  
  return(
    data.frame(
      B = B,
      se = se,
      cil = cil,
      ciu = ciu,
      rr = rr,
      iqrm = iqrm
    )
  )
}
