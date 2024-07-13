# for testing
TEST <- FALSE
if (TEST == TRUE) {
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
  season_df <- 4
  relative_humidity_mean <- "rhave"
  k <- 0
}

run_noninteractive_glm_model <- function(
  city,
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
  season_df,
  relative_humidity_mean
) {
  # validate input param types
  if(!is.character(city)) stop("Input 'city' not 'character' type.")
  if(!is.data.frame(data)) stop("Input 'data' not 'data.frame' type.")
  if(!is.character(outcome)) stop("Input 'outcome' not 'character type.")
  if(!is.character(exposure)) stop("Input 'exposure' not 'character' type.")
  if(!is.vector(lags)) stop("Input 'lags' not 'vector' type.")
  if(!is.character(asian_dust_days)) stop("Input 'asian_dust_days' not 'character' type.")
  if(!is.character(holiday)) stop("Input 'holiday' not 'character' type.")
  if(!is.character(day_of_week)) stop("Input 'day_of_week' not 'character' type.")
  if(!is.character(day_of_year)) stop("Input 'day_of_year' not 'character' type.")
  if(!is.character(year)) stop("Input 'year' not 'character' type.")
  if(!is.character(temp_mean)) stop("Input 'temp_mean' not 'character' type.")
  if(!is.character(date)) stop("Input 'date' not 'character' type.")
  if(!is.numeric(season_df)) stop("Input 'season_df' not 'numeric' type.")
  if(!is.character(relative_humidity_mean)) stop(
    "Input 'relative_humidity_mean' not 'character' type."
  )

  # init collector frame
  model_result_collector <- list()
  
  # run glm model for all exposure x lags pairs; collect data into model_result_collector
  for(k in lags){
    # rename as adjusted exposure easier processing (even if not adjusted)
    lagged_exposure <- data[, paste0(exposure, "_", k)]
    
    # run model
    model_result <- tryCatch({
      glm(
        data[, outcome] ~
          lagged_exposure +
          factor(data[, asian_dust_days]) +
          factor(data[, holiday]) +
          data[, relative_humidity_mean] +
          data[, day_of_week] +
          ns(data[, day_of_year], df=season_df):factor(data[, year]) +
          ns(data[, temp_mean], df=3) +
          ns(data[, date], df=1),
        family=quasipoisson
      )
    }, error = function(e) {
      NULL
    })

    # collect equation and variables
    if (is.null(model_result)) {
      next
    } else {
      model_metadata <- list(c(
        model = model_result["call"],
        outcome = outcome,
        exposure = exposure,
        k = k,
        asian_dust_days = asian_dust_days,
        holiday = holiday,
        relative_humidity_mean = relative_humidity_mean,
        day_of_week = day_of_week,
        day_of_year = day_of_year,
        year = year,
        temp_mean = temp_mean,
        date = date
      ))

      model_metadata <- paste(sapply(names(model_metadata[[1]]), function(name) {
        paste(name, model_metadata[[1]][[name]], sep = "=")
      }), collapse = "; ")
  
      model_result_collector[[length(model_result_collector) + 1]] <- c(
        ci.exp(model_result, subset="lagged_exposure"),
        summary(model_result)$coefficients[, "Estimate"][["lagged_exposure"]],
        summary(model_result)$coefficients[, "Std. Error"][["lagged_exposure"]],
        model_metadata
      )
    }
  }
  
  # convert to data.frame
  model_result_collector <- do.call(
    rbind,
    lapply(model_result_collector, function(x) data.frame(t(x), stringsAsFactors = FALSE))
  )
  colnames(model_result_collector) <- c("rr", "cil", "ciu", "B", "se", "model_metadata")

  # add column "city", "exposure", "outcome" to the results
  model_result_collector["city"] <- city
  model_result_collector["exposure"] <- exposure
  if (city == "Miyazaki" & exposure == "spm" & outcome == "resp65") {
    model_result_collector[, "lag"] <- c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma4", "ma5")
  } else {
    model_result_collector[, "lag"] <- paste0(lags)
  }
  model_result_collector["iqr"] <- IQR(lagged_exposure, na.rm = TRUE)
  model_result_collector["outcome"] <- outcome

  # validate output
  if(!is.data.frame(model_result_collector)) stop(
    "Output 'model_result_collector' not 'data.frame type."
  )
  
  return(model_result_collector)
}

run_interactive_glm_model <- function(
  city,
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
  season_df,
  relative_humidity_mean
) {
  # validate input param types
  if(!is.character(city)) stop("Input 'city' not 'character' type.")
  if(!is.data.frame(data)) stop("Input 'data' not 'data.frame' type.")
  if(!is.character(outcome)) stop("Input 'outcome' not 'character type.")
  if(!is.character(exposure)) stop("Input 'exposure' not 'character' type.")
  if(!is.character(interactive)) stop("Input 'interactive' not 'character' type.")
  if (!is.character(quantile_type) || !(quantile_type %in% c("quartile", "tercile", "bisection"))) {
    stop("Invalid input: 'quantile_type' must be one of 'quartile', 'tercile', or 'bisection'.")
  }
  if(!is.vector(lags)) stop("Input 'lags' not 'vector' type.")
  if(!is.character(asian_dust_days)) stop("Input 'asian_dust_days' not 'character' type.")
  if(!is.character(holiday)) stop("Input 'holiday' not 'character' type.")
  if(!is.character(day_of_week)) stop("Input 'day_of_week' not 'character' type.")
  if(!is.character(day_of_year)) stop("Input 'day_of_year' not 'character' type.")
  if(!is.character(year)) stop("Input 'year' not 'character' type.")
  if(!is.character(temp_mean)) stop("Input 'temp_mean' not 'character' type.")
  if(!is.character(date)) stop("Input 'date' not 'character' type.")
  if(!is.numeric(season_df)) stop("Input 'season_df' not 'numeric' type.")
  if(!is.character(relative_humidity_mean)) stop(
    "Input 'relative_humidity_mean' not 'character' type."
  )
  
  # init collector frame
  model_result_collector <- list()
  
  # run glm model for all exposure x lags pairs; collect data into model_result_collector
  for(k in lags) {
    lagged_exposure <- data[, paste0(exposure, "_", k)]
    # TODO: create this dynamically based on the LAGGED version of SuHi (e.g. SPMout5 -> category of SuHiout5)
    lagged_interactive <- as.factor(data[, paste0(interactive, "_", k, "_", quantile_type)]) 

    # run model
    model_result <- tryCatch({
      glm(
        data[, outcome] ~
          lagged_interactive +
          lagged_exposure:lagged_interactive +
          factor(data[, asian_dust_days]) +
          factor(data[, holiday]) +
          data[, relative_humidity_mean] +
          data[, day_of_week] +
          ns(data[, day_of_year], df=season_df):factor(data[, year]) +
          ns(data[, temp_mean], df=3) +
          ns(data[, date], df=1),
        family=quasipoisson
      )
    }, error = function(e) {
      NULL
    })
    
    # collect equation and variables
    if (is.null(model_result)) {
      # handle the error by filling in NA values
      model_metadata <- list(c(
        model = NA,
        outcome = outcome,
        exposure = exposure,
        k = k,
        asian_dust_days = asian_dust_days,
        holiday = holiday,
        relative_humidity_mean = relative_humidity_mean,
        day_of_week = day_of_week,
        day_of_year = day_of_year,
        year = year,
        temp_mean = temp_mean,
        date = date
      ))
      
      model_metadata <- paste(sapply(names(model_metadata[[1]]), function(name) {
        paste(name, model_metadata[[1]][[name]], sep = "=")
      }), collapse = "; ")
      
      model_result_collector[[length(model_result_collector) + 1]] <- c(
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        model_metadata
      )
    } else {
      model_metadata <- list(c(
        model = model_result["call"],
        outcome = outcome,
        exposure = exposure,
        k = k,
        interactive = interactive,
        quantile_type = quantile_type,
        asian_dust_days = asian_dust_days,
        holiday = holiday,
        relative_humidity_mean = relative_humidity_mean,
        day_of_week = day_of_week,
        day_of_year = day_of_year,
        year = year,
        temp_mean = temp_mean,
        date = date
      ))
  
      model_metadata <- paste(sapply(names(model_metadata[[1]]), function(name) {
        paste(name, model_metadata[[1]][[name]], sep = "=")
      }), collapse = "; ")
  
      # collect results based on adjusted_lagged_exposure
      ci_vector <- as.vector(t(ci.exp(model_result, subset="lagged_exposure")))  # results
  
      # extract B, se
      coeff_matrix <- summary(model_result)$coefficients
      B_vector <- coeff_matrix[grepl("lagged_exposure", rownames(coeff_matrix)), ][, "Estimate"]
      se_vector <- coeff_matrix[grepl("lagged_exposure", rownames(coeff_matrix)), ][, "Std. Error"]
      
      model_result_collector[[length(model_result_collector) + 1]] <- c(
        ci_vector,
        B_vector,
        se_vector,
        model_metadata
      )
    }
  }
  
  # convert to data.frame
  model_result_collector <- do.call(
    rbind,
    lapply(model_result_collector, function(x) data.frame(t(x), stringsAsFactors = FALSE))
  )
  
  # column names: rows, separator, columns
  row_names <- rownames(ci.exp(model_result, subset="lagged_exposure"))
  row_names <- gsub("lagged_exposure", "exposure", row_names)
  row_names <- gsub("lagged_interactive", "interactive", row_names)
  colnames(model_result_collector) <- c(
    paste0(c("rr", "cil", "ciu"), "_", rep(row_names, each=3)),
    paste0(c("B"), "_", rep(row_names, each=1)),
    paste0(c("se"), "_", rep(row_names, each=1)),
    "metadata"
  )

  # add column "city", "exposure", "outcome" to the results
  model_result_collector["city"] <- city  
  model_result_collector["exposure"] <- exposure
  model_result_collector[, "lag"] <- paste0(lags)
  model_result_collector["iqr"] <- IQR(lagged_exposure, na.rm = TRUE)
  model_result_collector["outcome"] <- outcome
  model_result_collector["quantile_type"] <- quantile_type

  # validate output
  if(!is.data.frame(model_result_collector)) stop(
    "Output 'model_result_collector' not 'data.frame type."
  )

  return(model_result_collector)
}

# for testing
TEST <- FALSE
if (TEST == TRUE) {
  root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
  quantile_type <- "bisection"
  data <- read.csv(file=paste0(root_dir, "/data/interactive_", quantile_type, ".csv"))
  exposure <- "spm"
  outcome <- "resp65"
  lags <- c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5")
  q <- 1
  k <- 1
}

metafor_noninteractive <- function(
  data,
  exposure,
  outcome,
  lags
) {
  # start by subsetting data
  data <- data[data["exposure"] == exposure & data["outcome"] == outcome, ]
  iqrm <- mean(data[, "iqr"])   # calculate iqr for each column related to "exposure" in data

  # initialize matrix for results
  matrix_columns <- c("B", "se", "I2", "p.Qtest", "rr", "cil", "ciu")
  results <- matrix(
    numeric(0), nrow = length(lags), ncol = length(matrix_columns),
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
    results[k, 1:4] <- c(meta_analysis$b, meta_analysis$se, meta_analysis$I2, meta_analysis$QEp)
    
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

metafor_bisection <- function(
  data,
  exposure,
  outcome,
  quantile_type,
  lags
) {
  # start by subsetting data
  data <- data[data["exposure"] == exposure & data["outcome"] == outcome & data["quantile_type"] == quantile_type, ]
  iqrm <- mean(data[, "iqr"])   # calculate iqr for each column related to "exposure" in data
  
  # initialize matrix for results
  quantile_names <- c("interactive1.exposure", "interactive2.exposure")
  matrix_columns <- c("B", "se", "I2", "p.Qtest", "rr", "cil", "ciu", "quantile")
  results <- matrix(
    numeric(0), nrow = (length(lags)) * length(quantile_names), ncol = length(matrix_columns),
    dimnames = list(NULL, matrix_columns)
  )
  
  # Loop over quantiles * lags
  for (q in 1:length(quantile_names)) {
    for (k in seq(lags)) {
      est_vector <- as.numeric(data[data["lag"] == lags[k], paste0("B_", quantile_names[q])])
      se_vector <- as.numeric(data[data["lag"] == lags[k], paste0("se_", quantile_names[q])])
      row_number <- k + (length(lags)) * (q - 1)  # Adjust row number for each quantile
      
      if (length(est_vector) > 0 && !any(is.na(est_vector)) && !any(is.na(se_vector))) {
        tryCatch({
          meta_analysis <- metafor::rma(
            yi = est_vector,
            sei = se_vector,
            method = "REML"
          )
          # Store results from meta-analysis in matrix
          results[row_number, 1:4] <- c(meta_analysis$b, meta_analysis$se, meta_analysis$I2, meta_analysis$QEp)
          results[row_number, "rr"] <- exp(meta_analysis$b * iqrm)
          results[row_number, "cil"] <- exp((meta_analysis$b - 1.96 * meta_analysis$se) * iqrm)
          results[row_number, "ciu"] <- exp((meta_analysis$b + 1.96 * meta_analysis$se) * iqrm)
          results[row_number, "quantile"] <- q
        }, error = function(e) {
          cat("Error processing lag", k, "quantile", q, ":", e$message, "\n")
          results[row_number, ] <- rep(NA, length(matrix_columns))
        })
      } else {
        cat("Data missing or NA for lag", k, "quantile", q - 1, "\n")
        results[row_number, ] <- rep(NA, length(matrix_columns))
      }
    }
  }

  # Convert matrix to data frame
  df_results <- as.data.frame(results)
  df_results$exposure <- exposure
  df_results$outcome <- outcome
  df_results$quantile_type <- quantile_type
  df_results$lag <- paste0(rep(lags, length(quantile_names))) 
  df_results$iqrm <- iqrm
  
  return(df_results)
}

metafor_tercile <- function(
  data,
  exposure,
  outcome,
  quantile_type,
  lags
) {
  # start by subsetting data
  data <- data[data["exposure"] == exposure & data["outcome"] == outcome & data["quantile_type"] == quantile_type, ]
  iqrm <- mean(data[, "iqr"])   # calculate iqr for each column related to "exposure" in data

  # initialize matrix for results
  quantile_names <- c("interactive1.exposure", "interactive2.exposure", "interactive3.exposure")
  matrix_columns <- c("B", "se", "I2", "p.Qtest", "rr", "cil", "ciu", "quantile")
  results <- matrix(
    numeric(0), nrow = (length(lags)) * length(quantile_names), ncol = length(matrix_columns),
    dimnames = list(NULL, matrix_columns)
  )

  # Loop over quantiles and lags
  for (q in 1:length(quantile_names)) {
    for (k in seq(lags)) {
      est_vector <- as.numeric(data[data["lag"] == lags[k], paste0("B_", quantile_names[q])])
      se_vector <- as.numeric(data[data["lag"] == lags[k], paste0("se_", quantile_names[q])])
      row_number <- k + (length(lags)) * (q - 1) # Adjust row number for each quantile

      if (length(est_vector) > 0 && !any(is.na(est_vector)) && !any(is.na(se_vector))) {
          tryCatch({
            meta_analysis <- metafor::rma(
            yi = est_vector,
            sei = se_vector,
            method = "REML"
          )
          # Store results from meta-analysis in matrix
          results[row_number, 1:4] <- c(meta_analysis$b, meta_analysis$se, meta_analysis$I2, meta_analysis$QEp)
          results[row_number, "rr"] <- exp(meta_analysis$b * iqrm)
          results[row_number, "cil"] <- exp((meta_analysis$b - 1.96 * meta_analysis$se) * iqrm)
          results[row_number, "ciu"] <- exp((meta_analysis$b + 1.96 * meta_analysis$se) * iqrm)
          results[row_number, "quantile"] <- q
        }, error = function(e) {
          cat("Error processing lag", k, "quantile", q, ":", e$message, "\n")
          results[row_number, ] <- rep(NA, length(matrix_columns))
        })
      } else {
        cat("Data missing or NA for lag", k, "quantile", q, "\n")
        results[row_number, ] <- rep(NA, length(matrix_columns))
      }
    }
  }

  # Convert matrix to data frame
  df_results <- as.data.frame(results)
  df_results$exposure <- exposure
  df_results$outcome <- outcome
  df_results$quantile_type <- quantile_type
  df_results$lag <- paste0(rep(lags, length(quantile_names)))
  df_results$iqrm <- iqrm

  return(df_results)
}

metafor_quartile <- function(
    data,
    exposure,
    outcome,
    quantile_type,
    lags
) {
  # start by subsetting data
  data <- data[data["exposure"] == exposure & data["outcome"] == outcome & data["quantile_type"] == quantile_type, ]
  iqrm <- mean(data[, "iqr"])   # calculate iqr for each column related to "exposure" in data
  
  # initialize matrix for results
  quantile_names <- c("interactive1.exposure", "interactive2.exposure", "interactive3.exposure", "interactive4.exposure")
  matrix_columns <- c("B", "se", "I2", "p.Qtest", "rr", "cil", "ciu", "quantile")
  results <- matrix(
    numeric(0), nrow = (length(lags)) * length(quantile_names), ncol = length(matrix_columns),
    dimnames = list(NULL, matrix_columns)
  )
  
  # Loop over quantiles and lags
  for (q in 1:length(quantile_names)) {
    for (k in seq(lags)) {
      est_vector <- as.numeric(data[data["lag"] == lags[k], paste0("B_", quantile_names[q])])
      se_vector <- as.numeric(data[data["lag"] == lags[k], paste0("se_", quantile_names[q])])
      row_number <- k + (length(lags)) * (q - 1)  # Adjust row number for each quantile
      
      if (length(est_vector) > 0 && !any(is.na(est_vector)) && !any(is.na(se_vector))) {
        tryCatch({
          meta_analysis <- metafor::rma(
            yi = est_vector,
            sei = se_vector,
            method = "REML"
          )
          # Store results from meta-analysis in matrix
          results[row_number, 1:4] <- c(meta_analysis$b, meta_analysis$se, meta_analysis$I2, meta_analysis$QEp)
          results[row_number, "rr"] <- exp(meta_analysis$b * iqrm)
          results[row_number, "cil"] <- exp((meta_analysis$b - 1.96 * meta_analysis$se) * iqrm)
          results[row_number, "ciu"] <- exp((meta_analysis$b + 1.96 * meta_analysis$se) * iqrm)
          results[row_number, "quantile"] <- q
        }, error = function(e) {
          cat("Error processing lag", k, "quantile", q, ":", e$message, "\n")
          results[row_number, ] <- rep(NA, length(matrix_columns))
        })
      } else {
        cat("Data missing or NA for lag", k, "quantile", q, "\n")
        results[row_number, ] <- rep(NA, length(matrix_columns))
      }
    }
  }
  
  # Convert matrix to data frame
  df_results <- as.data.frame(results)
  df_results$exposure <- exposure
  df_results$outcome <- outcome
  df_results$quantile_type <- quantile_type
  df_results$lag <- paste0(rep(lags, length(quantile_names)))
  df_results$iqrm <- iqrm
  
  return(df_results)
}
