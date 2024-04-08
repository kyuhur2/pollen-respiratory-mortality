run_noninteractive_glm_model <- function(
  city,
  data,
  outcome,
  exposure,
  lag_,
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
  if(!is.numeric(lag_)) stop("Input 'lag_' not 'nuemeric' type.")
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
  
  # run glm model for all exposure x lag_ pairs; collect data into model_result_collector
  for(k in 0:lag_){
    adjusted_lagged_exposure <- data[, paste0(exposure, k)] / 10  # adjusted by 10

    # run model
    model_result <- glm(
      data[, outcome] ~
        adjusted_lagged_exposure +
        factor(data[, asian_dust_days]) +
        factor(data[, holiday]) +
        data[, relative_humidity_mean] +
        data[, day_of_week] +
        ns(data[, day_of_year], df=season_df):factor(data[, year]) +
        ns(data[, temp_mean], df=3) +
        ns(data[, date], df=1),
      family=quasipoisson
    )
    
    # collect equation and variables
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
      ci.exp(model_result, subset="adjusted_lagged_exposure"),
      model_metadata
    )
  }
  
  # convert to data.frame
  model_result_collector <- do.call(
    rbind,
    lapply(model_result_collector, function(x) data.frame(t(x), stringsAsFactors = FALSE))
  )
  colnames(model_result_collector) <- c("rr", "cil", "ciu", "model_metadata")

  # add column "city", "exposure", "outcome" to the results
  model_result_collector["city"] <- city
  model_result_collector[, "exposure"] <- c(paste0(exposure, 0:lag_))
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
  lag_,
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
  if(!is.numeric(lag_)) stop("Input 'lag_' not 'nuemeric' type.")
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
  
  # run glm model for all exposure x lag_ pairs; collect data into model_result_collector
  for(k in 0:lag_){
    adjusted_lagged_exposure <- data[, paste0(exposure, k)] / 10  # adjusted by 10
    lagged_interactive <- factor(cut(data[, paste0(interactive, k)], breaks = 4, labels = FALSE))

    # run model
    model_result <- glm(
      data[, outcome] ~
        adjusted_lagged_exposure:lagged_interactive +
        factor(data[, asian_dust_days]) +
        factor(data[, holiday]) +
        data[, relative_humidity_mean] +
        data[, day_of_week] +
        ns(data[, day_of_year], df=season_df):factor(data[, year]) +
        ns(data[, temp_mean], df=3) +
        ns(data[, date], df=1),
      family=quasipoisson
    )
    
    # collect equation and variables
    model_metadata <- list(c(
      model = model_result["call"],
      outcome = outcome,
      exposure = exposure,
      k = k,
      interactive = interactive,
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
    
    # collect results based on IQR
    ci <- as.vector(t(ci.exp(model_result, subset="adjusted_lagged_exposure")))  # results
    
    model_result_collector[[length(model_result_collector) + 1]] <- c(
      ci,
      model_metadata
    )
  }
  
  # convert to data.frame
  model_result_collector <- do.call(
    rbind,
    lapply(model_result_collector, function(x) data.frame(t(x), stringsAsFactors = FALSE))
  )
  
  # column names: rows, separator, columns
  row_names <- rownames(ci.exp(model_result, subset="adjusted_lagged_exposure"))
  row_names <- gsub("adjusted_lagged_exposure", "exposure", row_names)
  row_names <- gsub("lagged_interactive", "interactive", row_names)
  colnames(model_result_collector) <- c(
    paste0(
      rep(row_names, each=3),
      "; ",
      c("rr", "cil", "ciu")
    ),
    "metadata"
  )

  # add column "city", "exposure", "outcome" to the results
  model_result_collector["city"] <- city  
  model_result_collector[, "exposure"] <- c(paste0(exposure, 0:lag_))
  model_result_collector["outcome"] <- outcome

  # validate output
  if(!is.data.frame(model_result_collector)) stop(
    "Output 'model_result_collector' not 'data.frame type."
  )
  
  return(model_result_collector)
}

meta <- function(
  data,
  lag_,
  exposure,
  coef
) {
  iqr <- sapply(data, function(x) IQR(x[, exposure], na.rm=TRUE))
  cmb <- matrix(
    numeric(),
    nrow=lag_,
    ncol=4,
    dimnames=list(lag_, c("est", "se", "I2", "p.Qtest"))
  )
  
  for (lag in seq(lag_)) {
    est <- sapply(coef, function(x) x[lag, "B"])
    se <- sapply(coef, function(x) x[lag, "se"])
    meta_ <- metafor::rma(yi=est, sei=se, data=cbind(est, se), method="REML")
    cmb[lag, ] <- c(meta$b, meta$se, meta$I2, meta$QEp)
  }
  
  iqrm <- mean(iqr)
  f <- data.frame(exposure, iqrm, cmb)
  f["rr"] <- exp(f$est * iqrm)
  f["cil"] <- exp((f$est - 1.96 * f$se) * iqrm)
  f["ciu"] <- exp((f$est + 1.96 * f$se) * iqrm)
  return(f)
}
