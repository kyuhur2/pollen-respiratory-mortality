run_noninteractive_glm_model <- function(
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
  model_result_collector[, "exposure"] <- c(paste0(exposure, 0:lag_))
  
  # validate output
  if(!is.data.frame(model_result_collector)) stop(
    "Output 'model_result_collector' not 'data.frame type."
  )
  
  return(model_result_collector)
}

run_interactive_glm_model <- function(
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
    adjusted_lagged_exposure <- data[, paste0(exposure, k)] / 10. # adjusted by 10
    lagged_interactive <- data[, paste0(interactive, k)]

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
  model_result_collector[, "exposure"] <- c(paste0(exposure, 0:lag_))
  
  # validate output
  if(!is.data.frame(model_result_collector)) stop(
    "Output 'model_result_collector' not 'data.frame type."
  )
  
  return(model_result_collector)
}

