library(lubridate)
library(Epi)
library(tsModel)
library(splines)
library(mgcv)

# initialize project state
project_dir <- "/Users/kyuhur/Documents/Github/pollen_resp_mort"
source(paste(project_dir, "/code/modeling/functions.R", sep=""))  # import functions
sublist <- read.csv(file=paste(project_dir, "/code/data/sublist.csv", sep=""))
laglist <- read.csv(file=paste(project_dir, "/code/data/laglist.csv", sep=""))

# data transformations
sublist[, "date"] <- as.Date(sublist[, "date"])
laglist[, "date"] <- as.Date(laglist[, "date"])

# constants
CITIES <- c(
  "Fukuoka", "Kumamoto", "Nagasaki", "Oita", "Saga", "Kagoshima", "Miyazaki", "Kitakyushu"
)

# run glm model for non-interactive models
noninteractive_results <- list()
for (city in CITIES) {
  results <- run_noninteractive_glm_model(
    data=sublist[sublist["city"] == city, ],
    outcome="resp.age65",
    exposure="SPMout",
    lag_=7,
    asian_dust_days="ad",
    holiday="Holiday",
    day_of_week="dow",
    day_of_year="doy",
    year="year",
    temp_mean="Tave07",
    date="date",
    season_df=7,
    relative_humidity_mean="RHave"
  )
  
  results[, "city"] <- city
  noninteractive_results <- append(noninteractive_results, list(results))
}
noninteractive_results <- do.call(rbind, noninteractive_results)

# run glm model for interactive models
interactive_results <- list()
for (city in CITIES) {
  results <- run_interactive_glm_model(
    data=sublist[sublist["city"] == city, ],
    outcome="resp.age65",
    exposure="SPMout",
    interactive="SuHi",
    lag_=7,
    asian_dust_days="ad",
    holiday="Holiday",
    day_of_week="dow",
    day_of_year="doy",
    year="year",
    temp_mean="Tave07",
    date="date",
    season_df=7,
    relative_humidity_mean="RHave"
  )
  
  results[, "city"] <- city
  interactive_results <- append(interactive_results, list(results))
}
interactive_results <- do.call(rbind, interactive_results)
