library(lubridate)
library(Epi)
library(tsModel)
library(splines)
library(mgcv)

# initialize project state
root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
source(paste0(root_dir, "/proj/functions.R"))  # import functions

fdata <- read.csv(file=paste0(root_dir, "/data/fdata.csv"))
subdata <- read.csv(file=paste0(root_dir, "/data/subdata.csv"))
lagdata <- read.csv(file=paste0(root_dir, "/data/lagdata.csv"))

# data transformations
fdata[, "SuHi0"] <- fdata[, "SuHi"]
fdata[, "date"] <- as.Date(fdata[, "date"])
subdata[, "SuHi0"] <- subdata[, "SuHi"]
subdata[, "date"] <- as.Date(subdata[, "date"])
lagdata[, "SuHi0"] <- lagdata[, "SuHi"]
lagdata[, "date"] <- as.Date(lagdata[, "date"])

# constants
CITIES <- c(
  "Fukuoka", "Kumamoto", "Nagasaki", "Oita", "Saga", "Kagoshima", "Miyazaki", "Kitakyushu"
)

# run glm model for non-interactive models ----------------------------------------------------

noninteractive <- list()
intermediary <- list()

for (city in CITIES) {
  results <- run_noninteractive_glm_model(
    city=city,
    data=subdata[subdata["city"] == city, ],
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
    season_df=4,
    relative_humidity_mean="RHave"
  )
  
  results[, "city"] <- city
  intermediary <- append(intermediary, list(results))
}
noninteractive <- append(noninteractive, list(do.call(rbind, intermediary)))

intermediary <- list()
for (city in CITIES) {
  results <- run_noninteractive_glm_model(
    city=city,
    data=subdata[subdata["city"] == city, ],
    outcome="resp.age65",
    exposure="NO2out",
    lag_=7,
    asian_dust_days="ad",
    holiday="Holiday",
    day_of_week="dow",
    day_of_year="doy",
    year="year",
    temp_mean="Tave07",
    date="date",
    season_df=4,
    relative_humidity_mean="RHave"
  )
  
  results[, "city"] <- city
  intermediary <- append(intermediary, list(results))
}
noninteractive <- append(noninteractive, list(do.call(rbind, intermediary)))

intermediary <- list()
for (city in CITIES) {
  results <- run_noninteractive_glm_model(
    city=city,
    data=subdata[subdata["city"] == city, ],
    outcome="resp.age65",
    exposure="SO2out",
    lag_=7,
    asian_dust_days="ad",
    holiday="Holiday",
    day_of_week="dow",
    day_of_year="doy",
    year="year",
    temp_mean="Tave07",
    date="date",
    season_df=4,
    relative_humidity_mean="RHave"
  )
  
  results[, "city"] <- city
  intermediary <- append(intermediary, list(results))
}
noninteractive <- append(noninteractive, list(do.call(rbind, intermediary)))

intermediary <- list()
for (city in CITIES) {
  results <- run_noninteractive_glm_model(
    city=city,
    data=subdata[subdata["city"] == city, ],
    outcome="resp.age65",
    exposure="SuHiout",
    lag_=7,
    asian_dust_days="ad",
    holiday="Holiday",
    day_of_week="dow",
    day_of_year="doy",
    year="year",
    temp_mean="Tave07",
    date="date",
    season_df=4,
    relative_humidity_mean="RHave"
  )
  
  results[, "city"] <- city
  intermediary <- append(intermediary, list(results))
}
noninteractive <- append(noninteractive, list(do.call(rbind, intermediary)))

# collapse noninteractive 
noninteractive <- do.call(rbind, noninteractive)

# run glm model for interactive models --------------------------------------------------------

interactive <- list()
intermediary <- list()
for (city in CITIES) {
  results <- run_interactive_glm_model(
    city=city,
    data=subdata[subdata["city"] == city, ],
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
    season_df=4,
    relative_humidity_mean="RHave"
  )
  
  results[, "city"] <- city
  intermediary <- append(intermediary, list(results))
}

all_columns <- unique(unlist(lapply(intermediary, names)))  # identify unique column names
intermediary <- lapply(intermediary, function(df) {
  missing_columns <- setdiff(all_columns, names(df))
  if (length(missing_columns) > 0) { df[missing_columns] <- NA }

  return(df[, all_columns])  # ensure consistent column order across all data frames
})
interactive <- append(interactive, list(do.call(rbind, intermediary)))

intermediary <- list()
for (city in CITIES) {
  results <- run_interactive_glm_model(
    city=city,
    data=subdata[subdata["city"] == city, ],
    outcome="resp.age65",
    exposure="SO2out",
    interactive="SuHi",
    lag_=7,
    asian_dust_days="ad",
    holiday="Holiday",
    day_of_week="dow",
    day_of_year="doy",
    year="year",
    temp_mean="Tave07",
    date="date",
    season_df=4,
    relative_humidity_mean="RHave"
  )
  
  results[, "city"] <- city
  intermediary <- append(intermediary, list(results))
}

all_columns <- unique(unlist(lapply(intermediary, names)))  # identify unique column names
intermediary <- lapply(intermediary, function(df) {
  missing_columns <- setdiff(all_columns, names(df))
  if (length(missing_columns) > 0) { df[missing_columns] <- NA }
  
  return(df[, all_columns])  # ensure consistent column order across all data frames
})
interactive <- append(interactive, list(do.call(rbind, intermediary)))


intermediary <- list()
for (city in CITIES) {
  results <- run_interactive_glm_model(
    city=city,
    data=subdata[subdata["city"] == city, ],
    outcome="resp.age65",
    exposure="NO2out",
    interactive="SuHi",
    lag_=7,
    asian_dust_days="ad",
    holiday="Holiday",
    day_of_week="dow",
    day_of_year="doy",
    year="year",
    temp_mean="Tave07",
    date="date",
    season_df=4,
    relative_humidity_mean="RHave"
  )
  
  results[, "city"] <- city
  intermediary <- append(intermediary, list(results))
}

all_columns <- unique(unlist(lapply(intermediary, names)))  # identify unique column names
intermediary <- lapply(intermediary, function(df) {
  missing_columns <- setdiff(all_columns, names(df))
  if (length(missing_columns) > 0) { df[missing_columns] <- NA }
  
  return(df[, all_columns])  # ensure consistent column order across all data frames
})
interactive <- append(interactive, list(do.call(rbind, intermediary)))

# collapse interactive 
interactive <- do.call(rbind, interactive)

# checkpoint 1 --------------------------------------------------------------------------------

write.csv(noninteractive, paste0(root_dir, "/data/noninteractive.csv"))
write.csv(interactive, paste0(root_dir, "/data/interactive.csv"))

rm(list=ls())
root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
noninteractive <- read.csv(file=paste0(root_dir, "/data/noninteractive.csv"))
interactive <- read.csv(file=paste0(root_dir, "/data/interactive.csv"))

# metafor: combine non-iteractive results -----------------------------------------------------

mixmeta?

library(metafor)
library(data.table)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

# constants
CITIES <- c(
  "Fukuoka", "Kumamoto", "Nagasaki", "Oita", "Saga", "Kagoshima", "Miyazaki", "Kitakyushu"
)

# metafor: combine interactive results --------------------------------------------------------



# checkpoint 2 --------------------------------------------------------------------------------



# non-interactive plot ------------------------------------------------------------------------



# interactive plot ----------------------------------------------------------------------------



# study area map ------------------------------------------------------------------------------

library(raster)
library(maps)
library(mapdata)
library(ggplot2)

root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
tiff(paste0(root_dir,"/plots/study_map.tif"), width=3200, height=1800, res=300)
par(mfrow=c(1,2),mar=c(4,5,0.1,0.1))
japan_geodata <- getData("GADM",country="JPN",level=1)

xdegrees = seq(129, 132, 1)
ydegrees = seq(31, 34, 1)
xdegrees_ = sapply(xdegrees, function(x) bquote(.(x) * degree ~ E))
ydegrees_ = sapply(ydegrees, function(x) bquote(.(x) * degree ~ N))

# city plot
cexcity <- 1.5
pchcity <- c(seq(7, 10, 1), seq(21, 24, 1))
colcity <- rep(c(1, 2, 3, 4), 2)

plot(
  japan_geodata,
  xlim=c(130, 131),
  ylim=c(30.75, 34.25),
  xlab="Latitude",
  col="white",
  border="gray50",
  axes=F,
  las=1,
  bty="n",
  box=F
)
axis(1, at=xdegrees, labels=do.call(expression, xdegrees_))
axis(2, at=ydegrees, labels=do.call(expression, ydegrees_), las=1)
mtext(side = 2, text = "Longitude", line = 3.5)

city_codes <- read.csv(paste(root_dir, "/data/city_geocodes.csv", sep=""))
CITIES <- c("Fukuoka","Kumamoto","Nagasaki","Oita","Saga","Kagoshima","Miyazaki","Kitakyushu")

for(i in seq(nrow(city_codes))){
  points(
    city_codes[i,3],
    city_codes[i,2],
    col=colcity[i],
    cex=cexcity,
    pch=pchcity[i]
  )
}

legend("bottomright", CITIES, pch=pchcity, col=colcity, cex=.8, bty='n')

# Plot 2 - Measurement stations and clinics plot

cexval <- 1.2
pch1 <- 17
pch2 <- 7
col1 <- 4
col2 <- 2

plot(
  japan_geodata,
  xlim=c(130, 131),
  ylim=c(30.75, 34.25),
  xlab="Latitude",
  col="white",
  border="gray50",
  axes=F,
  las=1,
  bty="n",
  box=F
)
axis(1, at=xdegrees, labels=do.call(expression, xdegrees_))
axis(2, at=ydegrees, labels=do.call(expression, ydegress_), las=1)
mtext(side = 2, text = "Longitude", line = 3.5)

airpcodes <- read.csv(paste(root_dir,"/data/airp_station_geocodes.csv", sep=""))
airpcodes <- airpcodes[,c(1:2,5:6)]

for(i in seq(nrow(airpcodes))){
  points(
    airpcodes[i,4],
    airpcodes[i,3],
    col=col1,
    cex=cexval,
    pch=pch1
  )
}

pollencodes <- read.csv(paste(root_dir,"/data/clinic_geocodes.csv", sep=""))
pollencodes <- pollencodes[, c(1, 4:5)]
names(pollencodes) <- c("clinicid", "latitude", 'longitude')
pollencodes <- pollencodes[pollencodes$clinicid %in% c(
  2, 3, 4, 8, 11, 17, 23, 25, 27, 28, 36, 38, 44, 49, 54, 59
), ]

for(i in seq(nrow(pollencodes))){
  points(
    pollencodes[i,3],
    pollencodes[i,2],
    col=col2,
    cex=cexval,
    pch=pch2
  )
}

legend(
  "bottomright",
  c("Pollen Clinics", "Air Pollution \nStations"), pch=c(pch1, pch2), col=c(col1, col2), cex=.8, bty='n'
)

dev.off()

