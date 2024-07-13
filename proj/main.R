library(lubridate)
library(Epi)
library(tsModel)
library(splines)
library(mgcv)
library(dplyr)

# TODO: add supplementary table that shows the cutoffs for
# - quartiles
# - terciles
# - bisection
# for each city. 

# set up --------------------------------------------------------------------------------------

# initialize project state
root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
source(paste0(root_dir, "/proj/functions.R"))  # import functions

tmp <- read.csv(file=paste0(root_dir, "/data/fdata.csv"))
data <- read.csv(file=paste0(root_dir, "/data/data.csv"))
data[, "date"] <- as.Date(data[, "date"])  # transform date col to date type

# run glm model for non-interactive models ----------------------------------------------------

# set up data structures
noninteractive <- list()  # temp list to append data.frames
i <- 0
CITIES <- c("Fukuoka", "Kumamoto", "Nagasaki", "Oita", "Saga", "Kagoshima", "Miyazaki", "Kitakyushu")
OUTCOMES <- c("all", "circ", "resp", "all65", "circ65", "resp65")
EXPOSURES <- c("spm", "no2", "so2", "pol")
LAGS <- c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5")

# three for loops, creating a matrix of (outcomes * exposures * city)
for (outcome in OUTCOMES) {
  for (exposure in EXPOSURES) {
    intermediary <- list()
    for (city in CITIES) {
      i = i + 1
      cat(i, "/", length(OUTCOMES) * length(EXPOSURES) * length(CITIES), "(outcome, exposure, city):", outcome, exposure, city, "\n")
      results <- run_noninteractive_glm_model(
        city=city,
        data=data[data["city"] == city, ],
        outcome=outcome,
        exposure=exposure,
        lags=LAGS,
        asian_dust_days="ad",
        holiday="holiday",
        day_of_week="dow",
        day_of_year="doy",
        year="year",
        temp_mean="tave07",
        date="date",
        season_df=4,
        relative_humidity_mean="rhave"
      )

      results[, "city"] <- city
      intermediary <- append(intermediary, list(results))
    }
    noninteractive <- append(noninteractive, list(do.call(rbind, intermediary)))
  }
}
noninteractive <- do.call(rbind, noninteractive)  # collapsing list into data.frame

# run glm model for interactive models --------------------------------------------------------

# temp list to append data.frames
interactive_quartile <- list()
interactive_tercile <- list()
interactive_bisection <- list()

# three for loops, creating a matrix of (outcomes * exposures * city)
i <- 0
EXPOSURES <- c("spm", "no2", "so2")  # rewrite exposures, drop "pol" as it's interactive
for (outcome in OUTCOMES) {
  for (exposure in EXPOSURES) {
    intermediary <- list()
    for (city in CITIES) {
      i = i + 1
      cat(i, "/", length(OUTCOMES) * length(EXPOSURES) * length(CITIES), "(outcome, exposure, city):", outcome, exposure, city, "\n")
      results <- run_interactive_glm_model(
        city=city,
        data=data[data["city"] == city, ],
        outcome=outcome,
        exposure=exposure,
        interactive="pol",
        quantile_type="quartile",
        lags=LAGS,
        asian_dust_days="ad",
        holiday="holiday",
        day_of_week="dow",
        day_of_year="doy",
        year="year",
        temp_mean="tave07",
        date="date",
        season_df=4,
        relative_humidity_mean="rhave"
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
    interactive_quartile <- append(interactive_quartile, list(do.call(rbind, intermediary)))
  }
}
interactive_quartile <- do.call(rbind, interactive_quartile)  # collapsing list into data.frame

i <- 0
for (outcome in OUTCOMES) {
  for (exposure in EXPOSURES) {
    intermediary <- list()
    for (city in CITIES) {
      i = i + 1
      cat(i, "/", length(OUTCOMES) * length(EXPOSURES) * length(CITIES), "(outcome, exposure, city):", outcome, exposure, city, "\n")
      results <- run_interactive_glm_model(
        city=city,
        data=data[data["city"] == city, ],
        outcome=outcome,
        exposure=exposure,
        interactive="pol",
        quantile_type="tercile",
        lags=LAGS,
        asian_dust_days="ad",
        holiday="holiday",
        day_of_week="dow",
        day_of_year="doy",
        year="year",
        temp_mean="tave07",
        date="date",
        season_df=4,
        relative_humidity_mean="rhave"
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
    interactive_tercile <- append(interactive_tercile, list(do.call(rbind, intermediary)))
  }
}
interactive_tercile <- do.call(rbind, interactive_tercile)  # collapsing list into data.frame

i <- 0
for (outcome in OUTCOMES) {
  for (exposure in EXPOSURES) {
    intermediary <- list()
    for (city in CITIES) {
      i = i + 1
      cat(i, "/", length(OUTCOMES) * length(EXPOSURES) * length(CITIES), "(outcome, exposure, city):", outcome, exposure, city, "\n")
      results <- run_interactive_glm_model(
        city=city,
        data=data[data["city"] == city, ],
        outcome=outcome,
        exposure=exposure,
        interactive="pol",
        quantile_type="bisection",
        lags=LAGS,
        asian_dust_days="ad",
        holiday="holiday",
        day_of_week="dow",
        day_of_year="doy",
        year="year",
        temp_mean="tave07",
        date="date",
        season_df=4,
        relative_humidity_mean="rhave"
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
    interactive_bisection <- append(interactive_bisection, list(do.call(rbind, intermediary)))
  }
}
interactive_bisection <- do.call(rbind, interactive_bisection)  # collapsing list into data.frame

# checkpoint 1 --------------------------------------------------------------------------------

write.csv(noninteractive, paste0(root_dir, "/data/noninteractive.csv"), row.names=FALSE)
write.csv(interactive_quartile, paste0(root_dir, "/data/interactive_quartile.csv"), row.names=FALSE)
write.csv(interactive_tercile, paste0(root_dir, "/data/interactive_tercile.csv"), row.names=FALSE)
write.csv(interactive_bisection, paste0(root_dir, "/data/interactive_bisection.csv"), row.names=FALSE)

rm(list = ls())  # reset
root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
source(paste0(root_dir, "/proj/functions.R")) 
noninteractive <- read.csv(file=paste0(root_dir, "/data/noninteractive.csv"))
interactive_bisection <- read.csv(file=paste0(root_dir, "/data/interactive_bisection.csv"))
interactive_tercile <- read.csv(file=paste0(root_dir, "/data/interactive_tercile.csv"))
interactive_quartile <- read.csv(file=paste0(root_dir, "/data/interactive_quartile.csv"))

# metafor: combine noniteractive results ------------------------------------------------------

library(mixmeta)
library(metafor)
library(data.table)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

# constants
CITIES <- c("Fukuoka", "Kumamoto", "Nagasaki", "Oita", "Saga", "Kagoshima", "Miyazaki", "Kitakyushu")
OUTCOMES <- c("all", "circ", "resp", "all65", "circ65", "resp65")
EXPOSURES <- c("spm", "no2", "so2")  # no "pol" here as it's interactive
LAGS <- c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5")

# metafor: combine interactive results --------------------------------------------------------

i <- 0
metafor_noninteractive_data <- list()
for (outcome in OUTCOMES) {
  for (exposure in  c("spm", "no2", "so2", "pol")) {
    i = i + 1
    results <- metafor_noninteractive(
      data=noninteractive,
      exposure=exposure,
      outcome=outcome,
      lags=LAGS
    )
    metafor_noninteractive_data <- append(metafor_noninteractive_data, list(results))
  }
}
metafor_noninteractive_data <- do.call(rbind, metafor_noninteractive_data) 

# for bisection
i <- 0
metafor_bisection_data <- list()
for (outcome in OUTCOMES) {
  for (exposure in EXPOSURES) {
    i = i + 1
    results <- metafor_bisection(
      data=interactive_bisection,
      exposure=exposure,
      outcome=outcome,
      quantile_type="bisection",
      lags=LAGS
    )
    metafor_bisection_data <- append(metafor_bisection_data, list(results))
  }
}
metafor_bisection_data <- do.call(rbind, metafor_bisection_data) 

# for tercile
i <- 0
metafor_tercile_data <- list()
for (outcome in OUTCOMES) {
  for (exposure in EXPOSURES) {
    i = i + 1
    results <- metafor_tercile(
      data=interactive_tercile,
      exposure=exposure,
      outcome=outcome,
      quantile_type="tercile",
      lags=LAGS
    )
    metafor_tercile_data <- append(metafor_tercile_data, list(results))
  }
}
metafor_tercile_data <- do.call(rbind, metafor_tercile_data) 

# for quartile
i <- 0
metafor_quartile_data <- list()
for (outcome in OUTCOMES) {
  for (exposure in EXPOSURES) {
    i = i + 1
    results <- metafor_quartile(
      data=interactive_quartile,
      exposure=exposure,
      outcome=outcome,
      quantile_type="quartile",
      lags=LAGS
    )
    metafor_quartile_data <- append(metafor_quartile_data, list(results))
  }
}
metafor_quartile_data <- do.call(rbind, metafor_quartile_data) 

# TODO: check only one city--e.g. Fukuoka--and see if I get the same estimates.

# checkpoint 2 --------------------------------------------------------------------------------

write.csv(metafor_noninteractive_data, paste0(root_dir, "/data/metafor_noninteractive_data.csv"), row.names=FALSE)
write.csv(metafor_bisection_data, paste0(root_dir, "/data/metafor_bisection_data.csv"), row.names=FALSE)
write.csv(metafor_tercile_data, paste0(root_dir, "/data/metafor_tercile_data.csv"), row.names=FALSE)
write.csv(metafor_quartile_data, paste0(root_dir, "/data/metafor_quartile_data.csv"), row.names=FALSE)

rm(list=ls())  # reset
root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
source(paste0(root_dir, "/proj/functions.R")) 
metafor_noninteractive_data <- read.csv(file=paste0(root_dir, "/data/metafor_noninteractive_data.csv"))
metafor_bisection_data <- read.csv(file=paste0(root_dir, "/data/metafor_bisection_data.csv"))
metafor_tercile_data <- read.csv(file=paste0(root_dir, "/data/metafor_tercile_data.csv"))
metafor_quartile_data <- read.csv(file=paste0(root_dir, "/data/metafor_quartile_data.csv"))

# non-interactive plot ------------------------------------------------------------------------

library("ggplot2")

# non-interactive
exposure <- "so2"
outcome <- "circ"
lags <- c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5")
dot_color <- "red"
data <- metafor_noninteractive_data[
  metafor_noninteractive_data["exposure"] == exposure &
    metafor_noninteractive_data["outcome"] == outcome &
    metafor_noninteractive_data$lag %in% lags,
]

ggplot(data, aes(x = lag, y = rr)) +
  geom_point(color = dot_color) +
  geom_errorbar(aes(ymin = cil, ymax = ciu), color = "black", width = 0.2) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray") +
  labs(x = "Lag", y = "Relative Risk (RR)", title = paste("Noninteractive RR across Lag 0-5, ma1-ma5 for", exposure, outcome)) +
  theme_classic() # theme_light(), theme_dark(), theme_gray()

# interactive
exposure <- "spm"
outcome <- "circ"
lags <- c(0, 1, 2)
dot_color <- "blue"
#data <- metafor_quartile_data[
#  metafor_quartile_data["exposure"] == exposure &
#    metafor_quartile_data["outcome"] == outcome &
#    metafor_quartile_data$lag %in% lags,
#]
data <- metafor_bisection_data[
  metafor_bisection_data["exposure"] == exposure &
    metafor_bisection_data["outcome"] == outcome &
    metafor_bisection_data$lag %in% lags,
]

ggplot(data, aes(x = interaction(quantile, lag), y = rr)) +
  geom_point(color = dot_color) +
  geom_errorbar(aes(ymin = cil, ymax = ciu), color = "black", width = 0.2) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "gray") +
  labs(x = "Quantile and Lag", y = "Relative Risk (RR)", title =  paste("Interactive RR across Lag 0-2 for", exposure, outcome)) +
  theme_classic() +
  scale_x_discrete(labels = function(x) paste("Q", gsub("\\..*", "", as.integer(x)), "\nLag", gsub(".*\\.", "", x)))

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

# relative pollen cutoff 75%, 80%, 85%, 90%

# absolute pollen cutoff 20, 40, 60, 80

