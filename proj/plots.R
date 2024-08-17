
# setup -----------------------------------------------------------------------------------------------------------

rm(list = ls())  # reset
root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
source(paste0(root_dir, "/proj/functions.R"))

library(ggplot2)
library(ggtext)
library(cowplot)
library(dplyr)
library(raster)
library(maps)
library(mapdata)

# main ------------------------------------------------------------------------------------------------------------

# parameters
data_0 <- {
  read.csv(file = paste0(root_dir, "/data/metafor_noninteractive.csv"))
}
data_1 <- {
  list(
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_perc75.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_perc80.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_perc85.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_perc90.csv"
    ))
  )
}
data_2 <- {
  list(
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_abs20.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_abs40.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_abs60.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_abs80.csv"
    ))
  )
}

# figure 2 --------------------------------------------------------------------------------------------------------

point_color = "red"
point_shape = 18
a <- {
  create_noninteractive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data = data_0,
    point_color = point_color,
    point_shape = point_shape
  )
}
b <- {
  create_noninteractive_plot(
    exposure = "spm",
    outcome = "all65",
    lags = c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data = data_0,
    point_color = point_color,
    point_shape = point_shape
  )
}
c <- {
  create_noninteractive_plot(
    exposure = "spm",
    outcome = "resp",
    lags = c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data = data_0,
    point_color = point_color,
    point_shape = point_shape
  )
}
d <- {
  create_noninteractive_plot(
    exposure = "spm",
    outcome = "resp65",
    lags = c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data = data_0,
    point_color = point_color,
    point_shape = point_shape
  )
}
e <- {
  create_noninteractive_plot(
    exposure = "spm",
    outcome = "circ",
    lags = c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data = data_0,
    point_color = point_color,
    point_shape = point_shape
  )
}
f <- {
  create_noninteractive_plot(
    exposure = "spm",
    outcome = "circ65",
    lags = c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data = data_0,
    point_color = point_color,
    point_shape = point_shape
  )
}
plot2 <- {
  plot_grid(
    a,
    b,
    c,
    d,
    e,
    f,
    nrow = 3,
    ncol = 2,
    labels = c(
      "[A] All-cause",
      "[D] All-cause Aged 65+",
      "[B] Respiratory",
      "[E] Respiratory Aged 65+",
      "[C] Cardiovascular",
      "[F] Cardiovascular Aged 65+"
    ),
    label_x = 0.05,
    hjust = 0
  )
}
{
  ggsave(
    filename = paste0(root_dir, "/plots/figure2.tif"),
    plot = plot2,
    device = "tiff",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )
}


# figure 3 --------------------------------------------------------------------------------------------------------

a <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = c(75, 80, 85, 90),
    legend_label = "Pollen \nCutoff \n(%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple")
  )
}
b <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = c(20, 40, 60, 80),
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple")
  )
}
c <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = c(75, 80, 85, 90),
    legend_label = "Pollen \nCutoff (%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple")
  )
}
d <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = c(20, 40, 60, 80),
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple")
  )
}
e <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = c(75, 80, 85, 90),
    legend_label = "Pollen \nCutoff (%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple")
  )
}
f <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = c(20, 40, 60, 80),
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple")
  )
}
plot3 <- {
  plot_grid(
    a,
    b,
    c,
    d,
    e,
    f,
    nrow = 3,
    ncol = 2,
    labels = c(
      "[A] All-cause",
      "[D] All-cause",
      "[B] Respiratory",
      "[E] Respiratory",
      "[C] Cardiovascular",
      "[F] Cardiovascular"
    ),
    label_x = 0.05,
    hjust = 0
  )
}
{
  ggsave(
    filename = paste0(root_dir, "/plots/figure3.tif"),
    plot = plot3,
    device = "tiff",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )
}

# figure 4 --------------------------------------------------------------------------------------------------------

a <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all65",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = c(75, 80, 85, 90),
    legend_label = "Pollen \nCutoff \n(%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple")
  )
}
b <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all65",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = c(20, 40, 60, 80),
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple")
  )
}
c <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp65",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = c(75, 80, 85, 90),
    legend_label = "Pollen \nCutoff (%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple")
  )
}
d <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp65",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = c(20, 40, 60, 80),
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple")
  )
}
e <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ65",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = c(75, 80, 85, 90),
    legend_label = "Pollen \nCutoff (%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple")
  )
}
f <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ65",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = c(20, 40, 60, 80),
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple")
  )
}
plot4 <- {
  plot_grid(
    a,
    b,
    c,
    d,
    e,
    f,
    nrow = 3,
    ncol = 2,
    labels = c(
      "[A] All-cause Aged 65+",
      "[D] All-cause Aged 65+",
      "[B] Respiratory Aged 65+",
      "[E] Respiratory Aged 65+",
      "[C] Cardiovascular Aged 65+",
      "[F] Cardiovascular Aged 65+"
    ),
    label_x = 0.05,
    hjust = 0
  )
}
{ 
  ggsave(
    filename = paste0(root_dir, "/plots/figure4.tif"),
    plot = plot4,
    device = "tiff",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )
}

# figure 1 --------------------------------------------------------------------------------------------------------

root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
tiff(
  paste0(root_dir, "/plots/study_map.tif"),
  width = 3200,
  height = 1800,
  res = 300
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
