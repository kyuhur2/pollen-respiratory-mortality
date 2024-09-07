
library(ggplot2)
library(ggtext)
library(cowplot)
library(dplyr)
library(raster)
library(maps)
library(mapdata)

# setup -----------------------------------------------------------------------------------------------------------

rm(list = ls())  # reset
root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
source(paste0(root_dir, "/proj/functions.R"))

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
    ))
  )
}
data_2 <- {
  list(
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_abs25.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_abs50.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_abs75.csv"
    ))
  )
}

# no2 conf
data_3 <- {
  list(
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_no2_perc75.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_no2_perc80.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_no2_perc85.csv"
    ))
  )
}
data_4 <- {
  list(
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_no2_abs25.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_no2_abs50.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_no2_abs75.csv"
    ))
  )
}

# so2 conf
data_5 <- {
  list(
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_so2_perc75.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_so2_perc80.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_so2_perc85.csv"
    ))
  )
}
data_6 <- {
  list(
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_so2_abs25.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_so2_abs50.csv"
    )),
    read.csv(file = paste0(
      root_dir, "/data/metafor_bisection_so2_abs75.csv"
    ))
  )
}

# figure 1 --------------------------------------------------------------------------------------------------------

# moved to misc.R because code no longer works (one of the libraries is pulling data from a location that doesn't exist)

# figure 2 --------------------------------------------------------------------------------------------------------

# params
tmp <- data_0[data_0$exposure %in% c("spm") &
              data_0$outcome %in% c("all", "circ", "resp", "all65", "circ65", "resp65") &
                data_0$lag %in% c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"), ]
y_lower_bound <- min(tmp$cil, na.rm = TRUE)
y_upper_bound <- max(tmp$ciu, na.rm = TRUE)
point_color = "red"
point_shape = 18

# plots
a <- {
  create_noninteractive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data = data_0,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
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
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
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
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
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
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
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
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
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
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
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
    filename = paste0(root_dir, "/plots/figure2.pdf"),
    plot = plot2,
    device = "pdf",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )
}

# figure 3 --------------------------------------------------------------------------------------------------------

# params
tmp <- do.call(rbind, c(data_1, data_2))
tmp <- tmp[tmp$exposure %in% c("spm") &
             tmp$outcome %in% c("all", "circ", "resp") &
             tmp$lag %in% c("0", "1", "2"), ]
y_lower_bound <- min(tmp$cil, na.rm = TRUE)
y_upper_bound <- max(tmp$ciu, na.rm = TRUE)
cutoffs_vec_perc <- c(75, 80, 85)
cutoffs_vec_abs <- c(25, 50, 75)
debug <- FALSE

# plots
a <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff \n(%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
b <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
c <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff (%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
d <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
e <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff (%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
f <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
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
    filename = paste0(root_dir, "/plots/figure3.pdf"),
    plot = plot3,
    device = "pdf",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )
}

# figure S1 -------------------------------------------------------------------------------------------------------

# params
tmp <- do.call(rbind, c(data_1, data_2))
tmp <- tmp[tmp$exposure %in% c("spm") &
             tmp$outcome %in% c("all65", "circ65", "resp65") &
             tmp$lag %in% c("0", "1", "2"), ]
y_lower_bound <- min(tmp$cil, na.rm = TRUE)
y_upper_bound <- max(tmp$ciu, na.rm = TRUE)
cutoffs_vec_perc <- c(75, 80, 85)
cutoffs_vec_abs <- c(25, 50, 75)
debug <- FALSE

# plots
a <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all65",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff \n(%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
b <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all65",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
c <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp65",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff (%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
d <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp65",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
e <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ65",
    lags = c(0, 1, 2),
    data = data_1,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff (%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
f <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ65",
    lags = c(0, 1, 2),
    data = data_2,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
plotS1 <- {
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
      "[A] All-cause Ages 65 and Above",
      "[D] All-cause Ages 65 and Above",
      "[B] Respiratory Ages 65 and Above",
      "[E] Respiratory Ages 65 and Above",
      "[C] Cardiovascular Ages 65 and Above",
      "[F] Cardiovascular Ages 65 and Above"
    ),
    label_x = 0.05,
    hjust = 0
  )
}
{ 
  ggsave(
    filename = paste0(root_dir, "/plots/figureS1.pdf"),
    plot = plotS1,
    device = "pdf",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )
}

# figure S2 -------------------------------------------------------------------------------------------------------

tmp <- do.call(rbind, c(data_3, data_4))
tmp <- tmp[tmp$exposure %in% c("spm") &
             tmp$outcome %in% c("all", "circ", "resp") &
             tmp$lag %in% c("0", "1", "2"), ]
y_lower_bound <- min(tmp$cil, na.rm = TRUE)
y_upper_bound <- max(tmp$ciu, na.rm = TRUE) * 1.005
cutoffs_vec_perc <- c(75, 80, 85)
cutoffs_vec_abs <- c(25, 50, 75)
debug <- FALSE

# plots
a <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c(0, 1, 2),
    data = data_3,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff \n(%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
b <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c(0, 1, 2),
    data = data_4,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
c <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp65",
    lags = c(0, 1, 2),
    data = data_3,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff (%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
d <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp65",
    lags = c(0, 1, 2),
    data = data_4,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
e <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ65",
    lags = c(0, 1, 2),
    data = data_3,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff (%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
f <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ65",
    lags = c(0, 1, 2),
    data = data_4,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
plotS2 <- {
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
      "[A] All-cause (NO2 Confounding)",
      "[D] All-cause (NO2 Confounding)",
      "[B] Respiratory (NO2 Confounding)",
      "[E] Respiratory (NO2 Confounding)",
      "[C] Cardiovascular (NO2 Confounding)",
      "[F] Cardiovascular (NO2 Confounding)"
    ),
    label_x = 0.05,
    hjust = 0
  )
}
{ 
  ggsave(
    filename = paste0(root_dir, "/plots/figureS2.pdf"),
    plot = plotS2,
    device = "pdf",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )
}

# figure S3 -------------------------------------------------------------------------------------------------------

# params
tmp <- do.call(rbind, c(data_5, data_6))
tmp <- tmp[tmp$exposure %in% c("spm") &
             tmp$outcome %in% c("all", "circ", "resp") &
             tmp$lag %in% c("0", "1", "2"), ]
y_lower_bound <- min(tmp$cil, na.rm = TRUE)
y_upper_bound <- max(tmp$ciu, na.rm = TRUE)
cutoffs_vec_perc <- c(75, 80, 85)
cutoffs_vec_abs <- c(25, 50, 75)
debug <- FALSE

# plots
a <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c(0, 1, 2),
    data = data_5,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff \n(%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
b <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "all",
    lags = c(0, 1, 2),
    data = data_6,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
c <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp",
    lags = c(0, 1, 2),
    data = data_5,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff (%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
d <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "resp",
    lags = c(0, 1, 2),
    data = data_6,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
e <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ",
    lags = c(0, 1, 2),
    data = data_5,
    cutoffs_vec = cutoffs_vec_perc,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCutoff (%)",
    point_shapes = c(15, 16, 17, 18),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
f <- {
  create_interactive_plot(
    exposure = "spm",
    outcome = "circ",
    lags = c(0, 1, 2),
    data = data_6,
    cutoffs_vec = cutoffs_vec_abs,
    y_lower_bound = y_lower_bound,
    y_upper_bound = y_upper_bound,
    legend_label = "Pollen \nCount",
    point_shapes = c(0, 1, 2, 9),
    point_colors = c("red", "blue", "green", "purple"),
    debug = debug
  )
}
plotS3 <- {
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
      "[A] All-cause (SO2 Confounding)",
      "[D] All-cause (SO2 Confounding)",
      "[B] Respiratory (SO2 Confounding)",
      "[E] Respiratory (SO2 Confounding)",
      "[C] Cardiovascular (SO2 Confounding)",
      "[F] Cardiovascular (SO2 Confounding)"
    ),
    label_x = 0.05,
    hjust = 0
  )
}
{ 
  ggsave(
    filename = paste0(root_dir, "/plots/figureS3.pdf"),
    plot = plotS3,
    device = "pdf",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )
}
