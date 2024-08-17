
# setup -------------------------------------------------------------------

rm(list=ls())  # reset
root_dir <- "/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"
source(paste0(root_dir, "/proj/functions.R")) 

library(ggplot2)
library(ggtext)
library(cowplot)
library(dplyr)

# main --------------------------------------------------------------------

# parameters
data_0 <- {
  read.csv(file=paste0(root_dir, "/data/metafor_noninteractive.csv"))
}
data_1 <- {
  list(
    read.csv(file=paste0(root_dir, "/data/metafor_bisection_pol75.csv")),
    read.csv(file=paste0(root_dir, "/data/metafor_bisection_pol80.csv")),
    read.csv(file=paste0(root_dir, "/data/metafor_bisection_pol85.csv")),
    read.csv(file=paste0(root_dir, "/data/metafor_bisection_pol90.csv"))
  )
}
data_2 <- {
  list(
    read.csv(file=paste0(root_dir, "/data/metafor_bisection_abs20.csv")),
    read.csv(file=paste0(root_dir, "/data/metafor_bisection_abs40.csv")),
    read.csv(file=paste0(root_dir, "/data/metafor_bisection_abs60.csv")),
    read.csv(file=paste0(root_dir, "/data/metafor_bisection_abs80.csv"))
  )
}

# figure 2
point_color="red"
point_shape=18
a <- {
  create_noninteractive_plot(
    exposure="spm",
    outcome="all",
    lags=c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data=data_0,
    point_color=point_color,
    point_shape=point_shape
  )
}
b <- {
  create_noninteractive_plot(
    exposure="spm",
    outcome="all65",
    lags=c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data=data_0,
    point_color=point_color,
    point_shape=point_shape
  )
}
c <- {
  create_noninteractive_plot(
    exposure="spm",
    outcome="resp",
    lags=c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data=data_0,
    point_color=point_color,
    point_shape=point_shape
  )
}
d <- {
  create_noninteractive_plot(
    exposure="spm",
    outcome="resp65",
    lags=c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data=data_0,
    point_color=point_color,
    point_shape=point_shape
  )
}
e <- {
  create_noninteractive_plot(
    exposure="spm",
    outcome="circ",
    lags=c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data=data_0,
    point_color=point_color,
    point_shape=point_shape
  )
}
f <- {
  create_noninteractive_plot(
    exposure="spm",
    outcome="circ65",
    lags=c("0", "1", "2", "3", "4", "5", "ma1", "ma2", "ma3", "ma4", "ma5"),
    data=data_0,
    point_color=point_color,
    point_shape=point_shape
  )
}
plot2 <- {
  plot_grid(
    a, b, c, d, e, f,
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
ggsave(
  filename = paste0(root_dir, "/plots/figure2.tif"),
  plot = plot2,
  device = "tiff",
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)

# figure 3
a <- {
  create_interactive_plot(
    exposure="spm",
    outcome="all",
    lags=c(0, 1, 2),
    data=data_1,
    cutoffs_vec=c(75, 80, 85, 90),
    legend_label="Pollen \nCutoff \n(%)",
    point_shapes=c(15, 16, 17, 18),
    point_colors=c("red", "blue", "green", "purple")
  )
}
b <- {
  create_interactive_plot(
    exposure="spm",
    outcome="all",
    lags=c(0, 1, 2),
    data=data_2,
    cutoffs_vec=c(20, 40, 60, 80),
    legend_label="Pollen \nCount",
    point_shapes=c(0, 1, 2, 9),
    point_colors=c("red", "blue", "green", "purple")
  )
}
c <- {
  create_interactive_plot(
    exposure="spm",
    outcome="resp",
    lags=c(0, 1, 2),
    data=data_1,
    cutoffs_vec=c(75, 80, 85, 90),
    legend_label="Pollen \nCutoff (%)",
    point_shapes=c(15, 16, 17, 18),
    point_colors=c("red", "blue", "green", "purple")
  )
}
d <- { 
  create_interactive_plot(
    exposure="spm",
    outcome="resp",
    lags=c(0, 1, 2),
    data=data_2,
    cutoffs_vec=c(20, 40, 60, 80),
    legend_label="Pollen \nCount",
    point_shapes=c(0, 1, 2, 9),
    point_colors=c("red", "blue", "green", "purple")
  )
}
e <- { 
  create_interactive_plot(
    exposure="spm",
    outcome="circ",
    lags=c(0, 1, 2),
    data=data_1,
    cutoffs_vec=c(75, 80, 85, 90),
    legend_label="Pollen \nCutoff (%)",
    point_shapes=c(15, 16, 17, 18),
    point_colors=c("red", "blue", "green", "purple")
  )
}
f <- {
  create_interactive_plot(
    exposure="spm",
    outcome="circ",
    lags=c(0, 1, 2),
    data=data_2,
    cutoffs_vec=c(20, 40, 60, 80),
    legend_label="Pollen \nCount",
    point_shapes=c(0, 1, 2, 9),
    point_colors=c("red", "blue", "green", "purple")
  )
}
plot3 <- {
  plot_grid(
    a, b, c, d, e, f,
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
ggsave(
  filename = paste0(root_dir, "/plots/figure3.tif"),
  plot = plot3,
  device = "tiff",
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)

# figure 4
a <- {
  create_interactive_plot(
    exposure="spm",
    outcome="all65",
    lags=c(0, 1, 2),
    data=data_1,
    cutoffs_vec=c(75, 80, 85, 90),
    legend_label="Pollen \nCutoff \n(%)",
    point_shapes=c(15, 16, 17, 18),
    point_colors=c("red", "blue", "green", "purple")
  )
}
b <- {
  create_interactive_plot(
    exposure="spm",
    outcome="all65",
    lags=c(0, 1, 2),
    data=data_2,
    cutoffs_vec=c(20, 40, 60, 80),
    legend_label="Pollen \nCount",
    point_shapes=c(0, 1, 2, 9),
    point_colors=c("red", "blue", "green", "purple")
  )
}
c <- {
  create_interactive_plot(
    exposure="spm",
    outcome="resp65",
    lags=c(0, 1, 2),
    data=data_1,
    cutoffs_vec=c(75, 80, 85, 90),
    legend_label="Pollen \nCutoff (%)",
    point_shapes=c(15, 16, 17, 18),
    point_colors=c("red", "blue", "green", "purple")
  )
}
d <- { 
  create_interactive_plot(
    exposure="spm",
    outcome="resp65",
    lags=c(0, 1, 2),
    data=data_2,
    cutoffs_vec=c(20, 40, 60, 80),
    legend_label="Pollen \nCount",
    point_shapes=c(0, 1, 2, 9),
    point_colors=c("red", "blue", "green", "purple")
  )
}
e <- { 
  create_interactive_plot(
    exposure="spm",
    outcome="circ65",
    lags=c(0, 1, 2),
    data=data_1,
    cutoffs_vec=c(75, 80, 85, 90),
    legend_label="Pollen \nCutoff (%)",
    point_shapes=c(15, 16, 17, 18),
    point_colors=c("red", "blue", "green", "purple")
  )
}
f <- {
  create_interactive_plot(
    exposure="spm",
    outcome="circ65",
    lags=c(0, 1, 2),
    data=data_2,
    cutoffs_vec=c(20, 40, 60, 80),
    legend_label="Pollen \nCount",
    point_shapes=c(0, 1, 2, 9),
    point_colors=c("red", "blue", "green", "purple")
  )
}
plot4 <- {
  plot_grid(
    a, b, c, d, e, f,
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
ggsave(
  filename = paste0(root_dir, "/plots/figure4.tif"),
  plot = plot4,
  device = "tiff",
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)
