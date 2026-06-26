
tmp <- read.csv(paste0(root_dir, "/data/lagdata.csv"))

tmp <- tmp %>%
  filter(city %in% CITIES, month %in% c(2, 3, 4)) %>%
  mutate(city = factor(city, levels = CITIES))

# tmp table
table2_new <- bind_rows(
  make_row_mean_sd(tmp, SPMout, "SPM (μg/m3)", "Mean ± SD"),
  make_row_median_iqr(tmp, SPMout, "SPM (μg/m3)", "Median, IQR"),
  make_row_max(tmp, SPMout, "SPM (μg/m3)", "Max"),
  make_row_mean_sd(tmp, SuHiout, "Pollen count", "Mean ± SD"),
  make_row_median_iqr(tmp, SuHiout, "Pollen count", "Median, IQR"),
  make_row_percentile(tmp, SuHiout, 0.75, "Pollen count", "75"),
  make_row_percentile(tmp, SuHiout, 0.80, "Pollen count", "80"),
  make_row_percentile(tmp, SuHiout, 0.85, "Pollen count", "85"),
  make_row_max(tmp, SuHiout, "Pollen count", "Maximum"),
  make_row_mean_sd(tmp, SO2, "SO2 (ppb)", "Mean ± SD"),
  make_row_mean_sd(tmp, NO2, "NO2 (ppb)", "Mean ± SD"),
  make_row_mean_sd(tmp, Tave, "Mean Temperature (°C)", "Mean ± SD"),
  make_row_mean_sd(tmp, RHave, "Relative Humidity (%)", "Mean ± SD")
)

write.csv(table2_new, file = file.path(root_dir, "tables/table2.csv"), row.names = FALSE)

#
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
  levels <- c(
    "Low L0", "High L0",
    "Low L1", "High L1",
    "Low L2", "High L2"
  )

  # Combine cutoff-specific dataframes
  data_frames <- list()

  for (i in seq_along(data)) {
    tmp <- data[[i]]

    tmp <- tmp[
      tmp$exposure == exposure &
        tmp$outcome == outcome &
        tmp$lag %in% lags,
    ]

    tmp$cutoffs <- cutoffs_vec[i]
    data_frames[[i]] <- tmp
  }

  data <- bind_rows(data_frames)

  # X-axis levels
  x_axis_levels <- paste0(
    rep(cutoffs_vec, times = length(levels)),
    "Q",
    rep(rep(1:2, each = length(cutoffs_vec)), times = length(lags)),
    "L",
    rep(lags, each = length(cutoffs_vec) * 2)
  )

  data <- data %>%
    arrange(lag, quantile, cutoffs) %>%
    mutate(
      x_axis = factor(
        paste0(cutoffs, "Q", quantile, "L", lag),
        levels = x_axis_levels
      )
    )

  # Assign Low/High facet groups
  data$group <- factor(
    rep(levels, each = length(cutoffs_vec)),
    levels = levels
  )

  group_labeller <- ggplot2::as_labeller(c(
    "Low L0"  = "<b>Low</b>",
    "High L0" = "<b>High</b>",
    "Low L1"  = "<b>Low</b>",
    "High L1" = "<b>High</b>",
    "Low L2"  = "<b>Low</b>",
    "High L2" = "<b>High</b>"
  ))

  # One significance marker per Low-vs-High comparison
  star_offset <- 0.015 * (y_upper_bound - y_lower_bound)

  significance_data <- data %>%
    filter(
      as.character(quantile) == "2",
      !is.na(significance),
      significance != ""
    ) %>%
    mutate(star_y = ciu + star_offset)

  x <- ggplot(
    data,
    aes(
      x = x_axis,
      y = rr,
      shape = as.factor(cutoffs),
      color = as.factor(cutoffs)
    )
  ) +
    geom_errorbar(
      aes(ymin = cil, ymax = ciu),
      color = "black",
      width = 0.75
    ) +
    geom_hline(
      yintercept = 1,
      linetype = "dotted",
      color = "black"
    ) +
    geom_point(size = 2.5) +

    # Significance stars above the High confidence intervals
    geom_text(
      data = significance_data,
      aes(
        x = x_axis,
        y = star_y,
        label = significance
      ),
      inherit.aes = FALSE,
      size = 4,
      fontface = "bold",
      vjust = 0
    ) +

    labs(
      x = NULL,
      y = "RR",
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
      strip.text = element_markdown(),
      plot.margin = margin(5.5, 5.5, 16, 5.5)
    ) +
    scale_y_continuous(
      limits = c(
        y_lower_bound,
        round(y_upper_bound + 0.05 * (y_upper_bound - y_lower_bound), 2)
      ),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    facet_wrap(
      ~ group,
      scales = "free_x",
      nrow = 1,
      labeller = group_labeller
    ) +
    scale_shape_manual(values = point_shapes) +
    scale_color_manual(values = point_colors)

  # Lag labels below Low–High pairs
  xs <- c(0.23, 0.48, 0.73)
  lag_labels <- paste0("Lag ", lags)

  xg <- cowplot::ggdraw(x)

  for (i in seq_along(xs)) {
    xg <- xg +
      cowplot::draw_label(
        lag_labels[i],
        x = xs[i],
        y = 0.02,
        vjust = 0,
        fontface = "bold",
        size = 10
      )
  }

  return(xg)
}