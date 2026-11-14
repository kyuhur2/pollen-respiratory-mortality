
rm(list = ls())  # reset

library(ggrepel)
library(ggspatial)
library(sf)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(readr)
library(lwgeom)

# Setup -------------------------------------------------------------------

os <- Sys.info()[["sysname"]]
if ("Darwin" %in% os) {
  root_dir <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality/vegetation-plot"
} else {
  root_dir <- "C:/Users/kyuhu/OneDrive/Documents/Github/pollen-respiratory-mortality/vegetation-plot"
}

fig_dir <- file.path(root_dir, "figures")
out_dir <- file.path(root_dir, "output")

dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Kyushu prefecture codes used in the vegetation GIS folders
pref_lookup <- tibble::tibble(
  pref_code = c("40", "41", "42", "43", "44", "45", "46"),
  pref_name = c(
    "Fukuoka",
    "Saga",
    "Nagasaki",
    "Kumamoto",
    "Oita",
    "Miyazaki",
    "Kagoshima"
  )
)

# Main integrated vegetation polygon file for each prefecture
get_main_shp <- function(pref_code) {
  file.path(
    root_dir,
    "vegetation-data/",
    paste0("vg_", pref_code),
    paste0("vg_", pref_code),
    paste0("vg_", pref_code, "a.shp")
  )
}

SUHI_RES = 25
OTHER_VEG_RES = 25

# Read vegetation shapefiles ----------------------------------------------

read_vegetation <- function(pref_code, pref_name) {
  shp_path <- get_main_shp(pref_code)
  
  if (!file.exists(shp_path)) {
    stop("Could not find shapefile: ", shp_path)
  }
  
  message("Reading: ", shp_path)
  
  x <- st_read(
    shp_path,
    options = "ENCODING=CP932",
    quiet = TRUE
  )
  
  x %>%
    mutate(
      pref_code = pref_code,
      pref_name = pref_name,
      MAJOR1_chr = as.character(MAJOR1),
      NAME_chr = enc2utf8(as.character(NAME))
    )
}

veg_all <- map2_dfr(
  pref_lookup$pref_code,
  pref_lookup$pref_name,
  read_vegetation
)

# transform to WGS84 for plotting
veg_all <- st_transform(veg_all, 4326)


# Identify sugi/hinoki-related plantation categories ----------------------

# Based on the Fukuoka file inspection:
# 9016 = スギ・ヒノキ・サワラ植林
# 9017 = スギ・ヒノキ植林
target_codes <- c("9016", "9017")

# Export a sanity-check table of relevant vegetation classes
class_check <- veg_all %>%
  st_drop_geometry() %>%
  distinct(MAJOR1_chr, NAME_chr) %>%
  filter(
    MAJOR1_chr %in% target_codes |
      str_detect(NAME_chr, "スギ") |
      str_detect(NAME_chr, "ヒノキ") |
      str_detect(NAME_chr, "サワラ")
  ) %>%
  arrange(MAJOR1_chr, NAME_chr)

write_csv(
  class_check,
  file.path(out_dir, "vegetation_class_check_sugi_hinoki.csv")
)

print(class_check)

# Filter to sugi/hinoki-related plantation vegetation
veg_target <- veg_all %>%
  filter(
    MAJOR1_chr %in% target_codes |
      (
        str_detect(NAME_chr, "スギ") &
          str_detect(NAME_chr, "ヒノキ") &
          str_detect(NAME_chr, "植林")
      )
  ) %>%
  mutate(
    vegetation_group = case_when(
      MAJOR1_chr == "9016" ~ "Sugi–hinoki–sawara plantation",
      MAJOR1_chr == "9017" ~ "Sugi–hinoki plantation",
      TRUE ~ "Other sugi–hinoki-related plantation"
    )
  )

# Area summary for checking / manuscript support --------------------------

area_summary <- veg_target %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry() %>%
  group_by(pref_name, vegetation_group) %>%
  summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    n_polygons = n(),
    .groups = "drop"
  ) %>%
  arrange(pref_name, vegetation_group)

write_csv(
  area_summary,
  file.path(out_dir, "sugi_hinoki_plantation_area_summary.csv")
)

print(area_summary)


# Vegetation area analysis ------------------------------------------------

sf_use_s2(FALSE)
plot_crs <- 32652  # WGS84 / UTM zone 52N, appropriate for Kyushu

analysis_scope <- "full_prefectures"
veg_area_sf <- veg_all %>%
  st_transform(plot_crs)

veg_area_df <- veg_area_sf %>%
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6,
    vegetation_class_3 = case_when(
      MAJOR1_chr == "9017" ~ "Sugi–hinoki plantation",
      MAJOR1_chr == "9016" ~ "Sugi–hinoki–sawara plantation",
      TRUE ~ "Other mapped vegetation"
    ),
    vegetation_class_2 = case_when(
      MAJOR1_chr %in% c("9016", "9017") ~ "Sugi–hinoki plantation classes",
      TRUE ~ "Other mapped vegetation"
    )
  ) %>%
  st_drop_geometry()

overall_3class <- veg_area_df %>%
  group_by(vegetation_class_3) %>%
  summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    n_polygons = n(),
    .groups = "drop"
  ) %>%
  mutate(
    percent_area = 100 * area_km2 / sum(area_km2, na.rm = TRUE)
  ) %>%
  arrange(desc(area_km2))

write_csv(
  overall_3class,
  file.path(out_dir, "overall_vegetation_area_3class.csv")
)

print(overall_3class)

# Create cleaned plotting objects -----------------------------------------

# crop to main Kyushu region
crop_wgs84 <- st_as_sfc(
  st_bbox(
    c(
      xmin = 129.3,
      xmax = 132.2,
      ymin = 31.0,
      ymax = 34.2
    ),
    crs = st_crs(4326)
  )
)

crop_proj <- st_transform(crop_wgs84, plot_crs)
bbox <- st_bbox(crop_proj)

# kyushu base map using MLIT/GSI N03 prefecture boundaries
get_boundary_shp <- function(pref_code) {
  boundary_dir <- file.path(
    root_dir,
    "kyushu-data",
    paste0("N03-140401_", pref_code, "_GML")
  )
  
  shp <- list.files(
    boundary_dir,
    pattern = "\\.shp$",
    full.names = TRUE
  )
  
  if (length(shp) == 0) {
    stop("Could not find boundary shapefile in: ", boundary_dir)
  }
  
  shp[1]
}

read_boundary <- function(pref_code, pref_name) {
  shp_path <- get_boundary_shp(pref_code)
  
  message("Reading boundary: ", shp_path)
  
  st_read(
    shp_path,
    options = "ENCODING=CP932",
    quiet = TRUE
  ) %>%
    st_transform(4326) %>%
    st_crop(crop_wgs84) %>%
    st_transform(plot_crs) %>%
    st_make_valid() %>%
    mutate(
      pref_code = pref_code,
      pref_name = pref_name
    ) %>%
    select(pref_code, pref_name, geometry)
}

kyushu_pref_plot <- map2_dfr(
  pref_lookup$pref_code,
  pref_lookup$pref_name,
  read_boundary
)

kyushu_pref_plot <- kyushu_pref_plot %>%
  st_simplify(
    dTolerance = 300,
    preserveTopology = TRUE
  )

# create main Kyushu landmass mask
kyushu_mask <- kyushu_pref_plot %>%
  st_make_valid() %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_make_valid() %>%
  st_cast("POLYGON", warn = FALSE) %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  slice_max(area_km2, n = 1, with_ties = FALSE) %>%
  select(geometry)

# fast filter to keep only polygons whose representative point is inside Kyushu
clip_to_kyushu <- function(x, mask) {
  x <- st_make_valid(x)
  x <- x[!st_is_empty(x), ]
  
  # point_on_surface is safer than centroid for irregular/coastal polygons
  pts <- st_point_on_surface(x)
  keep <- lengths(st_within(pts, mask)) > 0
  x[keep, ]
}

# vegetation plotting layers
is_target_veg <- function(df) {
  df$MAJOR1_chr %in% target_codes |
    (
      str_detect(df$NAME_chr, "スギ") &
        str_detect(df$NAME_chr, "ヒノキ") &
        str_detect(df$NAME_chr, "植林")
    )
}

crop_safely <- function(x, crop_obj) {
  tryCatch(
    st_crop(x, crop_obj),
    error = function(e) {
      message("st_crop failed; trying st_make_valid(): ", conditionMessage(e))
      st_crop(st_make_valid(x), crop_obj)
    }
  )
}

make_plot_layer <- function(x, label, tolerance_m) {
  x <- x %>%
    select(geometry) %>%
    st_transform(plot_crs)
  
  x <- crop_safely(x, crop_proj)
  x <- x[!st_is_empty(x), ]
  
  x <- st_simplify(
    x,
    dTolerance = tolerance_m,
    preserveTopology = FALSE
  )
  
  x <- x[!st_is_empty(x), ]
  x$vegetation_label <- label
  
  return(x)
}

is_other_forest_veg <- function(df) {
  !is_target_veg(df) &
    str_detect(
      df$NAME_chr,
      "植林|天然林|二次林|群落|群集|低木林|竹林"
    )
}

veg_other_plot <- veg_all %>%
  filter(is_other_forest_veg(.)) %>%
  make_plot_layer(
    label = "Other Vegetation",
    tolerance_m = OTHER_VEG_RES
  )

veg_target_plot <- veg_all %>%
  filter(is_target_veg(.)) %>%
  make_plot_layer(
    label = "Sugi–Hinoki Vegetation",
    tolerance_m = SUHI_RES
  )

veg_other_plot <- clip_to_kyushu(veg_other_plot, kyushu_mask)
veg_target_plot <- clip_to_kyushu(veg_target_plot, kyushu_mask)

veg_other_plot$vegetation_label <- factor(
  veg_other_plot$vegetation_label,
  levels = c("Other Vegetation", "Sugi–Hinoki Vegetation")
)

veg_target_plot$vegetation_label <- factor(
  veg_target_plot$vegetation_label,
  levels = c("Other Vegetation", "Sugi–Hinoki Vegetation")
)

# study cities
study_cities <- tibble::tibble(
  city = c(
    "Fukuoka",
    "Kitakyushu",
    "Kumamoto",
    "Nagasaki",
    "Oita",
    "Saga",
    "Kagoshima",
    "Miyazaki"
  ),
  lon = c(
    130.4017,
    130.8751,
    130.7079,
    129.8777,
    131.6093,
    130.3009,
    130.5571,
    131.4202
  ),
  lat = c(
    33.5902,
    33.8834,
    32.8031,
    32.7503,
    33.2396,
    33.2635,
    31.5966,
    31.9077
  )
)

city_points <- st_as_sf(
  study_cities,
  coords = c("lon", "lat"),
  crs = 4326
) %>%
  st_transform(plot_crs) %>%
  st_crop(crop_proj)

city_label_df <- city_points %>%
  cbind(st_coordinates(.)) %>%
  st_drop_geometry()

# Plot --------------------------------------------------------------------

veg_target_col <- "#2F6F4E"
veg_other_col <- "#BFD8B8"
land_fill <- "#F7F7F2"
land_border <- "#8A8A8A"

city_cols <- c(
  "Fukuoka" = "#0072B2",
  "Kitakyushu" = "#E69F00",
  "Kumamoto" = "#009E73",
  "Nagasaki" = "#D55E00",
  "Oita" = "#CC79A7",
  "Saga" = "#56B4E9",
  "Kagoshima" = "#8B5A2B",
  "Miyazaki" = "#5D5D5D"
)

p <- ggplot() +
  geom_sf(
    data = kyushu_pref_plot,
    fill = land_fill,
    color = land_border,
    linewidth = 0.25
  ) +
  geom_sf(
    data = veg_other_plot,
    aes(fill = vegetation_label),
    color = NA,
    alpha = 0.65
  ) +
  geom_sf(
    data = veg_target_plot,
    aes(fill = vegetation_label),
    color = NA,
    alpha = 0.95
  ) +
  geom_point(
    data = city_label_df,
    aes(x = X, y = Y, shape = city, color = city),
    size = 3.1,
    stroke = 0.9,
    fill = "white"
  ) +
  coord_sf(
    crs = st_crs(plot_crs),
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE,
    datum = NA
  ) +
  scale_fill_manual(
    values = c(
      "Other Vegetation" = veg_other_col,
      "Sugi–Hinoki Vegetation" = veg_target_col
    ),
    name = "Vegetation"
  ) +
  scale_shape_manual(
    values = c(
      "Fukuoka" = 21,
      "Kitakyushu" = 24,
      "Kumamoto" = 22,
      "Nagasaki" = 23,
      "Oita" = 25,
      "Saga" = 21,
      "Kagoshima" = 22,
      "Miyazaki" = 24
    ),
    name = "City"
  ) +
  scale_color_manual(
    values = city_cols,
    name = "City"
  ) +
  guides(
    shape = guide_legend(
      order = 1,
      ncol = 2,
      override.aes = list(
        size = 3.2,
        stroke = 0.9,
        fill = "white"
      )
    ),
    color = guide_legend(
      order = 1,
      ncol = 2,
      override.aes = list(
        size = 3.2,
        stroke = 0.9,
        fill = "white"
      )
    ),
    fill = guide_legend(
      order = 2,
      title = "Vegetation",
      override.aes = list(alpha = 1)
    )
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.7,
    line_width = 0.4,
    pad_x = grid::unit(0.35, "cm"),
    pad_y = grid::unit(0.35, "cm")
  ) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      text_size = 8,
      line_width = 0.4
    ),
    height = grid::unit(0.9, "cm"),
    width = grid::unit(0.9, "cm"),
    pad_x = grid::unit(0.35, "cm"),
    pad_y = grid::unit(0.35, "cm")
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    
    legend.position = "right",
    legend.box = "vertical",
    legend.box.just = "top",
    legend.spacing.y = grid::unit(0.18, "cm"),
    
    legend.background = element_rect(
      fill = "white",
      color = "grey70",
      linewidth = 0.3
    ),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.size = grid::unit(0.42, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8.5),
    legend.margin = margin(4, 6, 4, 6),
    
    plot.margin = margin(5, 5, 5, 5)
  )

graphics.off()

ggsave(
  filename = file.path(
    fig_dir, paste0("vegetation-map-", SUHI_RES, "-", OTHER_VEG_RES, "-res.pdf")),
  plot = p,
  width = 8.2,
  height = 7,
  units = "in",
  bg = "white",
  limitsize = FALSE
)
