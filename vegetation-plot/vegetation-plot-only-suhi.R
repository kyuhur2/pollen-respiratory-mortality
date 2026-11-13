rm(list = ls())

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
veg_dir <- file.path(root_dir, "vegetation-data")

dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

pref_lookup <- tibble::tibble(
  pref_code = c("40", "41", "42", "43", "44", "45", "46"),
  pref_name = c(
    "Fukuoka", "Saga", "Nagasaki", "Kumamoto",
    "Oita", "Miyazaki", "Kagoshima"
  )
)

veg_files <- file.path(
  veg_dir,
  c("veg2024bk8a.gpkg", "veg2024bk8b.gpkg")
)

missing_veg_files <- veg_files[!file.exists(veg_files)]
if (length(missing_veg_files) > 0) {
  stop("Could not find GeoPackage file(s): ", paste(missing_veg_files, collapse = ", "))
}

SUHI_RES <- 25
OTHER_VEG_RES <- 25
plot_crs <- 32652  # WGS84 / UTM zone 52N
sf_use_s2(FALSE)

# Helpers -----------------------------------------------------------------

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

read_boundary_full <- function(pref_code, pref_name) {
  shp_path <- get_boundary_shp(pref_code)
  message("Reading boundary: ", shp_path)
  
  st_read(
    shp_path,
    options = "ENCODING=CP932",
    quiet = TRUE
  ) %>%
    st_transform(4326) %>%
    st_make_valid() %>%
    mutate(
      pref_code = pref_code,
      pref_name = pref_name
    ) %>%
    select(pref_code, pref_name, geometry)
}

# Find a column despite Japanese/English naming differences between releases.
resolve_column <- function(nms, exact = character(), patterns = character(), label) {
  exact_hit <- exact[exact %in% nms]
  if (length(exact_hit) > 0) {
    return(exact_hit[1])
  }
  
  for (pattern in patterns) {
    hit <- grep(pattern, nms, value = TRUE, ignore.case = TRUE)
    if (length(hit) > 0) {
      return(hit[1])
    }
  }
  
  stop(
    "Could not identify the ", label, " column. Available columns are: ",
    paste(nms, collapse = ", ")
  )
}

get_feature_layer <- function(gpkg_path) {
  layer_info <- st_layers(gpkg_path)
  candidates <- layer_info$name[layer_info$geomtype %in% c(
    "Polygon", "Multi Polygon", "MultiPolygon", "Unknown (any)"
  )]
  
  if (length(candidates) == 0) {
    candidates <- layer_info$name
  }
  
  if (length(candidates) == 0) {
    stop("No readable layers found in: ", gpkg_path)
  }
  
  candidates[1]
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

clip_by_representative_point <- function(x, mask) {
  x <- st_make_valid(x)
  x <- x[!st_is_empty(x), ]
  pts <- st_point_on_surface(x)
  keep <- lengths(st_within(pts, mask)) > 0
  x[keep, ]
}

# Read Kyushu prefecture boundaries ---------------------------------------

kyushu_pref_full <- map2_dfr(
  pref_lookup$pref_code,
  pref_lookup$pref_name,
  read_boundary_full
)

# Dissolve municipality polygons in a projected CRS to avoid treating
# longitude/latitude coordinates as planar.
kyushu_pref_full <- kyushu_pref_full %>%
  st_transform(plot_crs) %>%
  group_by(pref_code, pref_name) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_make_valid() %>%
  st_transform(4326)

kyushu_all_mask <- kyushu_pref_full %>%
  st_transform(plot_crs) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_make_valid() %>%
  st_transform(4326)

# Read Existing Vegetation Map 2024 GeoPackages ---------------------------

# The files are JGD2011 (EPSG:6668). A spatial filter prevents Okinawa and
# unrelated offshore data from being loaded unnecessarily.
kyushu_filter_6668 <- kyushu_all_mask %>%
  st_transform(6668) %>%
  st_geometry() %>%
  st_as_text()

# st_read() expects one WKT string.
kyushu_filter_6668 <- kyushu_filter_6668[[1]]

read_veg2024 <- function(gpkg_path) {
  layer_name <- get_feature_layer(gpkg_path)
  message("Reading: ", gpkg_path, " | layer: ", layer_name)
  
  x <- st_read(
    gpkg_path,
    layer = layer_name,
    wkt_filter = kyushu_filter_6668,
    quiet = TRUE
  )
  
  nms <- names(x)
  message("Columns in ", basename(gpkg_path), ": ", paste(nms, collapse = ", "))
  
  legend_name_col <- resolve_column(
    nms,
    exact = c(
      "凡例名", "植生凡例名", "群落名",
      "legend_name", "LegendName", "LEGEND_NAME"
    ),
    patterns = c("凡例.*名", "legend.*name", "群落.*名"),
    label = "legend name"
  )
  
  legend_code_col <- resolve_column(
    nms,
    exact = c(
      "凡例コード", "凡例コード番号", "凡例番号",
      "legend_code", "LegendCode", "LEGEND_CODE"
    ),
    patterns = c("凡例.*(コード|番号)", "legend.*code"),
    label = "legend code"
  )
  
  year_col <- tryCatch(
    resolve_column(
      nms,
      exact = c(
        "作成年", "作成年次", "作成年度",
        "year_created", "creation_year", "YearCreated"
      ),
      patterns = c("作成.*年", "year.*creat", "creat.*year"),
      label = "year of creation"
    ),
    error = function(e) NA_character_
  )
  
  # Preserve the active sf geometry explicitly. In these GeoPackages the
  # geometry column is named `geom`, not `geometry`.
  x_geom <- st_geometry(x)
  
  x_attributes <- x %>%
    st_drop_geometry() %>%
    transmute(
      source_file = basename(gpkg_path),
      legend_code = as.character(.data[[legend_code_col]]),
      legend_name = enc2utf8(as.character(.data[[legend_name_col]])),
      year_created = if (!is.na(year_col)) {
        suppressWarnings(as.integer(as.character(.data[[year_col]])))
      } else {
        NA_integer_
      }
    )
  
  x <- st_sf(
    x_attributes,
    geometry = x_geom,
    crs = st_crs(x)
  ) %>%
    st_transform(4326)
  
  # wkt_filter uses a spatial envelope; apply an exact Kyushu spatial filter.
  clip_by_representative_point(x, kyushu_all_mask)
}

veg_list <- map(veg_files, read_veg2024)
veg_all <- do.call(rbind, veg_list)
rm(veg_list)
gc()

# Assign each polygon to a prefecture using a representative interior point.
# This avoids duplicating polygons in a direct polygon-to-polygon spatial join.
veg_points <- st_point_on_surface(veg_all)
pref_index <- st_within(veg_points, kyushu_pref_full)
pref_row <- vapply(pref_index, function(i) if (length(i)) i[1] else NA_integer_, integer(1))

veg_all$pref_code <- kyushu_pref_full$pref_code[pref_row]
veg_all$pref_name <- kyushu_pref_full$pref_name[pref_row]
veg_all <- veg_all %>% filter(!is.na(pref_code))
rm(veg_points, pref_index, pref_row)
gc()

# Identify sugi/hinoki-related plantation categories ----------------------

# Existing Vegetation Map 2024 uses the official six-digit legend code:
# 540100 = スギ・ヒノキ・サワラ植林
target_codes <- c("540100")

is_target_veg <- function(df) {
  df$legend_code %in% target_codes |
    str_detect(
      coalesce(df$legend_name, ""),
      "スギ.*ヒノキ.*サワラ.*植林|スギ.*ヒノキ.*植林"
    )
}

# Export all potentially relevant classes before making the final filter.
class_check <- veg_all %>%
  st_drop_geometry() %>%
  distinct(legend_code, legend_name, year_created) %>%
  filter(
    str_detect(coalesce(legend_name, ""), "スギ|ヒノキ|サワラ")
  ) %>%
  arrange(legend_code, legend_name, year_created)

write_csv(
  class_check,
  file.path(out_dir, "vegetation_class_check_sugi_hinoki_2024.csv")
)

veg_target <- veg_all %>%
  filter(is_target_veg(.)) %>%
  mutate(
    vegetation_group = case_when(
      str_detect(legend_name, "サワラ") ~ "Sugi–hinoki–sawara plantation",
      TRUE ~ "Sugi–hinoki plantation"
    )
  )

if (nrow(veg_target) == 0) {
  stop(
    "No sugi–hinoki plantation polygons were found. Inspect ",
    file.path(out_dir, "vegetation_class_check_sugi_hinoki_2024.csv"),
    " and adjust suhi_name_pattern to match the exact legend wording."
  )
}

# Area summaries ----------------------------------------------------------

veg_target_area <- veg_target %>% st_transform(plot_crs)

area_summary <- veg_target_area %>%
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
  file.path(out_dir, "sugi_hinoki_plantation_area_summary_2024.csv")
)
print(area_summary)

veg_area_df <- veg_all %>%
  st_transform(plot_crs) %>%
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6,
    vegetation_class_3 = case_when(
      is_target_veg(.) & str_detect(legend_name, "サワラ") ~
        "Sugi–hinoki–sawara plantation",
      is_target_veg(.) ~ "Sugi–hinoki plantation",
      TRUE ~ "Other mapped vegetation"
    ),
    vegetation_class_2 = case_when(
      is_target_veg(.) ~ "Sugi–hinoki plantation classes",
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
  mutate(percent_area = 100 * area_km2 / sum(area_km2, na.rm = TRUE)) %>%
  arrange(desc(area_km2))

write_csv(
  overall_3class,
  file.path(out_dir, "overall_vegetation_area_3class_2024.csv")
)
print(overall_3class)

# Optional mapping-year summary, useful for documenting temporal coverage.
year_summary <- veg_all %>%
  st_drop_geometry() %>%
  count(pref_name, year_created, name = "n_polygons") %>%
  arrange(pref_name, year_created)

write_csv(
  year_summary,
  file.path(out_dir, "vegetation_mapping_years_by_prefecture_2024.csv")
)

# Create plotting objects -------------------------------------------------

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

kyushu_pref_plot <- kyushu_pref_full %>%
  st_crop(crop_wgs84) %>%
  st_transform(plot_crs) %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 300, preserveTopology = TRUE)

# Keep only the largest connected land polygon, matching the original script's
# focus on the main Kyushu landmass.
kyushu_mask <- kyushu_pref_plot %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_make_valid() %>%
  st_cast("POLYGON", warn = FALSE) %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  slice_max(area_km2, n = 1, with_ties = FALSE) %>%
  select(geometry)

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
  x
}

is_other_forest_veg <- function(df) {
  !is_target_veg(df) &
    str_detect(
      coalesce(df$legend_name, ""),
      "植林|天然林|二次林|群落|群集|低木林|竹林"
    )
}

veg_other_plot <- veg_all %>%
  filter(is_other_forest_veg(.)) %>%
  make_plot_layer(
    label = "Other Vegetation",
    tolerance_m = OTHER_VEG_RES
  ) %>%
  clip_by_representative_point(kyushu_mask)

veg_target_plot <- veg_target %>%
  make_plot_layer(
    label = "Sugi–Hinoki Vegetation",
    tolerance_m = SUHI_RES
  ) %>%
  clip_by_representative_point(kyushu_mask)

veg_other_plot$vegetation_label <- factor(
  veg_other_plot$vegetation_label,
  levels = c("Other Vegetation", "Sugi–Hinoki Vegetation")
)

veg_target_plot$vegetation_label <- factor(
  veg_target_plot$vegetation_label,
  levels = c("Other Vegetation", "Sugi–Hinoki Vegetation")
)

# Study cities ------------------------------------------------------------

study_cities <- tibble::tibble(
  city = c(
    "Fukuoka", "Kitakyushu", "Kumamoto", "Nagasaki",
    "Oita", "Saga", "Kagoshima", "Miyazaki"
  ),
  lon = c(
    130.4017, 130.8751, 130.7079, 129.8777,
    131.6093, 130.3009, 130.5571, 131.4202
  ),
  lat = c(
    33.5902, 33.8834, 32.8031, 32.7503,
    33.2396, 33.2635, 31.5966, 31.9077
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
  scale_color_manual(values = city_cols, name = "City") +
  guides(
    shape = guide_legend(
      order = 1,
      ncol = 2,
      override.aes = list(size = 3.2, stroke = 0.9, fill = "white")
    ),
    color = guide_legend(
      order = 1,
      ncol = 2,
      override.aes = list(size = 3.2, stroke = 0.9, fill = "white")
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
    fig_dir,
    paste0("vegetation-map-2024-", SUHI_RES, "-", OTHER_VEG_RES, "-res.pdf")
  ),
  plot = p,
  width = 8.2,
  height = 7,
  units = "in",
  bg = "white",
  limitsize = FALSE
)
