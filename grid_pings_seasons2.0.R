#####################################################################
# GRID 5x5 km PAIRED — Autumn(Y) + Winter(Y+1)
# SAME INPUT FILES AS KDE — WIDE ONLY — ALL CELLS
# ================================================================
# - Uses proxy_grid_fall and proxy_grid_winter CSVs, same as KDE
# - Autumn(Y) and Winter(Y+1) are shown on the SAME map
# - Shows ALL cells with activity (no hotspot/quantile threshold)
# - Dates are parsed with multiple formats because Autumn and Winter differ
# - COMPUTE in EPSG:27700, PLOT with lon/lat axes in EPSG:4326
# - OUTPUT: TIFF, 300 dpi, LZW compression
#####################################################################

rm(list = ls())

# ==== PACKAGES ====
pkgs <- c("sf", "dplyr", "ggplot2", "readr", "lubridate", "tidyr", "scales", "ggnewscale")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyr)
library(scales)
library(ggnewscale)

sf_use_s2(FALSE)

# ==== CRS ====
crs_bng <- 27700
crs_ll  <- 4326

# ==== CONFIG ====
cell_km <- 5
cell_m  <- cell_km * 1000
years_map <- 1999:2023  # TEST ONLY: Autumn 1999 + Winter 2000

# IMPORTANT: all cells with activity are shown
apply_hotspot_threshold <- FALSE
thr_q <- 0.70  # ignored when apply_hotspot_threshold = FALSE

# ==== BBOX VERSION ====
version_name <- "wide"

bbox_cuts <- list(
  west_lon_cut    = -12.5,
  norway_lon_cut  = 5.0,
  south_lat_cut   = 48.5,
  iceland_lat_cut = 63
)

# ==== COLOURS / STYLE ====
# NOTE FOR THIS FIRST VISUAL TEST:
# The colour-scale limits are still automatic. Once the layout is approved,
# calculate fixed Autumn and Winter limits across all years and add them to
# the two scale_fill_gradientn() calls so colours are comparable through time.

sea_col       <- "#FAFAFA"
land_fill     <- "grey92"
land_border   <- "grey35"

# ICES boundaries: lighter and more subtle
ices_col <- scales::alpha("grey55", 0.30)

grid_line_col <- scales::alpha("white", 0.35)
grid_line_lwd <- 0.08

# Seasonal palettes
# Both start at a medium, clearly visible colour so low-activity cells
# remain visible against the near-white sea background.

cols_autumn <- c(
  "#F8D66D",
  "#F2A12C",
  "#E8751A",
  "#D94801",
  "#B52B12",
  "#7F0000"
)

cols_winter <- c(
  "#6BAED6",
  "#2171B5",
  "#0868AC",
  "#08519C",
  "#08306B",
  "#041F4A"
)

legend_text  <- 8
legend_title <- 9

# ==== PATHS ====
proxy_fall_path   <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_fall/proxy_fishing_activity_fall_timeweighted.csv"
proxy_winter_path <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_winter/proxy_fishing_activity_winter_timeweighted.csv"

shp_dir      <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/shapefiles_shetland_europe_scotland"
europe_path  <- file.path(shp_dir, "country.shp")
scot_path    <- file.path(shp_dir, "OS_Scotland_WGS84.shp")

ices_path <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/chapter2/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp"

# ==== OUTPUT ====
out_root <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/novas_KDE/grid_proxy_paired_autumn_winter"
out_dir  <- file.path(out_root, version_name)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
message("OUTPUT FOLDER: ", out_dir)
message("RUN STAMP: ", run_stamp)

# ============================================================
# HELPERS
# ============================================================

parse_date_multi <- function(x) {
  lubridate::parse_date_time(
    as.character(x),
    orders = c(
      "dmy HMS", "ymd HMS", "mdy HMS",
      "dmy HM",  "ymd HM",  "mdy HM",
      "dmy",     "ymd",     "mdy"
    ),
    quiet = TRUE,
    tz = "UTC"
  )
}

ensure_year <- function(df) {
  # Some CSVs contain a 'year' column that is not a calendar year
  # e.g. values 1–31. Use it only if it looks like a real year.
  year_is_valid <- "year" %in% names(df) &&
    suppressWarnings(max(as.integer(df$year), na.rm = TRUE)) > 1900
  
  if (!year_is_valid) {
    if ("date" %in% names(df)) {
      df <- df %>%
        mutate(
          date_parsed = parse_date_multi(date),
          year = lubridate::year(date_parsed)
        )
    } else if ("datetime" %in% names(df)) {
      df <- df %>%
        mutate(
          datetime_parsed = parse_date_multi(datetime),
          year = lubridate::year(datetime_parsed)
        )
    } else {
      stop("CSV sem coluna 'year' válida e sem 'date/datetime' para derivar year.")
    }
  }
  
  df %>% mutate(year = as.integer(year))
}

standardise_lonlat_names <- function(df) {
  if (all(c("X_lon", "Y_lat") %in% names(df))) {
    df <- df %>%
      mutate(
        X_lon = as.numeric(X_lon),
        Y_lat = as.numeric(Y_lat)
      )
  } else if (all(c("lon", "lat") %in% names(df))) {
    df <- df %>%
      mutate(
        X_lon = as.numeric(lon),
        Y_lat = as.numeric(lat)
      )
  } else {
    stop("Expected coordinate columns 'X_lon'/'Y_lat' or 'lon'/'lat'.")
  }
  df
}

clean_sf_points <- function(x) {
  x <- x[!sf::st_is_empty(x), ]
  x <- x[!is.na(sf::st_geometry(x)), ]
  xy <- sf::st_coordinates(x)
  keep <- is.finite(xy[, 1]) & is.finite(xy[, 2])
  x[keep, ]
}

make_frame_ll <- function(cuts) {
  frame_ll <- st_bbox(
    c(
      xmin = cuts$west_lon_cut,
      ymin = cuts$south_lat_cut,
      xmax = cuts$norway_lon_cut,
      ymax = cuts$iceland_lat_cut
    ),
    crs = st_crs(crs_ll)
  )
  st_as_sfc(frame_ll) %>% st_make_valid()
}

safe_intersection <- function(x, y) {
  bb <- st_bbox(y)
  suppressWarnings(
    tryCatch(
      st_intersection(x, y),
      error = function(e) st_crop(x, bb)
    )
  )
}

make_aligned_grid_bng <- function(frame_bng, cell_m = 5000) {
  bb <- st_bbox(frame_bng)
  xmin <- floor(as.numeric(bb["xmin"]) / cell_m) * cell_m
  ymin <- floor(as.numeric(bb["ymin"]) / cell_m) * cell_m
  xmax <- ceiling(as.numeric(bb["xmax"]) / cell_m) * cell_m
  ymax <- ceiling(as.numeric(bb["ymax"]) / cell_m) * cell_m
  
  bb_sfc <- st_as_sfc(
    st_bbox(
      c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
      crs = st_crs(frame_bng)
    )
  )
  
  st_make_grid(
    bb_sfc,
    cellsize = cell_m,
    what = "polygons",
    square = TRUE
  ) %>%
    st_as_sf() %>%
    mutate(cell_id = row_number()) %>%
    st_intersection(st_make_valid(frame_bng)) %>%
    st_make_valid()
}

points_to_grid_count <- function(pts_bng, grid_bng) {
  if (nrow(pts_bng) == 0) {
    return(grid_bng %>% mutate(n_points = NA_real_))
  }
  
  joined <- st_join(
    pts_bng,
    grid_bng %>% dplyr::select(cell_id),
    join = st_within,
    left = FALSE
  )
  
  if (nrow(joined) == 0) {
    return(grid_bng %>% mutate(n_points = NA_real_))
  }
  
  counts <- joined %>%
    st_drop_geometry() %>%
    count(cell_id, name = "n_points")
  
  grid_bng %>%
    left_join(counts, by = "cell_id") %>%
    mutate(
      n_points = ifelse(
        is.finite(n_points) & n_points > 0,
        n_points,
        NA_real_
      )
    )
}

apply_grid_threshold <- function(grid_sf, q = 0.70) {
  if (!apply_hotspot_threshold) return(grid_sf)
  
  v <- grid_sf$n_points
  v <- v[is.finite(v)]
  
  if (length(v) < 10) return(grid_sf)
  
  thr_val <- as.numeric(
    stats::quantile(v, probs = q, na.rm = TRUE)
  )
  
  grid_sf %>%
    mutate(
      n_points = ifelse(
        !is.na(n_points) & n_points >= thr_val,
        n_points,
        NA_real_
      )
    )
}

# ============================================================
# READ BASE MAPS
# ============================================================

europe_land_ll <- st_read(europe_path, quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(crs_ll)

scotland_land_ll <- st_read(scot_path, quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(crs_ll)

ICES_ll <- st_read(ices_path, quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(crs_ll)

# ============================================================
# READ DATASETS: SAME FILES AS KDE
# ============================================================

proxy_fall <- read_csv(
  proxy_fall_path,
  show_col_types = FALSE
) %>%
  standardise_lonlat_names() %>%
  ensure_year()

proxy_winter <- read_csv(
  proxy_winter_path,
  show_col_types = FALSE
) %>%
  standardise_lonlat_names() %>%
  ensure_year()

message(
  "Autumn years after correction: ",
  paste(range(proxy_fall$year, na.rm = TRUE), collapse = "–")
)

message(
  "Winter years after correction: ",
  paste(range(proxy_winter$year, na.rm = TRUE), collapse = "–")
)

pts_fall_bng <- st_as_sf(
  proxy_fall,
  coords = c("X_lon", "Y_lat"),
  crs = crs_ll,
  remove = FALSE
) %>%
  st_transform(crs_bng) %>%
  clean_sf_points()

pts_wint_bng <- st_as_sf(
  proxy_winter,
  coords = c("X_lon", "Y_lat"),
  crs = crs_ll,
  remove = FALSE
) %>%
  st_transform(crs_bng) %>%
  clean_sf_points()

# ============================================================
# BBOX + GRID
# ============================================================

frame_ll <- make_frame_ll(bbox_cuts)
frame_bbox_ll <- st_bbox(frame_ll)

xmin_ll <- as.numeric(frame_bbox_ll["xmin"])
xmax_ll <- as.numeric(frame_bbox_ll["xmax"])
ymin_ll <- as.numeric(frame_bbox_ll["ymin"])
ymax_ll <- as.numeric(frame_bbox_ll["ymax"])

frame_bng <- st_transform(frame_ll, crs_bng) %>%
  st_make_valid()

europe_crop_ll <- safe_intersection(
  europe_land_ll,
  frame_ll
) %>%
  st_make_valid()

scot_crop_ll <- safe_intersection(
  scotland_land_ll,
  frame_ll
) %>%
  st_make_valid()

ices_crop_ll <- safe_intersection(
  ICES_ll,
  frame_ll
) %>%
  st_make_valid()

ocean_ll <- st_as_sf(frame_ll)

ices_crop_bng <- st_transform(
  ices_crop_ll,
  crs_bng
) %>%
  st_make_valid()

ices_crop_bng <- tryCatch(
  st_buffer(ices_crop_bng, 0),
  error = function(e) ices_crop_bng
)

ices_crop_bng <- st_collection_extract(
  ices_crop_bng,
  "POLYGON"
)

ices_crop_bng <- ices_crop_bng[
  !st_is_empty(ices_crop_bng),
]

if (nrow(ices_crop_bng) == 0) {
  stop(
    "ICES crop became empty for bbox version: ",
    version_name
  )
}

ices_union_bng <- tryCatch(
  st_union(st_combine(ices_crop_bng)) %>%
    st_make_valid(),
  error = function(e) st_make_valid(ices_crop_bng)
)

grid_bng <- make_aligned_grid_bng(
  frame_bng,
  cell_m = cell_m
)

grid_bng <- grid_bng[
  lengths(
    st_intersects(grid_bng, ices_union_bng)
  ) > 0,
]

if (nrow(grid_bng) == 0) {
  stop(
    "Grid became empty after ICES clipping for bbox version: ",
    version_name
  )
}

grid_ll <- st_transform(
  grid_bng,
  crs_ll
)

# ============================================================
# LOOP: Autumn(Y) + Winter(Y+1)
# ============================================================

for (Y in years_map) {
  
  A_year <- Y
  W_year <- Y + 1
  
  message(
    sprintf(
      "-> Grid paired map: Autumn %d + Winter %d [%s]",
      A_year,
      W_year,
      version_name
    )
  )
  
  A_pts <- pts_fall_bng %>%
    filter(year == A_year) %>%
    clean_sf_points()
  
  W_pts <- pts_wint_bng %>%
    filter(year == W_year) %>%
    clean_sf_points()
  
  if (nrow(A_pts) < 5 && nrow(W_pts) < 5) {
    message("   Poucos pontos em ambas seasons. Pulando.")
    next
  }
  
  A_pts_clip <- suppressWarnings(
    tryCatch(
      st_intersection(A_pts, ices_union_bng),
      error = function(e) A_pts
    )
  )
  
  W_pts_clip <- suppressWarnings(
    tryCatch(
      st_intersection(W_pts, ices_union_bng),
      error = function(e) W_pts
    )
  )
  
  if (nrow(A_pts_clip) < 5) A_pts_clip <- A_pts
  if (nrow(W_pts_clip) < 5) W_pts_clip <- W_pts
  
  g_A <- points_to_grid_count(
    A_pts_clip,
    grid_bng
  ) %>%
    apply_grid_threshold(q = thr_q)
  
  g_W <- points_to_grid_count(
    W_pts_clip,
    grid_bng
  ) %>%
    apply_grid_threshold(q = thr_q)
  
  g_A_ll_draw <- st_transform(
    g_A,
    crs_ll
  ) %>%
    filter(!is.na(n_points), n_points > 1)
  
  g_W_ll_draw <- st_transform(
    g_W,
    crs_ll
  ) %>%
    filter(!is.na(n_points), n_points > 1)
  
  if (
    nrow(g_A_ll_draw) == 0 &&
    nrow(g_W_ll_draw) == 0
  ) {
    message("   Nenhuma célula com pontos. Pulando.")
    next
  }
  
  # ==========================================================
  # BASE MAP
  # ==========================================================
  
  p <- ggplot() +
    geom_sf(
      data = ocean_ll,
      fill = sea_col,
      color = NA
    ) +
    geom_sf(
      data = grid_ll,
      fill = NA,
      color = grid_line_col,
      linewidth = grid_line_lwd
    ) +
    geom_sf(
      data = europe_crop_ll,
      fill = land_fill,
      color = land_border,
      linewidth = 0.15
    ) +
    geom_sf(
      data = scot_crop_ll,
      fill = land_fill,
      color = land_border,
      linewidth = 0.25
    ) +
    
    # ICES boundaries: intentionally subtle
    geom_sf(
      data = ices_crop_ll,
      fill = NA,
      color = ices_col,
      linewidth = 0.18
    )
  
  # ==========================================================
  # AUTUMN
  # ==========================================================
  
  if (nrow(g_A_ll_draw) > 0) {
    
    p <- p +
      ggnewscale::new_scale_fill() +
      geom_sf(
        data = g_A_ll_draw,
        aes(fill = n_points),
        color = NA,
        alpha = 0.85
      ) +
      scale_fill_gradientn(
        colours = cols_autumn,
        trans = "sqrt",
        name = sprintf(
          "Autumn %d grid cells",
          A_year
        ),
        na.value = NA,
        guide = guide_colorbar(order = 1)
      )
  }
  
  # ==========================================================
  # WINTER
  # ==========================================================
  
  if (nrow(g_W_ll_draw) > 0) {
    
    p <- p +
      ggnewscale::new_scale_fill() +
      geom_sf(
        data = g_W_ll_draw,
        aes(fill = n_points),
        color = NA,
        alpha = 0.55
      ) +
      scale_fill_gradientn(
        colours = cols_winter,
        trans = "sqrt",
        name = sprintf(
          "Winter %d grid cells",
          W_year
        ),
        na.value = NA,
        guide = guide_colorbar(order = 2)
      )
  }
  
  # ==========================================================
  # MAP LAYOUT
  # ==========================================================
  
  p <- p +
    coord_sf(
      crs = st_crs(crs_ll),
      xlim = c(xmin_ll, xmax_ll),
      ylim = c(ymin_ll, ymax_ll),
      expand = FALSE
    ) +
    labs(
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.title = element_text(
        size = legend_title
      ),
      legend.text = element_text(
        size = legend_text
      ),
      
      # Legends fully inside the map, stacked vertically
      # Autumn on top, Winter underneath
      legend.position = "inside",
      legend.position.inside = c(0.97, 0.07),
      legend.box = "vertical",
      legend.justification = c(1, 0),
      
      # White background so the legends remain readable over the map
      legend.background = element_rect(
        fill = "white",
        colour = NA
      ),
      legend.box.background = element_rect(
        fill = "white",
        colour = NA
      ),
      
      # Compact legend keys
      legend.key.height = grid::unit(
        0.35,
        "cm"
      ),
      legend.key.width = grid::unit(
        0.35,
        "cm"
      ),
      
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = scales::alpha(
          "grey40",
          0.12
        ),
        linewidth = 0.3
      ),
      panel.background = element_rect(
        fill = sea_col,
        colour = NA
      ),
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      
      # Reduce unnecessary white space around the figure
      plot.margin = margin(
        3,
        3,
        3,
        3
      )
    )
  
  # ==========================================================
  # EXPORT
  # ==========================================================
  
  tiff_out <- file.path(
    out_dir,
    sprintf(
      "GRID_TEST2_PAIRED_%s_A%04d_W%04d_cell%dkm_allcells_%s.tiff",
      version_name,
      A_year,
      W_year,
      cell_km,
      run_stamp
    )
  )
  
  ggsave(
    filename = tiff_out,
    plot = p,
    width = 7.5,
    height = 8.6,
    units = "in",
    dpi = 300,
    bg = "white",
    device = "tiff",
    compression = "lzw"
  )
  
  message("   ✔ ", tiff_out)
  
  if (nrow(A_pts) < 5) {
    message(
      "   [AVISO] Autumn ",
      A_year,
      " com poucos dados (ou ausente)."
    )
  }
  
  if (nrow(W_pts) < 5) {
    message(
      "   [AVISO] Winter ",
      W_year,
      " com poucos dados (ou ausente)."
    )
  }
}

message(
  "\n🎯 Done. Check output folder:\n- ",
  out_dir
)

