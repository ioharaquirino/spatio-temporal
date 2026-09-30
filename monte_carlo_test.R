# ============================================================
# MONTE CARLO VALIDATION — GRID ONLY
# Paper version: no KDE is used anywhere in the analysis
#
# NULL sazonal (separately for Autumn and Winter):
#   - for each year, identify all occupied 5 x 5 km Plotter cells
#   - add a light 0.5-cell (2.5 km) annual buffer
#   - union annual GRID envelopes across 2020–2023
#   - convex hull
#   - 5 km buffer
#   - intersect with seasonal domain
#   - remove land
#
# ANNUAL PLOTTER FOOTPRINT:
#   - all occupied 5 x 5 km Plotter cells (>= 1 proxy occurrence)
#   - union cells
#   - 1-cell (5 km) buffer
#   - intersect with seasonal domain
#   - remove land
#
# MONTE CARLO:
#   - for each season x year, count observed self-sampling hauls
#     falling inside the annual Plotter GRID footprint
#   - generate the same number of random points within the
#     GRID-only seasonal null
#   - repeat 999 times
#   - compare observed overlap with the null distribution
#
# OUTPUTS:
#   1) final_table       = paper-ready Monte Carlo results
#   2) frame_audit_table = audit of Plotter records excluded by frame
# ============================================================

rm(list = ls())

# ============================================================
# PACKAGES
# ============================================================
pkgs <- c("sf", "dplyr", "readr", "lubridate")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(sf)
library(dplyr)
library(readr)
library(lubridate)

sf_use_s2(FALSE)

# ============================================================
# CRS / CONFIG
# ============================================================
crs_bng <- 27700
crs_ll  <- 4326

# GRID
cell_km                    <- 5
cell_m                     <- cell_km * 1000
grid_foot_buffer_cells     <- 1
grid_foot_buffer_m         <- grid_foot_buffer_cells * cell_m
grid_null_annual_buf_cells <- 0.5
grid_null_annual_buf_m     <- grid_null_annual_buf_cells * cell_m

# Final seasonal-null buffer
null_buffer_km <- 5
null_buffer_m  <- null_buffer_km * 1000

# Monte Carlo
nperm <- 999
set.seed(123)

# Years
years_keep <- 2020:2023

# ============================================================
# PATHS
# Corrected to match the current overlap script
# ============================================================

shp_dir <- paste0(
  "C:/Users/SH01IQ/OneDrive - UHI/Desktop/",
  "iohara's phd/PhD_chapters/",
  "Chapter 2 - fishing areas changes/",
  "shapefiles_shetland_europe_scotland"
)

europe_path <- file.path(
  shp_dir,
  "country.shp"
)

scot_path <- file.path(
  shp_dir,
  "OS_Scotland_WGS84.shp"
)

# PLOTTER DATA
path_plotter_aut <- paste0(
  "C:/Users/SH01IQ/OneDrive - UHI/Desktop/",
  "iohara's phd/PhD_chapters/",
  "Chapter 2 - fishing areas changes/",
  "all_datasets/proxy_grid_fall/",
  "proxy_fishing_activity_fall_timeweighted.csv"
)

path_plotter_win <- paste0(
  "C:/Users/SH01IQ/OneDrive - UHI/Desktop/",
  "iohara's phd/PhD_chapters/",
  "Chapter 2 - fishing areas changes/",
  "all_datasets/proxy_grid_winter/",
  "proxy_fishing_activity_winter_timeweighted.csv"
)

# SELF-SAMPLING DATA
base_dir <- paste0(
  "C:/Users/SH01IQ/OneDrive - UHI/Desktop/",
  "iohara's phd/PhD_chapters/",
  "Chapter 2 - fishing areas changes/",
  "all_datasets"
)

ss_path <- file.path(
  base_dir,
  "self_sampling_final.csv"
)

# OUTPUT
desktop_dir <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop"

# ============================================================
# FRAME BASE + SEASON CUTS
# IMPORTANT:
# These are kept exactly as in the Monte Carlo script supplied.
# They are analytical-domain settings, not the display bbox of
# the newer overlap figures.
# ============================================================
shetland_bbox_ll   <- c(xmin = -2.70, xmax = -0.20, ymin = 59.40, ymax = 61.30)
shetland_buffer_km <- 30
norway_lon_cut     <- 6.00
iceland_lat_cut    <- 66.00

frame_ll <- st_bbox(
  c(xmin = -16, ymin = 48, xmax = 10, ymax = 67),
  crs = st_crs(crs_ll)
)
frame_ll["xmax"] <- min(as.numeric(frame_ll["xmax"]), norway_lon_cut)
frame_ll["ymax"] <- min(as.numeric(frame_ll["ymax"]), iceland_lat_cut)
frame_sfc_ll <- st_as_sfc(frame_ll)

shet_bb  <- st_bbox(shetland_bbox_ll, crs = st_crs(crs_ll))
shet_sfc <- st_as_sfc(shet_bb)
shet_buf <- st_buffer(
  st_transform(shet_sfc, 3857),
  dist = shetland_buffer_km * 1000
) |>
  st_transform(crs_ll) |>
  st_make_valid()

frame_union_ll_base <- st_union(
  st_make_valid(frame_sfc_ll),
  st_make_valid(shet_buf)
) |>
  st_make_valid()

autumn_ymin_core <- 57.0
autumn_ymax      <- 64.0
autumn_xmin      <- -10.0
autumn_lat_buffer_deg <- 0.20

winter_ymin <- 53.5
winter_ymax <- 64.0
winter_xmax <- 3.25

make_season_frame_ll <- function(season_name, base_frame_union_ll) {
  bb <- st_bbox(base_frame_union_ll)
  
  xmin <- as.numeric(bb["xmin"])
  xmax <- as.numeric(bb["xmax"])
  ymin <- as.numeric(bb["ymin"])
  ymax <- as.numeric(bb["ymax"])
  
  if (tolower(season_name) == "autumn") {
    ymin <- max(ymin, autumn_ymin_core - autumn_lat_buffer_deg)
    ymax <- min(ymax, autumn_ymax)
    xmin <- max(xmin, autumn_xmin)
    
  } else if (tolower(season_name) == "winter") {
    ymin <- max(ymin, winter_ymin)
    ymax <- min(ymax, winter_ymax)
    xmax <- min(xmax, winter_xmax)
    
  } else {
    stop("season_name must be 'Autumn' or 'Winter'.")
  }
  
  season_bb <- st_bbox(
    c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
    crs = st_crs(crs_ll)
  ) |>
    st_as_sfc()
  
  suppressWarnings(
    tryCatch(
      st_intersection(base_frame_union_ll, season_bb),
      error = function(e) st_crop(base_frame_union_ll, st_bbox(season_bb))
    )
  ) |>
    st_make_valid()
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

# ============================================================
# HELPERS
# ============================================================
km2 <- function(x) {
  as.numeric(st_area(x)) / 1e6
}

parse_date_multi <- function(x) {
  lubridate::parse_date_time(
    as.character(x),
    orders = c(
      "ymd HMS", "dmy HMS", "mdy HMS",
      "ymd HM",  "dmy HM",  "mdy HM",
      "ymd",     "dmy",     "mdy"
    ),
    quiet = TRUE,
    tz = "UTC"
  )
}

ensure_year <- function(df) {
  
  year_is_valid <-
    "year" %in% names(df) &&
    suppressWarnings(
      max(as.integer(df$year), na.rm = TRUE)
    ) > 1900
  
  if (!year_is_valid) {
    
    if ("date" %in% names(df)) {
      df <- df |>
        mutate(
          date_parsed = parse_date_multi(date),
          year = lubridate::year(date_parsed)
        )
      
    } else if ("datetime" %in% names(df)) {
      df <- df |>
        mutate(
          datetime_parsed = parse_date_multi(datetime),
          year = lubridate::year(datetime_parsed)
        )
      
    } else {
      stop(
        paste0(
          "CSV without a valid 'year' column and without ",
          "'date/datetime' from which year can be derived."
        )
      )
    }
  }
  
  df |>
    mutate(year = as.integer(year))
}

standardise_plotter_lonlat <- function(df) {
  
  if (all(c("X_lon", "Y_lat") %in% names(df))) {
    df <- df |>
      mutate(
        X_lon = as.numeric(X_lon),
        Y_lat = as.numeric(Y_lat)
      )
    
  } else if (all(c("lon", "lat") %in% names(df))) {
    df <- df |>
      mutate(
        X_lon = as.numeric(lon),
        Y_lat = as.numeric(lat)
      )
    
  } else {
    stop(
      "Expected Plotter coordinate columns 'X_lon'/'Y_lat' or 'lon'/'lat'."
    )
  }
  
  df
}

standardise_self_lonlat <- function(df) {
  
  if (!all(c("londd", "latdd") %in% names(df))) {
    stop("Expected self-sampling columns 'londd' and 'latdd'.")
  }
  
  df |>
    mutate(
      X_lon = as.numeric(londd),
      Y_lat = as.numeric(latdd)
    )
}

# Use datetime directly for BOTH year and season.
# This avoids the earlier 2023 problem caused by missing month values.
prepare_self_datetime <- function(df) {
  
  if (!"datetime" %in% names(df)) {
    stop("Self-sampling dataset does not contain column 'datetime'.")
  }
  
  df |>
    mutate(
      datetime_parsed = datetime,
      year = as.integer(lubridate::year(datetime)),
      month_from_datetime = lubridate::month(datetime),
      season = case_when(
        month_from_datetime %in% c(9, 10, 11, 12) ~ "Autumn",
        month_from_datetime %in% c(1, 2, 3)        ~ "Winter",
        TRUE                                       ~ NA_character_
      )
    )
}

clean_sf_points <- function(x) {
  x <- x[!st_is_empty(x), ]
  x <- x[!is.na(st_geometry(x)), ]
  
  if (nrow(x) == 0) return(x)
  
  xy <- st_coordinates(x)
  keep <- is.finite(xy[, 1]) & is.finite(xy[, 2])
  
  x[keep, ]
}

# Count Plotter proxy occurrences in each 5 x 5 km cell.
# Presence is defined as >= 1 occurrence; no time-weight column is used.
points_to_grid_count <- function(pts_bng, grid_bng) {
  
  if (nrow(pts_bng) == 0) {
    return(
      grid_bng |>
        mutate(n_points = NA_real_)
    )
  }
  
  joined <- st_join(
    pts_bng,
    grid_bng |> dplyr::select(cell_id),
    join = st_within,
    left = FALSE
  )
  
  if (nrow(joined) == 0) {
    return(
      grid_bng |>
        mutate(n_points = NA_real_)
    )
  }
  
  counts <- joined |>
    st_drop_geometry() |>
    count(cell_id, name = "n_points")
  
  grid_bng |>
    left_join(counts, by = "cell_id") |>
    mutate(
      n_points = ifelse(
        is.finite(n_points) & n_points > 0,
        n_points,
        NA_real_
      )
    )
}

to_single_sf <- function(geom, crs_obj) {
  geom <- st_make_valid(geom)
  geom <- st_collection_extract(geom, "POLYGON")
  
  if (length(geom) == 0) return(NULL)
  
  st_as_sf(
    st_sfc(
      geom,
      crs = crs_obj
    )
  )
}

erase_land <- function(poly_sf, land_geom) {
  
  if (is.null(poly_sf) || nrow(poly_sf) == 0) {
    return(NULL)
  }
  
  diff_geom <- suppressWarnings(
    tryCatch(
      st_difference(
        st_union(st_geometry(poly_sf)),
        land_geom
      ) |>
        st_make_valid(),
      
      error = function(e) {
        st_union(st_geometry(poly_sf)) |>
          st_make_valid()
      }
    )
  )
  
  diff_geom <- st_collection_extract(diff_geom, "POLYGON")
  
  if (length(diff_geom) == 0) {
    return(NULL)
  }
  
  st_as_sf(
    st_sfc(
      diff_geom,
      crs = st_crs(poly_sf)
    )
  )
}

# ============================================================
# READ DATA
# ============================================================
message("\n===== READING DATA =====")

europe_land_ll <- st_read(
  europe_path,
  quiet = TRUE
) |>
  st_make_valid() |>
  st_transform(crs_ll)

scotland_land_ll <- st_read(
  scot_path,
  quiet = TRUE
) |>
  st_make_valid() |>
  st_transform(crs_ll)

plotter_aut <- read_csv(
  path_plotter_aut,
  show_col_types = FALSE
) |>
  standardise_plotter_lonlat() |>
  ensure_year()

plotter_win <- read_csv(
  path_plotter_win,
  show_col_types = FALSE
) |>
  standardise_plotter_lonlat() |>
  ensure_year()

message("\nPLOTTER AUTUMN COUNTS BY YEAR:")
print(
  plotter_aut |>
    count(year) |>
    arrange(year)
)

message("\nPLOTTER WINTER COUNTS BY YEAR:")
print(
  plotter_win |>
    count(year) |>
    arrange(year)
)

pts_plot_aut_bng <- st_as_sf(
  plotter_aut,
  coords = c("X_lon", "Y_lat"),
  crs = crs_ll,
  remove = FALSE
) |>
  st_transform(crs_bng) |>
  mutate(year = as.integer(year)) |>
  clean_sf_points()

pts_plot_win_bng <- st_as_sf(
  plotter_win,
  coords = c("X_lon", "Y_lat"),
  crs = crs_ll,
  remove = FALSE
) |>
  st_transform(crs_bng) |>
  mutate(year = as.integer(year)) |>
  clean_sf_points()

ss <- read_csv(
  ss_path,
  show_col_types = FALSE
) |>
  standardise_self_lonlat() |>
  prepare_self_datetime() |>
  filter(
    season %in% c("Autumn", "Winter"),
    is.finite(year),
    !is.na(X_lon),
    !is.na(Y_lat)
  )

message("\nSELF-SAMPLING COUNTS BY YEAR/SEASON:")
print(
  ss |>
    count(year, season) |>
    arrange(year, season)
)

message("\nPOSSIBLE DUPLICATED SELF POINTS (same year/season/X_lon/Y_lat):")
dup_ss <- ss |>
  count(
    year,
    season,
    X_lon,
    Y_lat,
    name = "n_dup"
  ) |>
  filter(n_dup > 1) |>
  arrange(desc(n_dup))

if (nrow(dup_ss) == 0) {
  message("No duplicated self points found under this check.")
} else {
  print(dup_ss)
}

pts_ss_bng <- st_as_sf(
  ss,
  coords = c("X_lon", "Y_lat"),
  crs = crs_ll,
  remove = FALSE
) |>
  st_transform(crs_bng) |>
  clean_sf_points()

# ============================================================
# DOMAIN = SEASON FRAME - LAND
# ============================================================
get_domain_bng <- function(
    season_name,
    europe_land_ll,
    scotland_land_ll
) {
  
  frame_ll <- make_season_frame_ll(
    season_name,
    frame_union_ll_base
  )
  
  frame_bng <- st_transform(
    frame_ll,
    crs_bng
  ) |>
    st_make_valid()
  
  land1_ll <- safe_intersection(
    europe_land_ll,
    frame_ll
  ) |>
    st_make_valid()
  
  land2_ll <- safe_intersection(
    scotland_land_ll,
    frame_ll
  ) |>
    st_make_valid()
  
  land_geom_ll <- st_union(
    c(
      st_geometry(land1_ll),
      st_geometry(land2_ll)
    )
  ) |>
    st_make_valid()
  
  land_bng <- st_transform(
    st_as_sf(land_geom_ll),
    crs_bng
  ) |>
    st_make_valid()
  
  land_geom <- st_union(
    st_geometry(land_bng)
  ) |>
    st_make_valid()
  
  domain_geom <- suppressWarnings(
    tryCatch(
      st_difference(
        st_union(
          st_geometry(
            st_as_sf(frame_bng)
          )
        ),
        land_geom
      ) |>
        st_make_valid(),
      
      error = function(e) {
        st_union(
          st_geometry(
            st_as_sf(frame_bng)
          )
        ) |>
          st_make_valid()
      }
    )
  )
  
  domain_geom <- st_collection_extract(
    domain_geom,
    "POLYGON"
  )
  
  if (length(domain_geom) == 0) {
    return(NULL)
  }
  
  list(
    domain_sf = st_as_sf(domain_geom),
    land_geom = land_geom,
    frame_sf  = st_as_sf(frame_bng)
  )
}

# ============================================================
# BUILD GRID-ONLY SHARED SEASONAL NULL
# ============================================================
build_grid_seasonal_null <- function(
    pts_plot_bng,
    domain_sf,
    land_geom,
    cell_m = 5000,
    grid_null_annual_buf_m = 2500,
    null_buffer_m = 5000,
    years_keep = 2020:2023
) {
  
  dom_bb <- st_as_sfc(
    st_bbox(domain_sf)
  )
  
  grid0 <- st_make_grid(
    dom_bb,
    cellsize = cell_m,
    square = TRUE,
    what = "polygons"
  ) |>
    st_as_sf() |>
    mutate(
      cell_id = row_number()
    ) |>
    st_make_valid()
  
  seasonal_grid <- suppressWarnings(
    tryCatch(
      st_intersection(
        grid0,
        domain_sf
      ),
      error = function(e) grid0
    )
  ) |>
    st_collection_extract("POLYGON") |>
    st_make_valid()
  
  if (nrow(seasonal_grid) == 0) {
    message("❌ Seasonal grid is empty.")
    return(NULL)
  }
  
  grid_parts <- list()
  
  for (yy in years_keep) {
    
    message(
      "GRID-ONLY NULL - Year: ",
      yy
    )
    
    pts_year <- pts_plot_bng |>
      filter(year == yy) |>
      clean_sf_points()
    
    message(
      "  Plotter points before domain crop: ",
      nrow(pts_year)
    )
    
    pts_year <- suppressWarnings(
      tryCatch(
        st_intersection(
          pts_year,
          domain_sf
        ),
        error = function(e) pts_year
      )
    )
    
    message(
      "  Plotter points inside domain: ",
      nrow(pts_year)
    )
    
    if (nrow(pts_year) < 1) {
      message(
        "  ⚠ No Plotter points - skipping year"
      )
      next
    }
    
    g_plot <- points_to_grid_count(
      pts_year,
      seasonal_grid
    )
    
    ids <- g_plot$cell_id[
      !is.na(g_plot$n_points)
    ]
    
    message(
      "  Occupied GRID cells: ",
      length(ids)
    )
    
    if (length(ids) > 0) {
      
      union_cells <- seasonal_grid |>
        filter(
          cell_id %in% ids
        )
      
      cell_union <- st_union(
        st_geometry(union_cells)
      ) |>
        st_buffer(
          dist = grid_null_annual_buf_m
        ) |>
        st_make_valid()
      
      cell_union_sf <- to_single_sf(
        cell_union,
        st_crs(seasonal_grid)
      )
      
      cell_union_sf <- erase_land(
        cell_union_sf,
        land_geom
      )
      
      if (!is.null(cell_union_sf)) {
        
        grid_parts[[as.character(yy)]] <-
          cell_union_sf
        
        message(
          "  ✔ Annual GRID envelope area (km²): ",
          round(
            km2(cell_union_sf),
            2
          )
        )
        
      } else {
        message(
          "  ⚠ GRID envelope disappeared after land removal"
        )
      }
    }
  }
  
  if (length(grid_parts) == 0) {
    message(
      "❌ No annual GRID envelopes available for seasonal NULL"
    )
    return(NULL)
  }
  
  grid_union <- st_union(
    do.call(
      c,
      lapply(
        grid_parts,
        st_geometry
      )
    )
  ) |>
    st_make_valid()
  
  message(
    "✔ Union GRID area before hull (km²): ",
    round(
      km2(
        st_as_sf(grid_union)
      ),
      2
    )
  )
  
  null_geom <- st_convex_hull(
    grid_union
  ) |>
    st_buffer(
      dist = null_buffer_m
    ) |>
    st_make_valid()
  
  null_geom <- suppressWarnings(
    tryCatch(
      st_intersection(
        null_geom,
        st_geometry(domain_sf)
      ) |>
        st_make_valid(),
      
      error = function(e) {
        null_geom
      }
    )
  )
  
  null_sf <- to_single_sf(
    null_geom,
    st_crs(domain_sf)
  )
  
  if (is.null(null_sf)) {
    message(
      "❌ GRID seasonal NULL collapsed after domain intersection"
    )
    return(NULL)
  }
  
  null_sf <- erase_land(
    null_sf,
    land_geom
  )
  
  if (is.null(null_sf)) {
    message(
      "❌ GRID seasonal NULL disappeared after land removal"
    )
    return(NULL)
  }
  
  message(
    "✔ GRID-only seasonal NULL area (km²): ",
    round(
      km2(null_sf),
      2
    )
  )
  
  list(
    null_poly = null_sf,
    seasonal_grid = seasonal_grid
  )
}

# ============================================================
# GRID MONTE CARLO TEST
# ============================================================
run_grid_full_test <- function(
    season_name,
    year_val,
    pts_plot_bng,
    pts_self_bng,
    domain_sf,
    land_geom,
    seasonal_grid,
    shared_null_poly,
    grid_foot_buffer_m = 5000,
    nperm = 999
) {
  
  pts_p <- pts_plot_bng |>
    filter(
      year == year_val
    ) |>
    clean_sf_points()
  
  pts_s <- pts_self_bng |>
    filter(
      year == year_val
    ) |>
    clean_sf_points()
  
  message(
    "  [GRID] Plotter proxy records raw: ",
    nrow(pts_p)
  )
  
  message(
    "  [GRID] Self-sampling hauls raw: ",
    nrow(pts_s)
  )
  
  if (
    nrow(pts_p) < 1 ||
    nrow(pts_s) < 1
  ) {
    message(
      "  [GRID] Skipped: no data"
    )
    return(NULL)
  }
  
  pts_p <- suppressWarnings(
    tryCatch(
      st_intersection(
        pts_p,
        domain_sf
      ),
      error = function(e) pts_p
    )
  )
  
  pts_s <- suppressWarnings(
    tryCatch(
      st_intersection(
        pts_s,
        domain_sf
      ),
      error = function(e) pts_s
    )
  )
  
  message(
    "  [GRID] Plotter proxy records in domain: ",
    nrow(pts_p)
  )
  
  message(
    "  [GRID] Self-sampling hauls in domain: ",
    nrow(pts_s)
  )
  
  if (
    nrow(pts_p) < 1 ||
    nrow(pts_s) < 1
  ) {
    message(
      "  [GRID] Skipped after domain crop"
    )
    return(NULL)
  }
  
  grid <- seasonal_grid
  
  if (nrow(grid) == 0) {
    return(NULL)
  }
  
  # Every occupied Plotter cell is included: n_points >= 1.
  g_plot <- points_to_grid_count(
    pts_p,
    grid
  )
  
  foot_cells <- g_plot$cell_id[
    !is.na(g_plot$n_points)
  ]
  
  message(
    "  [GRID] Occupied footprint cells: ",
    length(foot_cells)
  )
  
  if (length(foot_cells) == 0) {
    message(
      "  [GRID] No footprint cells"
    )
    return(NULL)
  }
  
  foot_sf <- g_plot |>
    filter(
      cell_id %in% foot_cells
    )
  
  foot_geom_raw <- st_union(
    st_geometry(foot_sf)
  ) |>
    st_make_valid()
  
  foot_geom <- st_buffer(
    foot_geom_raw,
    dist = grid_foot_buffer_m
  ) |>
    st_make_valid()
  
  foot_geom <- suppressWarnings(
    tryCatch(
      st_intersection(
        foot_geom,
        st_geometry(domain_sf)
      ) |>
        st_make_valid(),
      
      error = function(e) {
        foot_geom
      }
    )
  )
  
  foot_poly <- to_single_sf(
    foot_geom,
    st_crs(domain_sf)
  )
  
  if (is.null(foot_poly)) {
    message(
      "  [GRID] Footprint collapsed after intersection"
    )
    return(NULL)
  }
  
  foot_poly <- erase_land(
    foot_poly,
    land_geom
  )
  
  if (is.null(foot_poly)) {
    message(
      "  [GRID] Footprint disappeared after land removal"
    )
    return(NULL)
  }
  
  # Observed overlap
  inside_obs <- st_within(
    pts_s,
    foot_poly,
    sparse = FALSE
  )[, 1]
  
  k_obs   <- sum(inside_obs)
  n_hauls <- nrow(pts_s)
  prop_obs <- k_obs / n_hauls
  
  # Monte Carlo null distribution
  k_sim <- numeric(nperm)
  
  for (i in seq_len(nperm)) {
    
    sim_pts <- st_sample(
      shared_null_poly,
      size = n_hauls,
      type = "random"
    ) |>
      st_as_sf()
    
    inside_sim <- st_within(
      sim_pts,
      foot_poly,
      sparse = FALSE
    )[, 1]
    
    k_sim[i] <- sum(inside_sim)
  }
  
  mean_null <- mean(k_sim)
  sd_null   <- sd(k_sim)
  
  p_val <- (
    sum(k_sim >= k_obs) + 1
  ) / (
    nperm + 1
  )
  
  z <- ifelse(
    sd_null > 0,
    (k_obs - mean_null) / sd_null,
    NA_real_
  )
  
  domain_area <- sum(
    km2(domain_sf)
  )
  
  foot_area <- sum(
    km2(foot_poly)
  )
  
  # ==========================================================
  # PAPER TABLE — exact requested columns
  # ==========================================================
  data.frame(
    Season = season_name,
    Year = year_val,
    `Tracks proxy` = nrow(pts_p),
    Hauls = n_hauls,
    `Footprint area (km²)` = foot_area,
    `% area of domain` = ifelse(
      domain_area > 0,
      100 * foot_area / domain_area,
      NA_real_
    ),
    `Hauls inside` = k_obs,
    `% observed` = 100 * prop_obs,
    `Mean null` = mean_null,
    `% mean null` = 100 * (
      mean_null / n_hauls
    ),
    `Z-score` = z,
    `p-value` = p_val,
    check.names = FALSE
  )
}

# ============================================================
# RUN ALL + FRAME AUDIT
# ============================================================
res <- list()
frame_audit <- list()

for (
  current_season in c(
    "Autumn",
    "Winter"
  )
) {
  
  message(
    "\n===================="
  )
  
  message(
    "SEASON: ",
    current_season
  )
  
  message(
    "===================="
  )
  
  if (current_season == "Autumn") {
    pts_plot <- pts_plot_aut_bng
  } else {
    pts_plot <- pts_plot_win_bng
  }
  
  dom_obj <- get_domain_bng(
    season_name = current_season,
    europe_land_ll = europe_land_ll,
    scotland_land_ll = scotland_land_ll
  )
  
  if (is.null(dom_obj)) {
    message(
      "Empty domain for ",
      current_season,
      " — skipping."
    )
    next
  }
  
  domain_sf <- dom_obj$domain_sf
  land_geom <- dom_obj$land_geom
  frame_sf  <- dom_obj$frame_sf
  
  message(
    "Domain area (km²): ",
    round(
      sum(km2(domain_sf)),
      2
    )
  )
  
  pts_self <- pts_ss_bng[
    pts_ss_bng$season == current_season,
  ]
  
  message(
    "\nPlotter counts by year in ",
    current_season,
    ":"
  )
  
  print(
    pts_plot |>
      st_drop_geometry() |>
      count(year)
  )
  
  message(
    "\nSelf-sampling counts by year in ",
    current_season,
    ":"
  )
  
  print(
    pts_self |>
      st_drop_geometry() |>
      count(
        year,
        season
      )
  )
  
  # ----------------------------------------------------------
  # FRAME AUDIT
  # ----------------------------------------------------------
  for (yy in years_keep) {
    
    pts_year_raw <- pts_plot |>
      filter(
        year == yy
      ) |>
      clean_sf_points()
    
    pts_year_in_frame <- suppressWarnings(
      tryCatch(
        st_intersection(
          pts_year_raw,
          frame_sf
        ),
        error = function(e) {
          pts_year_raw
        }
      )
    )
    
    n_total <- nrow(
      pts_year_raw
    )
    
    n_inframe <- nrow(
      pts_year_in_frame
    )
    
    n_outframe <- n_total -
      n_inframe
    
    pct_outframe <- ifelse(
      n_total > 0,
      100 * n_outframe / n_total,
      NA_real_
    )
    
    frame_audit[[
      paste0(
        current_season,
        "_",
        yy
      )
    ]] <- data.frame(
      Season = current_season,
      Year = yy,
      `Plotter raw` = n_total,
      `Plotter in frame` = n_inframe,
      `Plotter excluded by frame` = n_outframe,
      `% plotter excluded by frame` = pct_outframe,
      check.names = FALSE
    )
    
    message(
      "[FRAME AUDIT] ",
      current_season,
      " ",
      yy,
      " | raw = ",
      n_total,
      " | in frame = ",
      n_inframe,
      " | excluded by frame = ",
      n_outframe
    )
  }
  
  # ----------------------------------------------------------
  # GRID-ONLY SEASONAL NULL
  # ----------------------------------------------------------
  shared_null_obj <- build_grid_seasonal_null(
    pts_plot_bng = pts_plot,
    domain_sf = domain_sf,
    land_geom = land_geom,
    cell_m = cell_m,
    grid_null_annual_buf_m = grid_null_annual_buf_m,
    null_buffer_m = null_buffer_m,
    years_keep = years_keep
  )
  
  if (is.null(shared_null_obj)) {
    message(
      "GRID-only seasonal null could not be built for ",
      current_season,
      "."
    )
    next
  }
  
  shared_null_poly <- shared_null_obj$null_poly
  seasonal_grid    <- shared_null_obj$seasonal_grid
  
  # ----------------------------------------------------------
  # YEARLY GRID MONTE CARLO TESTS
  # ----------------------------------------------------------
  for (yy in years_keep) {
    
    message(
      "\n-> ",
      current_season,
      " ",
      yy
    )
    
    r_grid <- run_grid_full_test(
      season_name = current_season,
      year_val = yy,
      pts_plot_bng = pts_plot,
      pts_self_bng = pts_self,
      domain_sf = domain_sf,
      land_geom = land_geom,
      seasonal_grid = seasonal_grid,
      shared_null_poly = shared_null_poly,
      grid_foot_buffer_m = grid_foot_buffer_m,
      nperm = nperm
    )
    
    if (!is.null(r_grid)) {
      res[[
        paste0(
          "GRID_",
          current_season,
          "_",
          yy
        )
      ]] <- r_grid
    }
  }
}

# ============================================================
# OUTPUT TABLES
# ============================================================
final_table <- bind_rows(
  res
) |>
  arrange(
    factor(
      Season,
      levels = c(
        "Autumn",
        "Winter"
      )
    ),
    Year
  )

frame_audit_table <- bind_rows(
  frame_audit
) |>
  arrange(
    factor(
      Season,
      levels = c(
        "Autumn",
        "Winter"
      )
    ),
    Year
  )

# ------------------------------------------------------------
# DISPLAY VERSION
# Same information as final_table, formatted like paper table.
# p = 0.001 is displayed as <=0.001 because with 999
# permutations this is the minimum attainable Monte Carlo p.
# ------------------------------------------------------------
paper_table <- final_table |>
  mutate(
    `Footprint area (km²)` = round(
      `Footprint area (km²)`,
      2
    ),
    `% area of domain` = round(
      `% area of domain`,
      2
    ),
    `% observed` = round(
      `% observed`,
      2
    ),
    `Mean null` = round(
      `Mean null`,
      5
    ),
    `% mean null` = round(
      `% mean null`,
      2
    ),
    `Z-score` = round(
      `Z-score`,
      2
    ),
    `p-value` = ifelse(
      `p-value` <= 0.001,
      "<=0.001",
      format(
        round(
          `p-value`,
          3
        ),
        nsmall = 3
      )
    )
  )

message(
  "\n===== PAPER MONTE CARLO TABLE ====="
)
print(
  paper_table,
  row.names = FALSE
)

message(
  "\n===== FRAME AUDIT TABLE ====="
)
print(
  frame_audit_table,
  digits = 10
)

# Raw numeric output: best version for archiving/reproducibility
write.csv(
  final_table,
  "C:/Users/SH01IQ/OneDrive - UHI/Desktop/validation_table_montecarlo_GRID_ONLY_raw_2020_2023.csv",
  row.names = FALSE
)

# Formatted output: matches the paper table structure
write.csv(
  paper_table,
  "C:/Users/SH01IQ/OneDrive - UHI/Desktop/validation_table_montecarlo_GRID_ONLY_paper_2020_2023.csv",
  row.names = FALSE
)

write.csv(
  frame_audit_table,
  "C:/Users/SH01IQ/OneDrive - UHI/Desktop/validation_table_frame_audit_GRID_ONLY_2020_2023.csv",
  row.names = FALSE
)

message(
  paste0(
    "\n✔ GRID-only Monte Carlo finished successfully.",
    "\n✔ No KDE is used anywhere in this paper version.",
    "\n✔ Raw and paper-formatted tables were saved to Desktop."
  )
)
