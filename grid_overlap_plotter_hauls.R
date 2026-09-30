# PLOTTER SEM FILTRO #

################################################################################
# GRID OVERLAP 5x5 km — PLOTTER + SELF-SAMPLING
# 2020–2023 where both datasets are available, by season
#
# SENSITIVITY VERSION: PLOTTER WITHOUT RECURRENCE FILTER
#
# METHOD
# -------------------------------------------------------------------------------
# Same 5 x 5 km grid for both datasets.
#
# PLOTTER:
# - Count proxy occurrences per grid cell
# - Display ALL occupied cells: n_points >= 1
#
# SELF-SAMPLING:
# - datetime format = YYYY-MM-DD HH:MM:SS
# - Year and month derived directly from datetime
# - Autumn = September–December
# - Winter = January–March
# - Count self-sampled hauls per grid cell
# - Display ALL cells containing at least one haul: n_points >= 1
#
# VISUALISATION
# - Plotter = filled grid cells
# - Self-sampling = larger transparent circles
#
# COMPUTE: EPSG:27700
# PLOT: EPSG:4326
# OUTPUT: TIFF, 300 dpi, LZW
################################################################################


rm(list = ls())


# ==============================================================================
# PACKAGES
# ==============================================================================

pkgs <- c(
  "sf",
  "dplyr",
  "readr",
  "lubridate",
  "ggplot2",
  "scales"
)

to_install <- pkgs[
  !pkgs %in% installed.packages()[, "Package"]
]

if (length(to_install) > 0) {
  install.packages(to_install)
}

library(sf)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)

sf_use_s2(FALSE)


# ==============================================================================
# CRS
# ==============================================================================

crs_bng <- 27700
crs_ll  <- 4326


# ==============================================================================
# CONFIGURATION
# ==============================================================================

cell_km <- 5
cell_m  <- cell_km * 1000

years_keep <- 2019:2023


# ==============================================================================
# SELF-SAMPLING VISUAL STYLE
# ==============================================================================

self_pt_size  <- 2.8
self_pt_alpha <- 0.45


# ==============================================================================
# MAP STYLE
# ==============================================================================

sea_col     <- "#FAFAFA"
land_fill   <- "grey92"
land_border <- "grey35"

ices_col <- scales::alpha(
  "grey55",
  0.30
)

grid_line_col <- scales::alpha(
  "white",
  0.35
)

grid_line_lwd <- 0.08

legend_text  <- 8
legend_title <- 9


# ==============================================================================
# COLOUR PALETTES
# ==============================================================================

plotter_cols <- c(
  "#FDE725",
  "#F8961E",
  "#E63946",
  "#5A189A"
)

self_cols <- c(
  "#081D58",
  "#253494",
  "#2C7FB8",
  "#41B6C4",
  "#7FCDBB"
)


# ==============================================================================
# PATHS
# ==============================================================================

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


# ------------------------------------------------------------------------------
# PLOTTER DATA
# ------------------------------------------------------------------------------

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


# ------------------------------------------------------------------------------
# SELF-SAMPLING DATA
# ------------------------------------------------------------------------------

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


# ------------------------------------------------------------------------------
# ICES AREAS
# ------------------------------------------------------------------------------

ices_path <- paste0(
  "C:/Users/SH01IQ/OneDrive - UHI/Desktop/",
  "iohara's phd/PhD_chapters/",
  "Chapter 2 - fishing areas changes/",
  "chapter2/ICES_areas/",
  "ICES_Areas_20160601_cut_dense_3857.shp"
)


# ==============================================================================
# OUTPUT
# ==============================================================================

desktop_dir <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop"

# Separate folder so this test cannot overwrite the >1 version
out_root <- file.path(
  desktop_dir,
  "grid_overlap_5km_plotter_ALL_cells"
)

out_dir_aut <- file.path(
  out_root,
  "autumn"
)

out_dir_win <- file.path(
  out_root,
  "winter"
)

dir.create(
  out_dir_aut,
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  out_dir_win,
  showWarnings = FALSE,
  recursive = TRUE
)

run_stamp <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

message(
  "OUTPUT FOLDERS:\n- ",
  out_dir_aut,
  "\n- ",
  out_dir_win
)

message(
  "RUN STAMP: ",
  run_stamp
)


# ==============================================================================
# SEASON BBOX
# ==============================================================================

# Autumn
autumn_ymin_core <- 57.5
autumn_ymax      <- 61.0
autumn_xmin      <- -5.0
autumn_xmax      <- 3.0

autumn_lat_buffer_deg <- 0.20


# Winter
winter_ymin <- 54.0
winter_ymax <- 62.0
winter_xmin <- -16.0
winter_xmax <- 1.5


make_season_frame_ll <- function(season_name) {
  
  if (tolower(season_name) == "autumn") {
    
    xmin <- autumn_xmin
    xmax <- autumn_xmax
    
    ymin <- autumn_ymin_core -
      autumn_lat_buffer_deg
    
    ymax <- autumn_ymax
    
  } else if (tolower(season_name) == "winter") {
    
    xmin <- winter_xmin
    xmax <- winter_xmax
    ymin <- winter_ymin
    ymax <- winter_ymax
    
  } else {
    
    stop(
      "Season must be Autumn or Winter."
    )
  }
  
  st_bbox(
    c(
      xmin = xmin,
      ymin = ymin,
      xmax = xmax,
      ymax = ymax
    ),
    crs = st_crs(crs_ll)
  ) |>
    st_as_sfc() |>
    st_make_valid()
}


# ==============================================================================
# HELPERS — PLOTTER
# ==============================================================================

parse_date_multi <- function(x) {
  
  lubridate::parse_date_time(
    as.character(x),
    orders = c(
      "dmy HMS",
      "ymd HMS",
      "mdy HMS",
      "dmy HM",
      "ymd HM",
      "mdy HM",
      "dmy",
      "ymd",
      "mdy"
    ),
    quiet = TRUE,
    tz = "UTC"
  )
}


ensure_year <- function(df) {
  
  year_is_valid <-
    "year" %in% names(df) &&
    suppressWarnings(
      max(
        as.integer(df$year),
        na.rm = TRUE
      )
    ) > 1900
  
  if (!year_is_valid) {
    
    if ("date" %in% names(df)) {
      
      df <- df |>
        mutate(
          date_parsed =
            parse_date_multi(date),
          
          year =
            lubridate::year(
              date_parsed
            )
        )
      
    } else if ("datetime" %in% names(df)) {
      
      df <- df |>
        mutate(
          datetime_parsed =
            parse_date_multi(datetime),
          
          year =
            lubridate::year(
              datetime_parsed
            )
        )
      
    } else {
      
      stop(
        paste0(
          "CSV without a valid 'year' column ",
          "and without 'date/datetime' from which ",
          "year can be derived."
        )
      )
    }
  }
  
  df |>
    mutate(
      year = as.integer(year)
    )
}


standardise_plotter_lonlat <- function(df) {
  
  if (
    all(
      c(
        "X_lon",
        "Y_lat"
      ) %in% names(df)
    )
  ) {
    
    df <- df |>
      mutate(
        X_lon = as.numeric(X_lon),
        Y_lat = as.numeric(Y_lat)
      )
    
  } else if (
    all(
      c(
        "lon",
        "lat"
      ) %in% names(df)
    )
  ) {
    
    df <- df |>
      mutate(
        X_lon = as.numeric(lon),
        Y_lat = as.numeric(lat)
      )
    
  } else {
    
    stop(
      paste0(
        "Expected Plotter coordinate columns ",
        "'X_lon'/'Y_lat' or 'lon'/'lat'."
      )
    )
  }
  
  df
}


# ==============================================================================
# HELPERS — SELF-SAMPLING
# ==============================================================================

standardise_self_lonlat <- function(df) {
  
  if (
    !all(
      c(
        "londd",
        "latdd"
      ) %in% names(df)
    )
  ) {
    
    stop(
      paste0(
        "Expected self-sampling columns ",
        "'londd' and 'latdd'."
      )
    )
  }
  
  df |>
    mutate(
      X_lon = as.numeric(londd),
      Y_lat = as.numeric(latdd)
    )
}


# ------------------------------------------------------------------------------
# SELF-SAMPLING DATE/TIME
#
# Actual format:
# YYYY-MM-DD HH:MM:SS
#
# Example:
# 2023-10-16 16:20:00
#
# Year and month are therefore derived using ymd_hms().
# ------------------------------------------------------------------------------

prepare_self_datetime <- function(df) {
  
  if (!"datetime" %in% names(df)) {
    
    stop(
      "Self-sampling dataset does not contain column 'datetime'."
    )
  }
  
  df <- df |>
    mutate(
      
      datetime_parsed =
        lubridate::ymd_hms(
          as.character(datetime),
          quiet = TRUE,
          tz = "UTC"
        ),
      
      year_from_datetime =
        lubridate::year(
          datetime_parsed
        ),
      
      month_from_datetime =
        lubridate::month(
          datetime_parsed
        ),
      
      season = case_when(
        
        month_from_datetime %in% c(
          9,
          10,
          11,
          12
        ) ~ "Autumn",
        
        month_from_datetime %in% c(
          1,
          2,
          3
        ) ~ "Winter",
        
        TRUE ~ "None"
      )
    )
  
  n_failed <- sum(
    is.na(df$datetime_parsed)
  )
  
  if (n_failed > 0) {
    
    warning(
      paste0(
        n_failed,
        " self-sampling datetime value(s) could not be parsed ",
        "as YYYY-MM-DD HH:MM:SS."
      )
    )
  }
  
  df |>
    mutate(
      year = as.integer(
        year_from_datetime
      )
    )
}


# ==============================================================================
# GENERAL SF HELPERS
# ==============================================================================

clean_sf_points <- function(x) {
  
  x <- x[
    !sf::st_is_empty(x),
  ]
  
  x <- x[
    !is.na(
      sf::st_geometry(x)
    ),
  ]
  
  xy <- sf::st_coordinates(x)
  
  keep <-
    is.finite(xy[, 1]) &
    is.finite(xy[, 2])
  
  x[keep, ]
}


safe_intersection <- function(
    x,
    y
) {
  
  bb <- st_bbox(y)
  
  suppressWarnings(
    tryCatch(
      
      st_intersection(
        x,
        y
      ),
      
      error = function(e) {
        
        st_crop(
          x,
          bb
        )
      }
    )
  )
}


# ==============================================================================
# GRID
# ==============================================================================

make_aligned_grid_bng <- function(
    frame_bng,
    cell_m = 5000
) {
  
  bb <- st_bbox(frame_bng)
  
  xmin <-
    floor(
      as.numeric(bb["xmin"]) /
        cell_m
    ) * cell_m
  
  ymin <-
    floor(
      as.numeric(bb["ymin"]) /
        cell_m
    ) * cell_m
  
  xmax <-
    ceiling(
      as.numeric(bb["xmax"]) /
        cell_m
    ) * cell_m
  
  ymax <-
    ceiling(
      as.numeric(bb["ymax"]) /
        cell_m
    ) * cell_m
  
  bb_sfc <- st_as_sfc(
    st_bbox(
      c(
        xmin = xmin,
        ymin = ymin,
        xmax = xmax,
        ymax = ymax
      ),
      crs = st_crs(frame_bng)
    )
  )
  
  st_make_grid(
    bb_sfc,
    cellsize = cell_m,
    what = "polygons",
    square = TRUE
  ) |>
    st_as_sf() |>
    mutate(
      cell_id = row_number()
    ) |>
    st_intersection(
      st_make_valid(
        frame_bng
      )
    ) |>
    st_make_valid()
}


# ==============================================================================
# COUNT RECORDS PER CELL
#
# Plotter:
# n_points = number of proxy occurrences
#
# Self-sampling:
# n_points = number of self-sampled hauls
# ==============================================================================

points_to_grid_count <- function(
    pts_bng,
    grid_bng
) {
  
  if (nrow(pts_bng) == 0) {
    
    return(
      grid_bng |>
        mutate(
          n_points = NA_real_
        )
    )
  }
  
  joined <- st_join(
    pts_bng,
    grid_bng |>
      dplyr::select(cell_id),
    join = st_within,
    left = FALSE
  )
  
  if (nrow(joined) == 0) {
    
    return(
      grid_bng |>
        mutate(
          n_points = NA_real_
        )
    )
  }
  
  counts <- joined |>
    st_drop_geometry() |>
    count(
      cell_id,
      name = "n_points"
    )
  
  grid_bng |>
    left_join(
      counts,
      by = "cell_id"
    ) |>
    mutate(
      n_points = ifelse(
        is.finite(n_points) &
          n_points > 0,
        n_points,
        NA_real_
      )
    )
}


# ==============================================================================
# READ BASE MAPS
# ==============================================================================

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


ICES_ll <- st_read(
  ices_path,
  quiet = TRUE
) |>
  st_make_valid() |>
  st_transform(crs_ll)


# ==============================================================================
# READ PLOTTER DATA
# ==============================================================================

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


message(
  "Plotter Autumn years: ",
  paste(
    range(
      plotter_aut$year,
      na.rm = TRUE
    ),
    collapse = "–"
  )
)


message(
  "Plotter Winter years: ",
  paste(
    range(
      plotter_win$year,
      na.rm = TRUE
    ),
    collapse = "–"
  )
)


pts_plot_aut_bng <- st_as_sf(
  plotter_aut,
  coords = c(
    "X_lon",
    "Y_lat"
  ),
  crs = crs_ll,
  remove = FALSE
) |>
  st_transform(crs_bng) |>
  clean_sf_points()


pts_plot_win_bng <- st_as_sf(
  plotter_win,
  coords = c(
    "X_lon",
    "Y_lat"
  ),
  crs = crs_ll,
  remove = FALSE
) |>
  st_transform(crs_bng) |>
  clean_sf_points()


# ==============================================================================
# READ + PREPARE SELF-SAMPLING
# ==============================================================================

ss <- read_csv(
  ss_path,
  show_col_types = FALSE
) |>
  standardise_self_lonlat() |>
  prepare_self_datetime() |>
  filter(
    season %in% c(
      "Autumn",
      "Winter"
    ),
    is.finite(year),
    !is.na(X_lon),
    !is.na(Y_lat)
  )


# ------------------------------------------------------------------------------
# DIAGNOSTICS
# ------------------------------------------------------------------------------

message(
  "Self-sampling years after datetime processing: ",
  paste(
    sort(
      unique(
        ss$year
      )
    ),
    collapse = ", "
  )
)


message(
  "\nSelf-sampling records by year and season:"
)

print(
  table(
    ss$year,
    ss$season
  )
)


# ------------------------------------------------------------------------------
# CONVERT SELF-SAMPLING TO SF
# ------------------------------------------------------------------------------

pts_ss_bng <- st_as_sf(
  ss,
  coords = c(
    "X_lon",
    "Y_lat"
  ),
  crs = crs_ll,
  remove = FALSE
) |>
  st_transform(crs_bng) |>
  clean_sf_points()


# ==============================================================================
# MAIN FUNCTION
# ==============================================================================

run_season <- function(
    season_name,
    pts_plot_bng,
    pts_self_bng,
    out_dir
) {
  
  message(
    "\n============================"
  )
  
  message(
    "SEASON: ",
    season_name
  )
  
  message(
    "============================\n"
  )
  
  
  # ---------------------------------------------------------------------------
  # FRAME
  # ---------------------------------------------------------------------------
  
  frame_ll <-
    make_season_frame_ll(
      season_name
    )
  
  frame_bbox_ll <-
    st_bbox(
      frame_ll
    )
  
  xmin_ll <-
    as.numeric(
      frame_bbox_ll["xmin"]
    )
  
  xmax_ll <-
    as.numeric(
      frame_bbox_ll["xmax"]
    )
  
  ymin_ll <-
    as.numeric(
      frame_bbox_ll["ymin"]
    )
  
  ymax_ll <-
    as.numeric(
      frame_bbox_ll["ymax"]
    )
  
  frame_bng <-
    st_transform(
      frame_ll,
      crs_bng
    ) |>
    st_make_valid()
  
  
  # ---------------------------------------------------------------------------
  # CROP BASE MAPS
  # ---------------------------------------------------------------------------
  
  europe_crop_ll <-
    safe_intersection(
      europe_land_ll,
      frame_ll
    ) |>
    st_make_valid()
  
  scot_crop_ll <-
    safe_intersection(
      scotland_land_ll,
      frame_ll
    ) |>
    st_make_valid()
  
  ices_crop_ll <-
    safe_intersection(
      ICES_ll,
      frame_ll
    ) |>
    st_make_valid()
  
  ocean_ll <-
    st_as_sf(
      frame_ll
    )
  
  
  # ---------------------------------------------------------------------------
  # ICES DOMAIN IN BNG
  # ---------------------------------------------------------------------------
  
  ices_crop_bng <-
    st_transform(
      ices_crop_ll,
      crs_bng
    ) |>
    st_make_valid()
  
  ices_crop_bng <-
    tryCatch(
      
      st_buffer(
        ices_crop_bng,
        0
      ),
      
      error = function(e) {
        ices_crop_bng
      }
    )
  
  ices_crop_bng <-
    st_collection_extract(
      ices_crop_bng,
      "POLYGON"
    )
  
  ices_crop_bng <-
    ices_crop_bng[
      !st_is_empty(
        ices_crop_bng
      ),
    ]
  
  if (
    nrow(
      ices_crop_bng
    ) == 0
  ) {
    
    stop(
      paste0(
        "ICES crop empty for ",
        season_name
      )
    )
  }
  
  ices_union_bng <-
    tryCatch(
      
      st_union(
        st_combine(
          ices_crop_bng
        )
      ) |>
        st_make_valid(),
      
      error = function(e) {
        
        st_make_valid(
          ices_crop_bng
        )
      }
    )
  
  
  # ---------------------------------------------------------------------------
  # CREATE 5 x 5 KM GRID
  # ---------------------------------------------------------------------------
  
  grid_bng <-
    make_aligned_grid_bng(
      frame_bng,
      cell_m = cell_m
    )
  
  grid_bng <-
    grid_bng[
      lengths(
        st_intersects(
          grid_bng,
          ices_union_bng
        )
      ) > 0,
    ]
  
  if (
    nrow(
      grid_bng
    ) == 0
  ) {
    
    stop(
      paste0(
        "Grid empty for ",
        season_name
      )
    )
  }
  
  grid_ll <-
    st_transform(
      grid_bng,
      crs_ll
    )
  
  
  # ---------------------------------------------------------------------------
  # COMMON YEARS
  # ---------------------------------------------------------------------------
  
  yrs_plot <-
    sort(
      unique(
        pts_plot_bng$year
      )
    )
  
  yrs_self <-
    sort(
      unique(
        pts_self_bng$year
      )
    )
  
  yrs <-
    intersect(
      years_keep,
      intersect(
        yrs_plot,
        yrs_self
      )
    )
  
  message(
    "Years to map for ",
    season_name,
    ": ",
    paste(
      yrs,
      collapse = ", "
    )
  )
  
  if (length(yrs) == 0) {
    
    stop(
      paste0(
        "No common years for ",
        season_name
      )
    )
  }
  
  
  # ===========================================================================
  # YEAR LOOP
  # ===========================================================================
  
  for (yy in yrs) {
    
    message(
      sprintf(
        "-> %s %d",
        season_name,
        yy
      )
    )
    
    
    # -------------------------------------------------------------------------
    # SELECT YEAR
    # -------------------------------------------------------------------------
    
    pts_p <-
      pts_plot_bng |>
      filter(
        year == yy
      ) |>
      clean_sf_points()
    
    pts_s <-
      pts_self_bng |>
      filter(
        year == yy
      ) |>
      clean_sf_points()
    
    message(
      sprintf(
        "   Input records: Plotter=%d | Self-sampling=%d",
        nrow(pts_p),
        nrow(pts_s)
      )
    )
    
    if (
      nrow(pts_p) < 1 ||
      nrow(pts_s) < 1
    ) {
      
      message(
        "   Insufficient data. Skipping."
      )
      
      next
    }
    
    
    # -------------------------------------------------------------------------
    # CLIP TO ICES DOMAIN
    # -------------------------------------------------------------------------
    
    pts_p_clip <-
      suppressWarnings(
        tryCatch(
          
          st_intersection(
            pts_p,
            ices_union_bng
          ),
          
          error = function(e) {
            pts_p
          }
        )
      )
    
    pts_s_clip <-
      suppressWarnings(
        tryCatch(
          
          st_intersection(
            pts_s,
            ices_union_bng
          ),
          
          error = function(e) {
            pts_s
          }
        )
      )
    
    if (nrow(pts_p_clip) < 1) {
      pts_p_clip <- pts_p
    }
    
    if (nrow(pts_s_clip) < 1) {
      pts_s_clip <- pts_s
    }
    
    
    # -------------------------------------------------------------------------
    # COUNT RECORDS PER GRID CELL
    # -------------------------------------------------------------------------
    
    g_plot <-
      points_to_grid_count(
        pts_p_clip,
        grid_bng
      )
    
    g_self <-
      points_to_grid_count(
        pts_s_clip,
        grid_bng
      )
    
    
    # -------------------------------------------------------------------------
    # PLOTTER
    #
    # NO RECURRENCE FILTER:
    # display every occupied Plotter cell.
    # -------------------------------------------------------------------------
    
    g_plot_draw <-
      g_plot |>
      filter(
        !is.na(n_points),
        n_points >= 1
      )
    
    
    # -------------------------------------------------------------------------
    # SELF-SAMPLING
    #
    # Display every cell containing at least one observed haul.
    # -------------------------------------------------------------------------
    
    g_self_draw <-
      g_self |>
      filter(
        !is.na(n_points),
        n_points >= 1
      )
    
    
    message(
      sprintf(
        paste0(
          "   Cells displayed: ",
          "Plotter (ALL >=1)=%d | ",
          "Self-sampling (ALL >=1)=%d"
        ),
        nrow(g_plot_draw),
        nrow(g_self_draw)
      )
    )
    
    
    if (
      nrow(g_plot_draw) == 0 &&
      nrow(g_self_draw) == 0
    ) {
      
      message(
        "   No cells to display. Skipping."
      )
      
      next
    }
    
    
    # -------------------------------------------------------------------------
    # TRANSFORM FOR PLOTTING
    # -------------------------------------------------------------------------
    
    g_plot_ll_draw <-
      st_transform(
        g_plot_draw,
        crs_ll
      )
    
    self_pts_ll <-
      suppressWarnings(
        st_point_on_surface(
          st_transform(
            g_self_draw,
            crs_ll
          )
        )
      )
    
    
    # =========================================================================
    # BASE MAP
    # =========================================================================
    
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
      
      geom_sf(
        data = ices_crop_ll,
        fill = NA,
        color = ices_col,
        linewidth = 0.18
      )
    
    
    # =========================================================================
    # PLOTTER — ALL OCCUPIED CELLS
    # =========================================================================
    
    if (nrow(g_plot_ll_draw) > 0) {
      
      p <- p +
        
        geom_sf(
          data = g_plot_ll_draw,
          aes(
            fill = n_points
          ),
          color = NA,
          alpha = 0.85
        ) +
        
        scale_fill_gradientn(
          colours = plotter_cols,
          trans = "sqrt",
          name = "Plotter Data",
          na.value = NA,
          guide = guide_colorbar(
            order = 1
          )
        )
    }
    
    
    # =========================================================================
    # SELF-SAMPLING — ALL HAUL CELLS
    # =========================================================================
    
    if (nrow(self_pts_ll) > 0) {
      
      p <- p +
        
        geom_sf(
          data = self_pts_ll,
          aes(
            color = n_points
          ),
          shape = 16,
          size = self_pt_size,
          alpha = self_pt_alpha
        ) +
        
        scale_color_gradientn(
          colours = rev(
            self_cols
          ),
          trans = "sqrt",
          name = "Self-sampling Data",
          na.value = NA,
          guide = guide_colorbar(
            order = 2
          )
        )
    }
    
    
    # =========================================================================
    # MAP LAYOUT
    # =========================================================================
    
    p <- p +
      
      coord_sf(
        crs = st_crs(crs_ll),
        
        xlim = c(
          xmin_ll,
          xmax_ll
        ),
        
        ylim = c(
          ymin_ll,
          ymax_ll
        ),
        
        expand = FALSE
      ) +
      
      labs(
        x = "Longitude",
        y = "Latitude"
      ) +
      
      annotate(
        "text",
        
        x =
          xmax_ll -
          0.025 *
          (xmax_ll - xmin_ll),
        
        y =
          ymin_ll +
          0.245 *
          (ymax_ll - ymin_ll),
        
        label =
          as.character(yy),
        
        hjust = 1,
        vjust = 0,
        
        size = 3.3,
        fontface = "bold"
      ) +
      
      theme_minimal(
        base_size = 11
      ) +
      
      theme(
        
        legend.title =
          element_text(
            size = legend_title
          ),
        
        legend.text =
          element_text(
            size = legend_text
          ),
        
        legend.position =
          "inside",
        
        legend.position.inside =
          c(
            0.97,
            0.07
          ),
        
        legend.box =
          "vertical",
        
        legend.justification =
          c(
            1,
            0
          ),
        
        legend.background =
          element_rect(
            fill = "white",
            colour = NA
          ),
        
        legend.box.background =
          element_rect(
            fill = "white",
            colour = NA
          ),
        
        legend.key.height =
          grid::unit(
            0.35,
            "cm"
          ),
        
        legend.key.width =
          grid::unit(
            0.35,
            "cm"
          ),
        
        panel.grid.minor =
          element_blank(),
        
        panel.grid.major =
          element_line(
            color =
              scales::alpha(
                "grey40",
                0.12
              ),
            linewidth = 0.3
          ),
        
        panel.background =
          element_rect(
            fill = sea_col,
            colour = NA
          ),
        
        plot.background =
          element_rect(
            fill = "white",
            colour = NA
          ),
        
        plot.margin =
          margin(
            3,
            3,
            3,
            3
          )
      )
    
    
    # =========================================================================
    # EXPORT
    # =========================================================================
    
    tiff_out <- file.path(
      out_dir,
      sprintf(
        paste0(
          "GRID_overlap_%s_%d_",
          "cell%dkm_plotter_ALL_self_ALL_%s.tiff"
        ),
        tolower(
          season_name
        ),
        yy,
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
    
    message(
      "   ✔ ",
      tiff_out
    )
  }
  
  
  message(
    "✅ Season completed: ",
    season_name
  )
}


# ==============================================================================
# AUTUMN
# ==============================================================================

pts_self_aut_bng <-
  pts_ss_bng |>
  filter(
    season == "Autumn"
  )


message(
  "Self-sampling Autumn years: ",
  paste(
    sort(
      unique(
        pts_self_aut_bng$year
      )
    ),
    collapse = ", "
  )
)


run_season(
  "Autumn",
  pts_plot_aut_bng,
  pts_self_aut_bng,
  out_dir_aut
)


# ==============================================================================
# WINTER
# ==============================================================================

pts_self_win_bng <-
  pts_ss_bng |>
  filter(
    season == "Winter"
  )


message(
  "Self-sampling Winter years: ",
  paste(
    sort(
      unique(
        pts_self_win_bng$year
      )
    ),
    collapse = ", "
  )
)


run_season(
  "Winter",
  pts_plot_win_bng,
  pts_self_win_bng,
  out_dir_win
)


# ==============================================================================
# DONE
# ==============================================================================

message(
  paste0(
    "\n🎯 Done. Check output folders:\n- ",
    out_dir_aut,
    "\n- ",
    out_dir_win
  )
)










