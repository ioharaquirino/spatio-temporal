# ============================================================
# STUDY AREA MAP — Chapter 2
# (labels + inset + blue shading only inside UK EEZ)
# FINAL tweaks requested:
#  1) Remove label "West of Scotland"
#  2) In symbol legend, change "UK territory" -> "Scotland"
#  3) Add ports: Lerwick, Fraserburgh, Peterhead (lighter dot + lighter labels)
#  4) Add legend item for ports: "Main Scottish ports"
#  5) Separate Fraserburgh/Peterhead labels with port-specific offsets
# ============================================================

# ==== PACKAGES ====
packages <- c("sf","dplyr","ggplot2","scales","ggspatial","patchwork","tibble")
to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(sf)
library(dplyr)
library(ggplot2)
library(scales)
library(ggspatial)
library(patchwork)
library(tibble)

sf_use_s2(FALSE)

# ==== CRS ====
crs_ll <- 4326

# ==== PATHS ====
shp_dir      <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/shapefiles_shetland_europe_scotland"
europe_path  <- file.path(shp_dir, "country.shp")
scot_path    <- file.path(shp_dir, "OS_Scotland_WGS84.shp")

ices_path    <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/chapter2/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp"
eez_path     <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/eez_UK/eez_UK/eez.shp"

# ==== OUTPUT ====
out_dir <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/chapter2_study_area_map"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_png <- file.path(out_dir, "Fig_StudyArea_UK_ScottishEEZ_inset_v5_ports_symbollegend.png")

# ============================================================
# FRAME / BBOX
# ============================================================
shetland_bbox_ll    <- c(xmin = -2.70, xmax = -0.20, ymin = 59.40, ymax = 61.30)
shetland_buffer_km  <- 30
norway_lon_cut      <- 6.00
iceland_lat_cut     <- 66.00

frame_ll <- st_bbox(c(
  xmin = -16.0,
  ymin = 48.0,
  xmax =  10.0,
  ymax =  67.0
), crs = st_crs(crs_ll))

frame_ll["xmax"] <- min(as.numeric(frame_ll["xmax"]), norway_lon_cut)
frame_ll["ymax"] <- min(as.numeric(frame_ll["ymax"]), iceland_lat_cut)
frame_sfc_ll <- st_as_sfc(frame_ll)

# ensure Shetland with buffer (buffer in meters -> 3857)
shet_bb   <- st_bbox(shetland_bbox_ll, crs = st_crs(crs_ll))
shet_sfc  <- st_as_sfc(shet_bb)
shet_buf  <- st_buffer(st_transform(shet_sfc, 3857), dist = shetland_buffer_km * 1000) |>
  st_transform(crs_ll)

frame_union <- st_union(st_make_valid(frame_sfc_ll), st_make_valid(shet_buf)) |>
  st_make_valid()

frame_box <- st_as_sf(st_as_sfc(st_bbox(frame_union, crs = st_crs(crs_ll))))

# ============================================================
# READ SHAPEFILES
# ============================================================
europe_land <- st_read(europe_path, quiet = TRUE) |>
  st_make_valid() |>
  st_transform(crs_ll)

scotland_land <- st_read(scot_path, quiet = TRUE) |>
  st_make_valid() |>
  st_transform(crs_ll)

ICES_Areas <- st_read(ices_path, quiet = TRUE) |>
  st_make_valid() |>
  st_transform(crs_ll)

EEZ_raw <- st_read(eez_path, quiet = TRUE) |>
  st_make_valid() |>
  st_transform(crs_ll)

# ============================================================
# EEZ: filter UK if attribute columns exist
# ============================================================
guess_cols <- intersect(
  names(EEZ_raw),
  c("TERRITORY1","TERRITORY","SOVEREIGN1","SOVEREIGN","COUNTRY","ADMIN","GEONAME","NAME","ISO3","ISO_TER1")
)

EEZ_UK <- EEZ_raw
if (length(guess_cols) > 0) {
  key <- EEZ_raw |>
    st_drop_geometry() |>
    transmute(.key = apply(across(all_of(guess_cols)), 1, function(x) {
      paste(na.omit(as.character(x)), collapse = " | ")
    }))
  EEZ_raw$.key <- key$.key
  
  EEZ_try <- EEZ_raw |>
    filter(grepl("United Kingdom|UK|Britain|Great Britain|GBR|Scotland|England|Wales|Northern Ireland",
                 .key, ignore.case = TRUE))
  
  if (nrow(EEZ_try) > 0) {
    EEZ_UK <- EEZ_try
    message("EEZ: filtered to UK (attribute match).")
  } else {
    message("EEZ: no clear UK attribute match — using full eez.shp geometry.")
  }
} else {
  message("EEZ: no typical country columns — using full eez.shp geometry.")
}

# ============================================================
# SAFE INTERSECTION (more stable crop/clip)
# ============================================================
safe_intersection <- function(x, y) {
  bb <- st_bbox(y)
  tryCatch(
    st_intersection(x, y),
    error = function(e) st_crop(x, bb)
  )
}

# crops
europe_crop <- safe_intersection(europe_land, frame_union) |> st_make_valid()
scot_crop   <- safe_intersection(scotland_land, frame_union) |> st_make_valid()
ices_crop   <- safe_intersection(ICES_Areas, frame_union) |> st_make_valid()
eez_crop    <- safe_intersection(EEZ_UK, frame_union) |> st_make_valid()

if (nrow(eez_crop) == 0) {
  stop("eez_crop is empty after clipping — check eez.shp and the frame extent.")
}

# ============================================================
# OCEAN BLUE ONLY INSIDE UK EEZ
# ============================================================
eez_union <- tryCatch(st_union(eez_crop) |> st_make_valid(),
                      error = function(e) st_make_valid(eez_crop))

if (is.null(eez_union) || length(eez_union) == 0 || any(st_is_empty(eez_union))) {
  ocean_uk <- st_as_sf(frame_union)
  message("Warning: EEZ clip is empty; using frame as ocean.")
} else {
  ocean_uk <- safe_intersection(st_as_sf(frame_union), st_as_sf(eez_union)) |>
    st_make_valid()
}

# ============================================================
# LABELS (removed "West of Scotland")
# ============================================================
labels <- tibble(
  label = c("North Sea", "Shetland Islands", "Faroe Islands", "Norway", "Ireland", "Iceland"),
  lon   = c(  1.6,        -1.2,             -6.8,           4.5,     -8.0,     -19.0),
  lat   = c( 56.8,         61.25,            62.8,          63.0,      53.6,      65.2)
)

# ============================================================
# PORTS (lighter dot + lighter label) with per-port offsets
# (Approximate coordinates in lon/lat; adjust if needed)
# ============================================================
ports <- tibble(
  name = c("Lerwick", "Fraserburgh", "Peterhead"),
  lon  = c(-1.144,    -2.005,        -1.785),
  lat  = c(60.155,     57.692,        57.505),
  # custom label offsets to avoid overlap (esp Fraserburgh vs Peterhead)
  dlon = c(0.22,       0.35,          0.25),
  dlat = c(0.16,       0.22,          0.05)
) |>
  mutate(lon_lab = lon + dlon,
         lat_lab = lat + dlat)

# ============================================================
# AESTHETICS
# ============================================================
sea_col        <- "#DCEEFF"                 # UK EEZ waters
land_fill      <- "grey96"                 # non-UK land
uk_land_fill   <- "grey78"                 # Scotland fill sample
land_border    <- "grey60"
eez_col        <- "#2C7FB8"
eez_lwd        <- 0.45
ices_col       <- scales::alpha("grey35", 0.18)

# Lighter port styling
port_col       <- "grey25"   # softer than pure black
port_text_col  <- "grey25"
port_pt_size   <- 1.7
port_txt_size  <- 2.4

# ============================================================
# MAIN MAP (NO CAPTION TEXT)
# ============================================================
p_main <- ggplot() +
  geom_sf(data = st_as_sf(frame_union), fill = "white", color = NA) +
  geom_sf(data = ocean_uk, fill = sea_col, color = NA) +
  geom_sf(data = eez_crop, fill = NA, color = eez_col, linewidth = eez_lwd) +
  geom_sf(data = ices_crop, fill = NA, color = ices_col, linewidth = 0.35) +
  geom_sf(data = europe_crop, fill = land_fill, color = land_border, linewidth = 0.30) +
  geom_sf(data = scot_crop,   fill = uk_land_fill, color = land_border, linewidth = 0.35) +
  
  # region/country labels
  geom_text(
    data = labels,
    aes(x = lon, y = lat, label = label),
    size = 3.2,
    lineheight = 0.95,
    color = "grey20"
  ) +
  
  # ports: lighter dot
  geom_point(
    data = ports,
    aes(x = lon, y = lat),
    color = port_col,
    size = port_pt_size
  ) +
  # ports: lighter name (offset)
  geom_text(
    data = ports,
    aes(x = lon_lab, y = lat_lab, label = name),
    size = port_txt_size,
    color = port_text_col,
    hjust = 0
  ) +
  
  coord_sf(
    xlim = c(st_bbox(frame_union)["xmin"], st_bbox(frame_union)["xmax"]),
    ylim = c(st_bbox(frame_union)["ymin"], st_bbox(frame_union)["ymax"]),
    expand = FALSE
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 8)
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.7
  ) +
  labs(title = NULL, subtitle = NULL, caption = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text  = element_text(size = 9, color = "grey25"),
    plot.margin = margin(10, 10, 10, 10)
  )

# ============================================================
# INSET MAP (RIGHT)
# ============================================================
inset_bbox <- st_bbox(c(
  xmin = -30, ymin = 40,
  xmax =  25, ymax = 72
), crs = st_crs(crs_ll)) |>
  st_as_sfc()

europe_inset <- safe_intersection(europe_land, inset_bbox) |> st_make_valid()

p_inset <- ggplot() +
  geom_sf(data = inset_bbox |> st_as_sf(), fill = "white", color = NA) +
  geom_sf(data = europe_inset, fill = "grey95", color = "grey50", linewidth = 0.25) +
  geom_sf(data = frame_box, fill = NA, color = "#D7301F", linewidth = 0.9) +
  coord_sf(
    xlim = c(st_bbox(inset_bbox)["xmin"], st_bbox(inset_bbox)["xmax"]),
    ylim = c(st_bbox(inset_bbox)["ymin"], st_bbox(inset_bbox)["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = "grey60", linewidth = 0.4),
    plot.margin = margin(4, 4, 4, 4)
  )

# Map with inset
p_map <- p_main +
  inset_element(p_inset, left = 0.76, bottom = 0.06, right = 0.995, top = 0.34, align_to = "full")

# ============================================================
# COMPACT SYMBOL LEGEND (CENTERED BELOW MAP)
#   [blue rectangle] UK EEZ waters
#   [grey circle]    Scotland
#   [small dot]      Main Scottish ports
# ============================================================
p_symlegend <- ggplot() +
  # Blue rectangle sample
  annotate("rect",
           xmin = 0.18, xmax = 0.24, ymin = 0.38, ymax = 0.62,
           fill = sea_col, color = "grey45", linewidth = 0.25) +
  annotate("text",
           x = 0.25, y = 0.50,
           label = "UK EEZ waters",
           hjust = 0, vjust = 0.5,
           size = 3.6, color = "grey20") +
  
  # Grey circle sample (Scotland)
  annotate("point",
           x = 0.52, y = 0.50,
           size = 6, shape = 16,
           color = uk_land_fill) +
  annotate("text",
           x = 0.54, y = 0.50,
           label = "Scotland",
           hjust = 0, vjust = 0.5,
           size = 3.6, color = "grey20") +
  
  # Ports sample
  annotate("point",
           x = 0.74, y = 0.50,
           size = 2.6, shape = 16,
           color = port_col) +
  annotate("text",
           x = 0.76, y = 0.50,
           label = "Main Scottish ports",
           hjust = 0, vjust = 0.5,
           size = 3.6, color = "grey20") +
  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  theme_void() +
  theme(
    plot.margin = margin(0, 10, 8, 10)
  )

# ============================================================
# STACK MAP + SYMBOL LEGEND
# ============================================================
final_plot <- p_map / p_symlegend +
  plot_layout(heights = c(1, 0.10))

# ============================================================
# EXPORT
# ============================================================
ggsave(out_png, final_plot, width = 10, height = 7.5, dpi = 400, bg = "white")
message("Map saved to: ", out_png)

