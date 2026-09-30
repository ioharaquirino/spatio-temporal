
# ============================================================
# Coverage figures: heatmaps (pings + vessels) + yearly line plot
# Data: tracks+vms+season
# ============================================================

rm(list = ls())

library(tidyverse)
library(lubridate)
library(scales)

# ---- 1) Paths ----
data_dir  <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets"
base_name <- "tracks+vms+season"

out_dir <- file.path(data_dir, "figures_coverage")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 2) Read file (tries multiple extensions) ----
candidates <- c(
  file.path(data_dir, paste0(base_name, ".rds")),
  file.path(data_dir, paste0(base_name, ".csv")),
  file.path(data_dir, paste0(base_name, ".csv.gz")),
  file.path(data_dir, paste0(base_name, ".rdata")),
  file.path(data_dir, paste0(base_name, ".RData")),
  file.path(data_dir, paste0(base_name, ".feather")),
  file.path(data_dir, paste0(base_name, ".parquet"))
)

in_file <- candidates[file.exists(candidates)][1]
if (is.na(in_file)) stop("Não achei o arquivo. Procurei: ", paste(basename(candidates), collapse = ", "))

message("Lendo: ", in_file)

read_any <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  if (ext == "rds") {
    readRDS(path)
    
  } else if (ext %in% c("csv", "gz")) {
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
    
  } else if (ext %in% c("rdata", "rda")) {
    e <- new.env()
    load(path, envir = e)
    obj_name <- ls(e)[1]
    e[[obj_name]]
    
  } else if (ext == "feather") {
    if (!requireNamespace("arrow", quietly = TRUE)) stop("Instale o pacote 'arrow' para ler feather.")
    arrow::read_feather(path)
    
  } else if (ext == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) stop("Instale o pacote 'arrow' para ler parquet.")
    arrow::read_parquet(path)
    
  } else {
    stop("Extensão não suportada: ", ext)
  }
}

df <- read_any(in_file) %>% as_tibble()

# ---- 3) Set key columns explicitly (based on your dataset) ----
vessel_col <- "VE_ID"
year_col   <- "year"
month_col  <- "month"
date_col   <- "datetime"  # fallback if year/month missing

# ---- 4) Ensure year/month exist (fallback to datetime if needed) ----
df2 <- df

if (!all(c(year_col, month_col) %in% names(df2))) {
  if (!(date_col %in% names(df2))) {
    stop("Não encontrei colunas year/month e também não achei 'datetime' para derivar.")
  }
  
  dt <- df2[[date_col]]
  if (!inherits(dt, c("POSIXct", "POSIXt", "Date"))) {
    dt <- suppressWarnings(lubridate::ymd_hms(dt, quiet = TRUE))
    if (all(is.na(dt))) dt <- suppressWarnings(lubridate::ymd(dt, quiet = TRUE))
    if (all(is.na(dt))) dt <- suppressWarnings(lubridate::dmy_hms(dt, quiet = TRUE))
    if (all(is.na(dt))) dt <- suppressWarnings(lubridate::dmy(dt, quiet = TRUE))
  }
  if (all(is.na(dt))) stop("Falhei convertendo a coluna datetime: ", date_col)
  
  df2 <- df2 %>%
    mutate(
      year  = lubridate::year(dt),
      month = lubridate::month(dt)
    )
}

# ---- 5) Clean + filter to thesis period ----
years_target <- 1999:2023

df2 <- df2 %>%
  mutate(
    Year  = as.integer(.data[[year_col]]),
    Month = as.integer(.data[[month_col]])
  ) %>%
  filter(!is.na(Year), !is.na(Month), Month >= 1, Month <= 12) %>%
  filter(Year %in% years_target)

if (!(vessel_col %in% names(df2))) stop("A coluna de vessel definida não existe: ", vessel_col)

# ---- 6) Monthly summaries for heatmaps ----
pings_m_y <- df2 %>%
  count(Year, Month, name = "Pings")

vessels_m_y <- df2 %>%
  group_by(Year, Month) %>%
  summarise(Vessels = n_distinct(.data[[vessel_col]]), .groups = "drop")

# Full grid (so missing month-year combinations appear as blank tiles)
full_grid <- expand_grid(
  Year = years_target,
  Month = 1:12
)

pings_m_y   <- full_grid %>% left_join(pings_m_y,   by = c("Year", "Month"))
vessels_m_y <- full_grid %>% left_join(vessels_m_y, by = c("Year", "Month"))

# Heatmap Y-axis order: 1999 at TOP, 2023 at BOTTOM
year_levels  <- rev(years_target)
month_levels <- 1:12
month_labels <- month.abb

prep_heat_df <- function(d, value_col) {
  d %>%
    mutate(
      YearF  = factor(Year, levels = year_levels),
      MonthF = factor(Month, levels = month_levels, labels = month_labels),
      Value  = .data[[value_col]]
    )
}

p1_df <- prep_heat_df(pings_m_y, "Pings")
p2_df <- prep_heat_df(vessels_m_y, "Vessels")

# ---- 7) Plot styling ----
heat_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 11),
    plot.title = element_text(face = "bold"),
    legend.title = element_text(size = 10)
  )

# Palette: low -> high (yellow -> orange -> red -> wine)
heat_cols <- c(
  "#FEE08B",  # yellow
  "#FDAE61",  # orange
  "#F46D43",  # red
  "#7F0000"   # wine (high)
)

# Heatmap 1: Pings
p_heat_pings <- ggplot(p1_df, aes(MonthF, YearF, fill = Value)) +
  geom_tile(color = "white", linewidth = 0.25, na.rm = FALSE) +
  scale_fill_gradientn(
    colours = heat_cols,
    trans = "log10",
    na.value = "white",
    breaks = c(1, 1e2, 1e4),
    labels = label_scientific()
  ) +
  labs(
    title = "Heatmap: pings coverage per month and year",
    x = "Month", y = "Year",
    fill = "Pings (N)"
  ) +
  heat_theme

# Heatmap 2: Vessels
p_heat_vessels <- ggplot(p2_df, aes(MonthF, YearF, fill = Value)) +
  geom_tile(color = "white", linewidth = 0.25, na.rm = FALSE) +
  scale_fill_gradientn(
    colours = heat_cols,
    trans = "log10",
    na.value = "white",
    breaks = c(1, 3, 10),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    title = "Heatmap: number of vessels per month and year",
    x = "Month", y = "Year",
    fill = "Number of vessels"
  ) +
  heat_theme

# ---- 8) Yearly totals line plot (clear + accessible + colored axes) ----
yearly <- df2 %>%
  group_by(Year) %>%
  summarise(
    Pings = n(),
    Vessels = n_distinct(.data[[vessel_col]]),
    .groups = "drop"
  )

# Ensure all years exist, even if 0 (standardized axis)
yearly <- tibble(Year = years_target) %>%
  left_join(yearly, by = "Year") %>%
  mutate(
    Pings = replace_na(Pings, 0),
    Vessels = replace_na(Vessels, 0)
  ) %>%
  arrange(Year)

max_v <- max(yearly$Vessels, na.rm = TRUE)
max_p <- max(yearly$Pings, na.rm = TRUE)
scale_factor <- ifelse(max_v == 0, 1, max_p / max_v)

# Line colors
col_pings   <- "seagreen4"
col_vessels <- "#C51B7D"  # warm but not orange

last_year <- max(yearly$Year, na.rm = TRUE)
lab_dat <- yearly %>% filter(Year == last_year)

p_year <- ggplot(yearly, aes(x = Year)) +
  # Pings: solid + circles
  geom_line(aes(y = Pings), linewidth = 1.1, linetype = "solid", color = col_pings) +
  geom_point(aes(y = Pings), size = 2.4, shape = 16, color = col_pings) +
  
  # Vessels: dashed + squares (scaled)
  geom_line(aes(y = Vessels * scale_factor), linewidth = 1.0, linetype = "longdash", color = col_vessels) +
  geom_point(aes(y = Vessels * scale_factor), size = 2.3, shape = 15, color = col_vessels) +
  
  # End labels (simple + controlled)
  annotate("text",
           x = lab_dat$Year + 0.35, y = lab_dat$Pings,
           label = "Pings", color = col_pings, size = 5, hjust = 0) +
  annotate("text",
           x = lab_dat$Year + 0.35, y = lab_dat$Vessels * scale_factor,
           label = "Vessels", color = col_vessels, size = 5, hjust = 0) +
  
  scale_x_continuous(
    limits = c(1999, 2023.9),
    breaks = seq(1999, 2023, by = 2)
  ) +
  scale_y_continuous(
    name = "Number of pings",
    sec.axis = sec_axis(~ . / scale_factor, name = "Number of vessels")
  ) +
  labs(title = "Number of Pings and Vessels per Year", x = "Year") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    
    # Axis left colored to match pings
    axis.title.y.left = element_text(color = col_pings),
    axis.text.y.left  = element_text(color = col_pings),
    
    # Axis right colored to match vessels
    axis.title.y.right = element_text(color = col_vessels),
    axis.text.y.right  = element_text(color = col_vessels)
  )

# ---- 9) Save ----
ggsave(file.path(out_dir, "heatmap_pings_month_year.png"),
       p_heat_pings, width = 10, height = 5, dpi = 300)

ggsave(file.path(out_dir, "heatmap_vessels_month_year.png"),
       p_heat_vessels, width = 10, height = 5, dpi = 300)

ggsave(file.path(out_dir, "yearly_pings_vs_vessels.png"),
       p_year, width = 10, height = 4.5, dpi = 300)

message("Pronto! Arquivos salvos em: ", out_dir)

