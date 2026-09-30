
# NND, ISD, NNI#

# SEASONS #


# FALL #

# 📦 Pacotes
library(sf)
library(dplyr)
library(geosphere)
library(ggplot2)
library(lubridate)

# 📁 Diretório
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/dataset_final/proxy_grid_fall")

# 📥 Carregar proxy (FALL)
proxy <- read.csv("proxy_fishing_activity_fall.csv") %>%
  mutate(year = year(as.Date(date))) %>%
  filter(year >= 1999, year <= 2023)   # 👈 garante 1999–2023

# Converter para sf
proxy_sf <- st_as_sf(proxy, coords = c("X_lon", "Y_lat"), crs = 4326)

# 🔁 Loop por ano
resultados <- list()
detalhes_por_ponto <- list()
years <- sort(unique(proxy_sf$year))           # já filtrado
years <- years[years >= 1999 & years <= 2023]  # 👈 redundante, por segurança

for (yr in years) {
  message("▶️ Ano: ", yr)
  
  df_year <- proxy_sf %>% filter(year == yr)
  if (nrow(df_year) < 2) {
    warning(paste("Ano", yr, "tem menos de 2 pontos — pulando."))
    next
  }
  
  # ⚠️ Coordenadas em graus (EPSG:4326)
  coords <- st_coordinates(df_year)
  
  # 📏 Distâncias geodésicas
  dists <- geosphere::distm(coords, fun = distHaversine)
  diag(dists) <- NA
  
  # NND e ISD
  nearest <- apply(dists, 1, function(x) min(x, na.rm = TRUE)) / 1000  # km
  mean_nnd <- mean(nearest)
  
  isd_point <- apply(dists, 1, function(x) sd(x, na.rm = TRUE)) / 1000  # km
  mean_isd <- mean(isd_point)
  
  # 🧭 NNI (usando convex hull do ano)
  hull <- st_union(df_year) %>% st_convex_hull()
  hull_area <- st_area(st_transform(hull, 27700))  # m² (CRS métrico)
  area_m2 <- as.numeric(hull_area)
  expected_dist <- 0.5 / sqrt(nrow(coords) / area_m2)
  expected_dist_km <- expected_dist / 1000
  nni_value <- mean_nnd / expected_dist_km
  
  # 🔢 Guardar resultados
  resultados[[as.character(yr)]] <- data.frame(
    year = yr,
    n_points = nrow(coords),
    mean_nnd_km = round(mean_nnd, 2),
    mean_isd_km = round(mean_isd, 2),
    nni = round(nni_value, 3)
  )
  
  detalhes_por_ponto[[as.character(yr)]] <- data.frame(
    year = yr,
    lon = coords[, 1],
    lat = coords[, 2],
    NND_km = round(nearest, 3),
    ISD_km = round(isd_point, 3)
  )
}

# 💾 Exportar tabelas
nni_df <- bind_rows(resultados)
write.csv(nni_df, "NNI_NND_ISD_by_year_FALL.csv", row.names = FALSE)

nni_pts_df <- bind_rows(detalhes_por_ponto)
write.csv(nni_pts_df, "NNI_NND_ISD_by_point_FALL.csv", row.names = FALSE)

cat("✅ Tabelas salvas:\n- NNI_NND_ISD_by_year_FALL.csv\n- NNI_NND_ISD_by_point_FALL.csv\n")

# 📊 Plots de séries temporais
# NNI
ggplot(nni_df, aes(x = year, y = nni)) +
  geom_line(color = "pink", size = 1.2) +
  geom_point(color = "pink", size = 2) +
  geom_smooth(method = "loess", se = TRUE,
              color = "#377eb8", fill = "#a6cee3", linetype = "solid", size = 0.5, alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey30") +
  labs(title = "Nearest Neighbor Index (NNI) - FALL (1999–2023)",
       x = "Year", y = "NNI",
       caption = "NNI < 1: Clustered | NNI = 1: Random | NNI > 1: Dispersed") +
  theme_minimal(base_size = 14)
ggsave("NNI_trend_FALL.png", width = 8, height = 6, dpi = 300)

# NND
ggplot(nni_df, aes(x = year, y = mean_nnd_km)) +
  geom_point(color = "#1b9e77") +
  geom_line(color = "#1b9e77") +
  geom_smooth(method = "loess", se = TRUE, fill = "#1b9e77", alpha = 0.2) +
  labs(title = "Nearest Neighbor Distance (NND) - FALL (1999–2023)",
       x = "Year", y = "Mean NND (km)") +
  theme_minimal(base_size = 13)
ggsave("NND_trend_FALL.png", width = 8, height = 6, dpi = 300)

# ISD
ggplot(nni_df, aes(x = year, y = mean_isd_km)) +
  geom_point(color = "#d95f02") +
  geom_line(color = "#d95f02") +
  geom_smooth(method = "loess", se = TRUE, fill = "#d95f02", alpha = 0.2) +
  labs(title = "Interpoint Standard Deviation (ISD) - FALL (1999–2023)",
       x = "Year", y = "Mean ISD (km)") +
  theme_minimal(base_size = 13)
ggsave("ISD_trend_FALL.png", width = 8, height = 6, dpi = 300)


#
#
#

# WINTER #

# 📦 Pacotes
library(sf)
library(dplyr)
library(geosphere)
library(ggplot2)
library(lubridate)

# 📁 Diretório
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/dataset_final/proxy_grid_winter")

# 📥 Carregar proxy (WINTER)
proxy <- read.csv("proxy_fishing_activity_winter.csv") %>%
  mutate(year = year(as.Date(date))) %>%
  filter(year >= 1999, year <= 2023)   # 👈 garante 1999–2023

# Converter para sf
proxy_sf <- st_as_sf(proxy, coords = c("X_lon", "Y_lat"), crs = 4326)

# 🔁 Loop por ano
resultados <- list()
detalhes_por_ponto <- list()
years <- sort(unique(proxy_sf$year))
years <- years[years >= 1999 & years <= 2023]  # 👈 redundante, por segurança

for (yr in years) {
  message("▶️ Ano: ", yr)
  
  df_year <- proxy_sf %>% filter(year == yr)
  if (nrow(df_year) < 2) {
    warning(paste("Ano", yr, "tem menos de 2 pontos — pulando."))
    next
  }
  
  coords <- st_coordinates(df_year)
  
  # 📏 Distâncias geodésicas
  dists <- geosphere::distm(coords, fun = distHaversine)
  diag(dists) <- NA
  
  # NND e ISD
  nearest <- apply(dists, 1, function(x) min(x, na.rm = TRUE)) / 1000  # km
  mean_nnd <- mean(nearest)
  
  isd_point <- apply(dists, 1, function(x) sd(x, na.rm = TRUE)) / 1000  # km
  mean_isd <- mean(isd_point)
  
  # 🧭 NNI (usando convex hull do ano)
  hull <- st_union(df_year) %>% st_convex_hull()
  hull_area <- st_area(st_transform(hull, 27700))  # m² (CRS métrico)
  area_m2 <- as.numeric(hull_area)
  expected_dist <- 0.5 / sqrt(nrow(coords) / area_m2)
  expected_dist_km <- expected_dist / 1000
  nni_value <- mean_nnd / expected_dist_km
  
  # 🔢 Guardar resultados
  resultados[[as.character(yr)]] <- data.frame(
    year = yr,
    n_points = nrow(coords),
    mean_nnd_km = round(mean_nnd, 2),
    mean_isd_km = round(mean_isd, 2),
    nni = round(nni_value, 3)
  )
  
  detalhes_por_ponto[[as.character(yr)]] <- data.frame(
    year = yr,
    lon = coords[, 1],
    lat = coords[, 2],
    NND_km = round(nearest, 3),
    ISD_km = round(isd_point, 3)
  )
}

# 💾 Exportar tabelas
nni_df <- bind_rows(resultados)
write.csv(nni_df, "NNI_NND_ISD_by_year_WINTER.csv", row.names = FALSE)

nni_pts_df <- bind_rows(detalhes_por_ponto)
write.csv(nni_pts_df, "NNI_NND_ISD_by_point_WINTER.csv", row.names = FALSE)

cat("✅ Tabelas salvas:\n- NNI_NND_ISD_by_year_WINTER.csv\n- NNI_NND_ISD_by_point_WINTER.csv\n")

# 📊 Plots de séries temporais
# NNI
ggplot(nni_df, aes(x = year, y = nni)) +
  geom_line(color = "pink", size = 1.2) +
  geom_point(color = "pink", size = 2) +
  geom_smooth(method = "loess", se = TRUE,
              color = "#377eb8", fill = "#a6cee3", linetype = "solid", size = 0.5, alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey30") +
  labs(title = "Nearest Neighbor Index (NNI) - WINTER (1999–2023)",
       x = "Year", y = "NNI",
       caption = "NNI < 1: Clustered | NNI = 1: Random | NNI > 1: Dispersed") +
  theme_minimal(base_size = 14)
ggsave("NNI_trend_WINTER.png", width = 8, height = 6, dpi = 300)

# NND
ggplot(nni_df, aes(x = year, y = mean_nnd_km)) +
  geom_point(color = "#1b9e77") +
  geom_line(color = "#1b9e77") +
  geom_smooth(method = "loess", se = TRUE, fill = "#1b9e77", alpha = 0.2) +
  labs(title = "Nearest Neighbor Distance (NND) - WINTER (1999–2023)",
       x = "Year", y = "Mean NND (km)") +
  theme_minimal(base_size = 13)
ggsave("NND_trend_WINTER.png", width = 8, height = 6, dpi = 300)

# ISD
ggplot(nni_df, aes(x = year, y = mean_isd_km)) +
  geom_point(color = "#d95f02") +
  geom_line(color = "#d95f02") +
  geom_smooth(method = "loess", se = TRUE, fill = "#d95f02", alpha = 0.2) +
  labs(title = "Interpoint Standard Deviation (ISD) - WINTER (1999–2023)",
       x = "Year", y = "Mean ISD (km)") +
  theme_minimal(base_size = 13)
ggsave("ISD_trend_WINTER.png", width = 8, height = 6, dpi = 300)


#
#
#

# Versao que junta nnd, isd (LDI), nni e suas figuras juntinhas e salva as coisas na area de trabalho #

# ===============================
# NND, ISD (LDI), NNI — FALL vs WINTER
# Saída: figura única 3x2 + CSVs
# ===============================

library(sf)
library(dplyr)
library(geosphere)
library(ggplot2)
library(lubridate)
library(patchwork)
library(scales)

fall_path   <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/dataset_final/proxy_grid_fall/proxy_fishing_activity_fall.csv"
winter_path <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/dataset_final/proxy_grid_winter/proxy_fishing_activity_winter.csv"

out_dir <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop"

col_fall   <- "#d95f02"  # laranja escuro
col_winter <- "#1b4f72"  # azul escuro

compute_metrics <- function(csv_path, season_label) {
  df <- read.csv(csv_path) %>%
    mutate(year = lubridate::year(as.Date(date))) %>%
    filter(year >= 1999, year <= 2023)
  
  if (!all(c("X_lon","Y_lat") %in% names(df))) {
    stop("Colunas X_lon e Y_lat não encontradas em: ", csv_path)
  }
  
  sf_pts <- st_as_sf(df, coords = c("X_lon","Y_lat"), crs = 4326)
  years <- sort(unique(sf_pts$year))
  
  res_list <- vector("list", length(years))
  names(res_list) <- years
  
  for (yr in years) {
    df_year <- sf_pts %>% filter(year == yr)
    if (nrow(df_year) < 2) {
      res_list[[as.character(yr)]] <- data.frame(
        season = season_label, year = yr,
        n_points = nrow(df_year),
        mean_nnd_km = NA_real_,
        mean_isd_km = NA_real_,
        nni = NA_real_
      )
      next
    }
    
    coords <- st_coordinates(df_year)
    dists  <- geosphere::distm(coords, fun = distHaversine)
    diag(dists) <- NA
    
    nearest  <- apply(dists, 1, function(x) min(x, na.rm = TRUE)) / 1000
    mean_nnd <- mean(nearest)
    
    isd_point <- apply(dists, 1, function(x) sd(x, na.rm = TRUE)) / 1000
    mean_isd  <- mean(isd_point)
    
    hull <- try(suppressWarnings(st_convex_hull(st_union(df_year))), silent = TRUE)
    if (inherits(hull, "try-error")) hull <- NA
    
    nni_value <- NA_real_
    if (!any(is.na(hull))) {
      hull_area_m2 <- suppressWarnings(as.numeric(st_area(st_transform(hull, 27700))))
      n_pts <- nrow(coords)
      if (!is.na(hull_area_m2) && hull_area_m2 > 0 && n_pts > 0) {
        expected_dist_m  <- 0.5 / sqrt(n_pts / hull_area_m2)
        expected_dist_km <- expected_dist_m / 1000
        nni_value <- mean_nnd / expected_dist_km
      }
    }
    
    res_list[[as.character(yr)]] <- data.frame(
      season = season_label,
      year = yr,
      n_points = nrow(coords),
      mean_nnd_km = round(mean_nnd, 3),
      mean_isd_km = round(mean_isd, 3),
      nni = round(nni_value, 3)
    )
  }
  
  bind_rows(res_list)
}

# Rodar
fall_year   <- compute_metrics(fall_path, "Autumn")
winter_year <- compute_metrics(winter_path, "Winter")

# (Opcional) CSVs no Desktop — mantive nomes originais
write.csv(fall_year,   file.path(out_dir, "NNI_NND_ISD_by_year_FALL.csv"),   row.names = FALSE)
write.csv(winter_year, file.path(out_dir, "NNI_NND_ISD_by_year_WINTER.csv"), row.names = FALSE)

all_years <- bind_rows(fall_year, winter_year)

x_breaks <- seq(1999, 2023, by = 2)
x_limits <- c(1999, 2023)

yrange <- function(v) {
  v <- v[is.finite(v)]
  if (length(v) == 0) return(c(0,1))
  rng <- range(v, na.rm = TRUE)
  pad <- 0.05 * diff(rng)
  if (pad == 0) pad <- ifelse(rng[2]==0, 0.1, 0.05*rng[2])
  c(rng[1]-pad, rng[2]+pad)
}

ylim_nnd <- yrange(all_years$mean_nnd_km)
ylim_isd <- yrange(all_years$mean_isd_km)
ylim_nni <- yrange(all_years$nni)

theme_base <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

p_metric <- function(df, metric, ylab, col) {
  ggplot(df, aes(x = year, y = .data[[metric]])) +
    geom_line(color = col, linewidth = 1) +
    geom_point(color = col, size = 1.8) +
    scale_x_continuous(breaks = x_breaks, limits = x_limits, expand = expansion(mult = 0.01)) +
    labs(x = "Year", y = ylab) +
    theme_base
}

df_fall   <- fall_year
df_winter <- winter_year

p_nnd_fall   <- p_metric(df_fall,   "mean_nnd_km", "Mean NND (km)", col_fall)   +
  coord_cartesian(ylim = ylim_nnd) + ggtitle("Autumn")
p_nnd_winter <- p_metric(df_winter, "mean_nnd_km", "Mean NND (km)", col_winter) +
  coord_cartesian(ylim = ylim_nnd) + ggtitle("Winter")

p_isd_fall   <- p_metric(df_fall,   "mean_isd_km", "Mean LDI (km)", col_fall)   +
  coord_cartesian(ylim = ylim_isd)
p_isd_winter <- p_metric(df_winter, "mean_isd_km", "Mean LDI (km)", col_winter) +
  coord_cartesian(ylim = ylim_isd)

p_nni_fall <- p_metric(df_fall, "nni", "NNI", col_fall) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  coord_cartesian(ylim = ylim_nni)
p_nni_winter <- p_metric(df_winter, "nni", "NNI", col_winter) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  coord_cartesian(ylim = ylim_nni)

layout <- "
AB
CD
EF
"
final_plot <- p_nnd_fall + p_nnd_winter +
  p_isd_fall + p_isd_winter +
  p_nni_fall + p_nni_winter +
  plot_layout(design = layout, guides = "collect") &
  theme(legend.position = "none")

# — sem título/subtítulo gerais —
outfile_png <- file.path(out_dir, "NND_ISD_NNI_Autumn_vs_Winter.png")
outfile_pdf <- file.path(out_dir, "NND_ISD_NNI_Autumn_vs_Winter.pdf")
ggsave(outfile_png, final_plot, width = 12, height = 12, dpi = 300)
ggsave(outfile_pdf, final_plot, width = 12, height = 12)


#
#
#

# Mesma coisa do script anterior mas com a adicao de um smothie # 

# PDR = NNI correto!

# ===============================
# NND, LDI e PDR — Autumn vs Winter (com smooth)
# PDR = NND / LDI (lógica Mackinson)
# Saída: figura única 3x2 + CSVs
# ===============================

library(sf)
library(dplyr)
library(geosphere)
library(ggplot2)
library(lubridate)
library(patchwork)
library(scales)

fall_path   <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/dataset_final/proxy_grid_fall/proxy_fishing_activity_fall.csv"
winter_path <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/dataset_final/proxy_grid_winter/proxy_fishing_activity_winter.csv"

out_dir <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop"

col_fall   <- "#d95f02"  # laranja escuro
col_winter <- "#1b4f72"  # azul escuro

# -------------------------
# Função: NND, LDI, PDR
# -------------------------
compute_metrics <- function(csv_path, season_label) {
  df <- read.csv(csv_path) %>%
    mutate(year = lubridate::year(as.Date(date))) %>%
    filter(year >= 1999, year <= 2023)
  
  if (!all(c("X_lon","Y_lat") %in% names(df))) {
    stop("Colunas X_lon e Y_lat não encontradas em: ", csv_path)
  }
  
  sf_pts <- st_as_sf(df, coords = c("X_lon","Y_lat"), crs = 4326)
  years  <- sort(unique(sf_pts$year))
  
  res_list <- vector("list", length(years))
  names(res_list) <- years
  
  for (yr in years) {
    df_year <- sf_pts %>% filter(year == yr)
    
    if (nrow(df_year) < 2) {
      # sem pares suficientes
      res_list[[as.character(yr)]] <- data.frame(
        season       = season_label,
        year         = yr,
        n_points     = nrow(df_year),
        mean_nnd_km  = NA_real_,
        mean_isd_km  = NA_real_,
        pdr          = NA_real_
      )
      next
    }
    
    coords <- st_coordinates(df_year)
    dists  <- geosphere::distm(coords, fun = distHaversine)  # metros
    diag(dists) <- NA
    
    # NND (km)
    nearest  <- apply(dists, 1, function(x) min(x, na.rm = TRUE)) / 1000
    mean_nnd <- mean(nearest)
    
    # LDI / ISD (km): sd das distâncias por ponto, depois média
    isd_point <- apply(dists, 1, function(x) sd(x, na.rm = TRUE)) / 1000
    mean_isd  <- mean(isd_point)
    
    # PDR = NND / LDI (se LDI > 0)
    pdr_value <- NA_real_
    if (!is.na(mean_isd) && mean_isd > 0) {
      pdr_value <- mean_nnd / mean_isd
    }
    
    res_list[[as.character(yr)]] <- data.frame(
      season       = season_label,
      year         = yr,
      n_points     = nrow(coords),
      mean_nnd_km  = round(mean_nnd, 3),
      mean_isd_km  = round(mean_isd, 3),
      pdr          = round(pdr_value, 3)
    )
  }
  
  bind_rows(res_list)
}

# Rodar para Autumn e Winter
fall_year   <- compute_metrics(fall_path, "Autumn")
winter_year <- compute_metrics(winter_path, "Winter")

# CSVs no Desktop (renomeados para refletir PDR)
write.csv(fall_year,   file.path(out_dir, "PDR_NND_LDI_by_year_AUTUMN.csv"), row.names = FALSE)
write.csv(winter_year, file.path(out_dir, "PDR_NND_LDI_by_year_WINTER.csv"), row.names = FALSE)

# Combinar para escalas comuns
all_years <- bind_rows(fall_year, winter_year)

x_breaks <- seq(1999, 2023, by = 2)
x_limits <- c(1999, 2023)

yrange <- function(v) {
  v <- v[is.finite(v)]
  if (length(v) == 0) return(c(0,1))
  rng <- range(v, na.rm = TRUE)
  pad <- 0.05 * diff(rng)
  if (pad == 0) pad <- ifelse(rng[2] == 0, 0.1, 0.05 * abs(rng[2]))
  c(rng[1] - pad, rng[2] + pad)
}

ylim_nnd <- yrange(all_years$mean_nnd_km)
ylim_isd <- yrange(all_years$mean_isd_km)
ylim_pdr <- yrange(all_years$pdr)

theme_base <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

# função com smooth
p_metric <- function(df, metric, ylab, col) {
  ggplot(df, aes(x = year, y = .data[[metric]])) +
    geom_line(color = col, linewidth = 0.8) +
    geom_point(color = col, size = 1.8) +
    geom_smooth(
      method = "loess",
      se     = TRUE,
      color  = col,
      fill   = col,
      alpha  = 0.2,
      linewidth = 0.5
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits,
      expand = expansion(mult = 0.01)
    ) +
    labs(x = "Year", y = ylab) +
    theme_base
}

df_fall   <- fall_year
df_winter <- winter_year

# NND
p_nnd_fall <- p_metric(df_fall, "mean_nnd_km", "Mean NND (km)", col_fall) +
  coord_cartesian(ylim = ylim_nnd) +
  ggtitle("Autumn")

p_nnd_winter <- p_metric(df_winter, "mean_nnd_km", "Mean NND (km)", col_winter) +
  coord_cartesian(ylim = ylim_nnd) +
  ggtitle("Winter")

# LDI
p_ldi_fall <- p_metric(df_fall, "mean_isd_km", "Mean LDI (km)", col_fall) +
  coord_cartesian(ylim = ylim_isd)

p_ldi_winter <- p_metric(df_winter, "mean_isd_km", "Mean LDI (km)", col_winter) +
  coord_cartesian(ylim = ylim_isd)

# PDR
p_pdr_fall <- p_metric(df_fall, "pdr", "PDR (NND / LDI)", col_fall) +
  coord_cartesian(ylim = ylim_pdr)

p_pdr_winter <- p_metric(df_winter, "pdr", "PDR (NND / LDI)", col_winter) +
  coord_cartesian(ylim = ylim_pdr)

# layout 3x2
layout <- "
AB
CD
EF
"

final_plot <- p_nnd_fall + p_nnd_winter +
  p_ldi_fall + p_ldi_winter +
  p_pdr_fall + p_pdr_winter +
  plot_layout(design = layout, guides = "collect") &
  theme(legend.position = "none")

# salvar no Desktop
outfile_png <- file.path(out_dir, "NND_LDI_PDR_Autumn_vs_Winter_smooth.png")
outfile_pdf <- file.path(out_dir, "NND_LDI_PDR_Autumn_vs_Winter_smooth.pdf")

ggsave(outfile_png, final_plot, width = 12, height = 12, dpi = 300)
ggsave(outfile_pdf, final_plot, width = 12, height = 12)


#
#
#

# SEM PDR #

# ===============================
# NND e LDI — Autumn vs Winter (com smooth)
# Saída: figura única 2x2 + CSVs
# ===============================

library(sf)
library(dplyr)
library(geosphere)
library(ggplot2)
library(lubridate)
library(patchwork)
library(scales)

fall_path   <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_fall/proxy_fishing_activity_fall_timeweighted.csv"
winter_path <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_winter/proxy_fishing_activity_winter_timeweighted.csv"

out_dir <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop"

col_fall   <- "#d95f02"  # laranja escuro
col_winter <- "#1b4f72"  # azul escuro

# -------------------------
# Função: NND e LDI (ISD)
# -------------------------
compute_metrics <- function(csv_path, season_label) {
  df <- read.csv(csv_path, stringsAsFactors = FALSE) %>%
    mutate(year = lubridate::year(as.Date(date))) %>%
    filter(year >= 1999, year <= 2023)
  
  if (!all(c("X_lon","Y_lat") %in% names(df))) {
    stop("Colunas X_lon e Y_lat não encontradas em: ", csv_path)
  }
  
  sf_pts <- st_as_sf(df, coords = c("X_lon","Y_lat"), crs = 4326)
  years  <- sort(unique(sf_pts$year))
  
  res_list <- vector("list", length(years))
  names(res_list) <- years
  
  for (yr in years) {
    df_year <- sf_pts %>% filter(year == yr)
    
    if (nrow(df_year) < 2) {
      res_list[[as.character(yr)]] <- data.frame(
        season      = season_label,
        year        = yr,
        n_points    = nrow(df_year),
        mean_nnd_km = NA_real_,
        mean_ldi_km = NA_real_
      )
      next
    }
    
    coords <- st_coordinates(df_year)
    dists  <- geosphere::distm(coords, fun = distHaversine)  # metros
    diag(dists) <- NA
    
    # NND (km)
    nearest  <- apply(dists, 1, function(x) min(x, na.rm = TRUE)) / 1000
    mean_nnd <- mean(nearest, na.rm = TRUE)
    
    # LDI/ISD (km): sd das distâncias por ponto, depois média
    ldi_point <- apply(dists, 1, function(x) sd(x, na.rm = TRUE)) / 1000
    mean_ldi  <- mean(ldi_point, na.rm = TRUE)
    
    res_list[[as.character(yr)]] <- data.frame(
      season      = season_label,
      year        = yr,
      n_points    = nrow(coords),
      mean_nnd_km = round(mean_nnd, 3),
      mean_ldi_km = round(mean_ldi, 3)
    )
  }
  
  bind_rows(res_list)
}

# Rodar para Autumn e Winter
fall_year   <- compute_metrics(fall_path, "Autumn")
winter_year <- compute_metrics(winter_path, "Winter")

# CSVs no Desktop
write.csv(fall_year,   file.path(out_dir, "NND_LDI_by_year_AUTUMN.csv"), row.names = FALSE)
write.csv(winter_year, file.path(out_dir, "NND_LDI_by_year_WINTER.csv"), row.names = FALSE)

# Combinar para escalas comuns
all_years <- bind_rows(fall_year, winter_year)

x_breaks <- seq(1999, 2023, by = 2)
x_limits <- c(1999, 2023)

yrange <- function(v) {
  v <- v[is.finite(v)]
  if (length(v) == 0) return(c(0, 1))
  rng <- range(v, na.rm = TRUE)
  pad <- 0.05 * diff(rng)
  if (pad == 0) pad <- ifelse(rng[2] == 0, 0.1, 0.05 * abs(rng[2]))
  c(rng[1] - pad, rng[2] + pad)
}

ylim_nnd <- yrange(all_years$mean_nnd_km)
ylim_ldi <- yrange(all_years$mean_ldi_km)

theme_base <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

# função com smooth
p_metric <- function(df, metric, ylab, col) {
  ggplot(df, aes(x = year, y = .data[[metric]])) +
    geom_line(color = col, linewidth = 0.8) +
    geom_point(color = col, size = 1.8) +
    geom_smooth(
      method = "loess",
      se     = TRUE,
      color  = col,
      fill   = col,
      alpha  = 0.2,
      linewidth = 0.5
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits,
      expand = expansion(mult = 0.01)
    ) +
    labs(x = "Year", y = ylab) +
    theme_base
}

df_fall   <- fall_year
df_winter <- winter_year

# NND
p_nnd_fall <- p_metric(df_fall, "mean_nnd_km", "Mean NND (km)", col_fall) +
  coord_cartesian(ylim = ylim_nnd) +
  ggtitle("Autumn")

p_nnd_winter <- p_metric(df_winter, "mean_nnd_km", "Mean NND (km)", col_winter) +
  coord_cartesian(ylim = ylim_nnd) +
  ggtitle("Winter")

# LDI
p_ldi_fall <- p_metric(df_fall, "mean_ldi_km", "Mean LDI (km)", col_fall) +
  coord_cartesian(ylim = ylim_ldi)

p_ldi_winter <- p_metric(df_winter, "mean_ldi_km", "Mean LDI (km)", col_winter) +
  coord_cartesian(ylim = ylim_ldi)

# layout 2x2
layout <- "
AB
CD
"

final_plot <- p_nnd_fall + p_nnd_winter +
  p_ldi_fall + p_ldi_winter +
  plot_layout(design = layout, guides = "collect") &
  theme(legend.position = "none")

# salvar no Desktop
outfile_png <- file.path(out_dir, "NND_LDI_Autumn_vs_Winter_smooth.png")
outfile_pdf <- file.path(out_dir, "NND_LDI_Autumn_vs_Winter_smooth.pdf")

ggsave(outfile_png, final_plot, width = 12, height = 9, dpi = 300)
ggsave(outfile_pdf, final_plot, width = 12, height = 9)











