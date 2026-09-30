#############################
# Centroides PROXY ACTIVITY # 
# FALL & WINTER             #
#############################

# FALL #

############################################# EM CONSTRUCAO ###############################################
# Com variancia em linhas nos centroides

# 📦 Pacotes
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(tidyr)
library(viridis)
library(lubridate)

# 📁 Diretório
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/dataset_final/proxy_grid_fall")

# 📥 Carregar proxy (fall)
proxy <- read.csv("proxy_fishing_activity_fall.csv") %>%
  mutate(year = year(as.Date(date)))

# 🌍 Mapas base
world <- ne_countries(scale = "medium", returnclass = "sf")
uk_neighbors <- world %>%
  filter(name_en %in% c("United Kingdom", "Ireland", "France", 
                        "Belgium", "Netherlands", "Germany", "Norway")) %>%
  st_transform(crs = 3857)
land_union <- st_union(uk_neighbors)

ICES_Areas <- st_read("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/chapter2/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp") %>%
  st_make_valid() %>%
  st_transform(crs = 3857)

# 🔍 1) Converter proxy para sf (em metros)
proxy_sf <- st_as_sf(proxy, coords = c("X", "Y"), crs = 4326) %>%
  st_transform(3857)

# Remover pontos em terra
proxy_sf <- proxy_sf[!st_intersects(proxy_sf, land_union, sparse = FALSE), ]

# Extrair coordenadas em METROS
proxy_sf <- proxy_sf %>%
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2])

# 🔗 Calcular centroides anuais
centroids_fall <- proxy_sf %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(
    mean_x = mean(X, na.rm = TRUE),
    mean_y = mean(Y, na.rm = TRUE),
    sd_x   = sd(X, na.rm = TRUE) / 1000,  # sd em km
    sd_y   = sd(Y, na.rm = TRUE) / 1000,  # sd em km
    .groups = "drop"
  ) %>%
  filter(!is.na(mean_x), !is.na(mean_y))

# Extrair centroides em km
centroids_fall <- centroids_fall %>%
  mutate(X = mean_x / 1000,   # metros → km
         Y = mean_y / 1000)   # metros → km

# 🗺️ Plot (shapefiles continuam em metros, mas eixos centrados nos centroides em km)
p <- ggplot() +
  geom_sf(data = uk_neighbors, fill = "grey95", color = "grey40", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, fill = NA, color = "grey30", linetype = "dashed", alpha = 0.8) +
  geom_errorbarh(data = centroids_fall,
                 aes(y = Y, xmin = X - sd_x, xmax = X + sd_x, colour = year),
                 height = 0, linewidth = 0.8) +
  geom_errorbar(data = centroids_fall,
                aes(x = X, ymin = Y - sd_y, ymax = Y + sd_y, colour = year),
                width = 0, linewidth = 0.8) +
  geom_point(data = centroids_fall, aes(x = X, y = Y, colour = year), size = 3) +
  scale_colour_viridis_c(option = "plasma", direction = -1, name = "Year") +
  labs(title = "Centroides of fishing activities - FALL (1999–2023)",
       x = "Longitude (km)", y = "Latitude (km)") +
  theme_minimal()

# 💾 Salvar
ggsave("centroids_fall_map_km.png", plot = p, width = 12, height = 8, dpi = 300, bg = "white")

#
#
#

##########################################
#Sem variancia nos centroides e com anos #
##########################################

# FALL #

# 📦 Pacotes necessários
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(viridis)
library(lubridate)

# 📁 Diretório
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/dataset_final/proxy_grid_fall")

# 📥 Carregar proxy (exemplo: FALL)
proxy <- read.csv("proxy_fishing_activity_fall.csv") %>%
  mutate(year = year(as.Date(date))) %>%
  filter(year >= 1999 & year <= 2023)

# 🌍 Mapas base (agora em 27700)
world <- ne_countries(scale = "medium", returnclass = "sf")
uk_neighbors <- world %>%
  filter(name_en %in% c("United Kingdom", "Ireland", "France",
                        "Belgium", "Netherlands", "Germany", "Norway")) %>%
  st_transform(27700)

land_union <- st_union(uk_neighbors)

ICES_Areas <- st_read("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/chapter2/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp") %>%
  st_make_valid() %>%
  st_transform(27700)

# Converter proxy em sf (lon/lat -> 27700)
proxy_sf <- st_as_sf(proxy, coords = c("X_lon", "Y_lat"), crs = 4326) %>%
  st_transform(27700)

# 🔗 Excluir pontos em terra
proxy_sea <- proxy_sf[!st_intersects(proxy_sf, land_union, sparse = FALSE), ]

# 🔗 Calcular centroides anuais simples
centroids_fall <- proxy_sea %>%
  group_by(year) %>%
  summarise(geometry = st_centroid(st_union(geometry)), .groups = "drop")

# Ajustar centroides que ainda caíram em terra
is_land <- st_intersects(centroids_fall, land_union, sparse = FALSE)
for (i in which(is_land)) {
  nearest_sea <- st_nearest_points(centroids_fall[i, ], st_boundary(land_union)) %>%
    st_cast("POINT") %>% .[2]
  st_geometry(centroids_fall[i, ]) <- nearest_sea
}

# Extrair coordenadas (em 27700)
coords <- st_coordinates(centroids_fall)
centroids_fall <- centroids_fall %>%
  mutate(X = coords[,1], Y = coords[,2])

# Criar bounding box expandida (em metros)
bbox_centroids <- st_bbox(centroids_fall)
bbox_exp <- bbox_centroids
bbox_exp[c("xmin","ymin")] <- bbox_centroids[c("xmin","ymin")] - 100000
bbox_exp[c("xmax","ymax")] <- bbox_centroids[c("xmax","ymax")] + 100000

# 🗺️ Plot
p <- ggplot() +
  geom_sf(data = uk_neighbors, fill = "grey95", color = "grey40", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, fill = NA, color = "grey30", linetype = "dashed", alpha = 0.8) +
  geom_sf(data = centroids_fall, aes(color = year), size = 3) +
  geom_text(data = centroids_fall, aes(X, Y, label = year, color = year),
            size = 3, vjust = -1.2) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Year",
                        limits = c(1999, 2023),
                        breaks = seq(1999, 2023, by = 8)) +
  coord_sf(xlim = c(bbox_exp["xmin"], bbox_exp["xmax"]),
           ylim = c(bbox_exp["ymin"], bbox_exp["ymax"]),
           expand = FALSE) +
  annotation_scale(location = "bl", bar_cols = c("grey60", "white")) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering()) +
  labs(title = "Centroids of fishing activities - FALL (1999–2023)",
       x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 13) +
  theme(panel.background = element_rect(fill = "lightcyan2", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "bottom")

ggsave("centroids_fall_corrected.png", plot = p, width = 12, height = 8, dpi = 300, bg = "white")


#
#
#

# WINTER #

# 📦 Pacotes necessários
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(viridis)
library(lubridate)

# 📁 Diretório
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/dataset_final/proxy_grid_winter")

# 📥 Carregar proxy (exemplo: WINTER)
proxy <- read.csv("proxy_fishing_activity_winter.csv") %>%
  mutate(year = year(as.Date(date))) %>%
  filter(year >= 1999 & year <= 2023)

# 🌍 Mapas base (agora em 27700)
world <- ne_countries(scale = "medium", returnclass = "sf")
uk_neighbors <- world %>%
  filter(name_en %in% c("United Kingdom", "Ireland", "France",
                        "Belgium", "Netherlands", "Germany", "Norway")) %>%
  st_transform(27700)

land_union <- st_union(uk_neighbors)

ICES_Areas <- st_read("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/chapter2/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp") %>%
  st_make_valid() %>%
  st_transform(27700)

# Converter proxy em sf (lon/lat -> 27700)
proxy_sf <- st_as_sf(proxy, coords = c("X_lon", "Y_lat"), crs = 4326) %>%
  st_transform(27700)

# 🔗 Excluir pontos em terra
proxy_sea <- proxy_sf[!st_intersects(proxy_sf, land_union, sparse = FALSE), ]

# 🔗 Calcular centroides anuais simples
centroids_winter <- proxy_sea %>%
  group_by(year) %>%
  summarise(geometry = st_centroid(st_union(geometry)), .groups = "drop")

# Ajustar centroides que ainda caíram em terra
is_land <- st_intersects(centroids_winter, land_union, sparse = FALSE)
for (i in which(is_land)) {
  nearest_sea <- st_nearest_points(centroids_winter[i, ], st_boundary(land_union)) %>%
    st_cast("POINT") %>% .[2]
  st_geometry(centroids_winter[i, ]) <- nearest_sea
}

# Extrair coordenadas
coords <- st_coordinates(centroids_winter)
centroids_winter <- centroids_winter %>%
  mutate(X = coords[,1], Y = coords[,2])

# Criar bounding box expandida
bbox_centroids <- st_bbox(centroids_winter)
bbox_exp <- bbox_centroids
bbox_exp[c("xmin","ymin")] <- bbox_centroids[c("xmin","ymin")] - 100000
bbox_exp[c("xmax","ymax")] <- bbox_centroids[c("xmax","ymax")] + 100000

# 🗺️ Plot
p <- ggplot() +
  geom_sf(data = uk_neighbors, fill = "grey95", color = "grey40", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, fill = NA, color = "grey30", linetype = "dashed", alpha = 0.8) +
  geom_sf(data = centroids_winter, aes(color = year), size = 3) +
  geom_text(data = centroids_winter, aes(X, Y, label = year, color = year),
            size = 3, vjust = -1.2) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Year",
                        limits = c(1999, 2023),
                        breaks = seq(1999, 2023, by = 8)) +
  coord_sf(xlim = c(bbox_exp["xmin"], bbox_exp["xmax"]),
           ylim = c(bbox_exp["ymin"], bbox_exp["ymax"]),
           expand = FALSE) +
  annotation_scale(location = "bl", bar_cols = c("grey60", "white")) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering()) +
  labs(title = "Centroids of fishing activities - WINTER (1999–2023)",
       x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 13) +
  theme(panel.background = element_rect(fill = "lightcyan2", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "bottom")

ggsave("centroids_winter_corrected.png", plot = p, width = 12, height = 8, dpi = 300, bg = "white")


#
#
#

# Figura centroides completa salvando na area de trabalho #

# 📦 Packages
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(viridis)
library(lubridate)
library(patchwork)

# ---------- Diretório principal ----------
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop")
out_dir <- getwd()  # salva todos os outputs na Área de Trabalho

# ---------- Paths ----------
path_fall   <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/dataset_final/proxy_grid_fall/proxy_fishing_activity_fall.csv"
path_winter <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/dataset_final/proxy_grid_winter/proxy_fishing_activity_winter.csv"
path_ices   <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/chapter2/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp"

# ---------- Base maps (to 27700) ----------
world <- ne_countries(scale = "medium", returnclass = "sf")
uk_neighbors <- world %>%
  filter(name_en %in% c("United Kingdom","Ireland","France","Belgium","Netherlands","Germany","Norway")) %>%
  st_transform(27700)
land_union <- st_union(uk_neighbors)

ICES_Areas <- st_read(path_ices) %>%
  st_make_valid() %>%
  st_transform(27700)

# ---------- Helper: read proxy -> sf -> sea-only -> yearly centroid ----------
centroid_by_year <- function(csv_path) {
  df <- read.csv(csv_path) %>%
    mutate(year = year(as.Date(date))) %>%
    filter(year >= 1999 & year <= 2023)
  
  sf_pts <- st_as_sf(df, coords = c("X_lon","Y_lat"), crs = 4326) %>% st_transform(27700)
  sea_pts <- sf_pts[!st_intersects(sf_pts, land_union, sparse = FALSE), ]
  
  cents <- sea_pts %>%
    group_by(year) %>%
    summarise(geometry = st_centroid(st_union(geometry)), .groups = "drop")
  
  # push-to-sea if on land
  on_land <- st_intersects(cents, land_union, sparse = FALSE)
  if (any(on_land)) {
    for (i in which(on_land)) {
      nearest_sea <- st_nearest_points(cents[i, ], st_boundary(land_union)) %>% st_cast("POINT") %>% .[2]
      st_geometry(cents[i, ]) <- nearest_sea
    }
  }
  
  coords <- st_coordinates(cents)
  cents  <- cents %>% mutate(X = coords[,1], Y = coords[,2])
  return(cents)
}

# ---------- Compute centroids ----------
centroids_fall   <- centroid_by_year(path_fall)
centroids_winter <- centroid_by_year(path_winter)

# ---------- Bounding boxes ----------
bbox_fall <- {
  b <- st_bbox(centroids_fall)
  b[c("xmin","ymin")] <- b[c("xmin","ymin")] - 100000
  b[c("xmax","ymax")] <- b[c("xmax","ymax")] + 100000
  b
}
bbox_winter <- {
  b <- st_bbox(centroids_winter)
  b[c("xmin","ymin")] <- b[c("xmin","ymin")] - 100000
  b[c("xmax","ymax")] <- b[c("xmax","ymax")] + 100000
  b
}
both <- rbind(
  centroids_fall %>% mutate(Season = "Autumn"),
  centroids_winter %>% mutate(Season = "Winter")
)
bbox_both <- {
  b <- st_bbox(both)
  b[c("xmin","ymin")] <- b[c("xmin","ymin")] - 100000
  b[c("xmax","ymax")] <- b[c("xmax","ymax")] + 100000
  b
}

# ---------- Themes & scales ----------
base_theme <- theme_minimal(base_size = 13) +
  theme(panel.background = element_rect(fill = "lightcyan2", color = NA),
        plot.background  = element_rect(fill = "white",      color = NA),
        legend.position  = "bottom")

year_scale <- scale_color_viridis_c(option = "plasma", direction = -1, name = "Year",
                                    limits = c(1999, 2023),
                                    breaks = seq(1999, 2023, by = 8))

# ---------- Plot: Autumn (título curto, sem legenda) ----------
p_autumn <- ggplot() +
  geom_sf(data = uk_neighbors, fill = "grey95", color = "grey40", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, fill = NA, color = "grey30", linetype = "dashed", alpha = 0.8) +
  geom_sf(data = centroids_fall, aes(color = year), size = 3) +
  geom_text(data = centroids_fall, aes(X, Y, label = year, color = year), size = 3, vjust = -1.2) +
  year_scale +
  coord_sf(xlim = c(bbox_fall["xmin"], bbox_fall["xmax"]),
           ylim = c(bbox_fall["ymin"], bbox_fall["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", bar_cols = c("grey60","white")) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering()) +
  labs(title = "Autumn", x = "Longitude", y = "Latitude") +
  base_theme +
  guides(color = "none") +
  theme(legend.position = "none")

# ---------- Plot: Winter (título curto, sem legenda) ----------
p_winter <- ggplot() +
  geom_sf(data = uk_neighbors, fill = "grey95", color = "grey40", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, fill = NA, color = "grey30", linetype = "dashed", alpha = 0.8) +
  geom_sf(data = centroids_winter, aes(color = year), size = 3) +
  geom_text(data = centroids_winter, aes(X, Y, label = year, color = year), size = 3, vjust = -1.2) +
  year_scale +
  coord_sf(xlim = c(bbox_winter["xmin"], bbox_winter["xmax"]),
           ylim = c(bbox_winter["ymin"], bbox_winter["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", bar_cols = c("grey60","white")) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering()) +
  labs(title = "Winter", x = "Longitude", y = "Latitude") +
  base_theme +
  guides(color = "none") +
  theme(legend.position = "none")

# ---------- Plot: Combined (Autumn + Winter) ----------
# shapes: Autumn = triangle (24), Winter = square (22), agora preenchidos por 'year'
p_both <- ggplot() +
  geom_sf(data = uk_neighbors, fill = "grey95", color = "grey40", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, fill = NA, color = "grey30", linetype = "dashed", alpha = 0.8) +
  geom_sf(data = both, aes(shape = Season, fill = year),
          color = "grey25", size = 3, stroke = 0.25) +   # borda discreta
  geom_text(data = both %>% st_drop_geometry(),
            aes(X, Y, label = year, color = year),
            size = 2.8, vjust = -1.1) +
  scale_shape_manual(values = c("Autumn" = 24, "Winter" = 22), name = "Season") +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Year",
                       limits = c(1999, 2023),
                       breaks = seq(1999, 2023, by = 8)) +
  scale_color_viridis_c(option = "plasma", direction = -1, guide = "none",
                        limits = c(1999, 2023)) +  # texto colorido pelo ano, sem duplicar legenda
  coord_sf(xlim = c(bbox_both["xmin"], bbox_both["xmax"]),
           ylim = c(bbox_both["ymin"], bbox_both["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", bar_cols = c("grey60","white")) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering()) +
  labs(title = "Centroids of fishing activities — Autumn & Winter (1999–2023)",
       x = "Longitude", y = "Latitude") +
  base_theme +
  guides(fill = guide_colorbar(order = 1), shape = guide_legend(order = 0))

# ---------- Final figure (mapa combinado à esquerda, Autumn em cima e Winter embaixo) ----------
final_fig <- (p_both | (p_autumn / p_winter)) +
  plot_layout(widths = c(1.35, 1.15))  # direita maior que antes, ainda menor que a esquerda

ggsave("centroids_three_panel_figure.png",
       plot = final_fig, width = 18, height = 10, dpi = 300, bg = "white")



#
#
#
#

# Figura centroides completa salvando na area de trabalho + novos shapefiles #

# 📦 Packages
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(viridis)
library(lubridate)
library(patchwork)

# ---------- Diretório principal ----------
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop")
out_dir <- getwd()

# ---------- Paths ----------
path_fall   <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_fall/proxy_fishing_activity_fall_timeweighted.csv"
path_winter <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_winter/proxy_fishing_activity_winter_timeweighted.csv"
path_ices   <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/chapter2/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp"

# ---------- Base maps (substituindo rnaturalearth) ----------
shp_dir <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/shapefiles_shetland_europe_scotland"
path_scotland <- file.path(shp_dir, "OS_Scotland_WGS84.shp")
path_country  <- file.path(shp_dir, "country.shp")

scotland_os <- st_read(path_scotland, quiet = TRUE) %>%
  st_make_valid()
europe_countries <- st_read(path_country, quiet = TRUE) %>%
  st_make_valid()

# Se vier sem CRS, assume WGS84
if (is.na(st_crs(scotland_os))) st_crs(scotland_os) <- 4326
if (is.na(st_crs(europe_countries))) st_crs(europe_countries) <- 4326

# Transformar para EPSG:27700 (mesmo do ICES)
scotland_os_27700 <- st_transform(scotland_os, 27700)
europe_countries_27700 <- st_transform(europe_countries, 27700)

# --- detectar coluna de nome (case-insensitive) e usar o nome REAL ---
candidate_cols <- c("name","name_en","country","admin","cntry_name","countryname")

orig_names  <- names(europe_countries_27700)
lower_names <- tolower(orig_names)

idx <- match(candidate_cols, lower_names)     # retorna índice onde bate
idx <- idx[!is.na(idx)][1]                    # pega o primeiro válido

if (!is.na(idx)) {
  name_col_real <- orig_names[idx]            # NOME REAL (ex: "NAME")
  
  europe_neighbors_27700 <- europe_countries_27700 %>%
    filter(.data[[name_col_real]] %in% c("United Kingdom","Ireland","France",
                                         "Belgium","Netherlands","Germany","Norway"))
  
} else {
  europe_neighbors_27700 <- europe_countries_27700
  message("Não achei uma coluna óbvia de nome em country.shp; usando todos os países.")
}

# União (terra) para máscara (como antes)
land_union <- st_union(europe_neighbors_27700)

# ICES em 27700
ICES_Areas <- st_read(path_ices, quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(27700)

# ---------- Helper: read proxy -> sf -> sea-only -> yearly centroid ----------
centroid_by_year <- function(csv_path) {
  df <- read.csv(csv_path, stringsAsFactors = FALSE) %>%
    mutate(year = year(as.Date(date))) %>%
    filter(year >= 1999 & year <= 2023)
  
  sf_pts <- st_as_sf(df, coords = c("X_lon","Y_lat"), crs = 4326) %>% st_transform(27700)
  sea_pts <- sf_pts[!st_intersects(sf_pts, land_union, sparse = FALSE), ]
  
  cents <- sea_pts %>%
    group_by(year) %>%
    summarise(geometry = st_centroid(st_union(geometry)), .groups = "drop")
  
  # push-to-sea if on land
  on_land <- st_intersects(cents, land_union, sparse = FALSE)
  if (any(on_land)) {
    for (i in which(on_land)) {
      nearest_sea <- st_nearest_points(cents[i, ], st_boundary(land_union)) %>%
        st_cast("POINT") %>% .[2]
      st_geometry(cents[i, ]) <- nearest_sea
    }
  }
  
  coords <- st_coordinates(cents)
  cents  <- cents %>% mutate(X = coords[,1], Y = coords[,2])
  return(cents)
}

# ---------- Compute centroids ----------
centroids_fall   <- centroid_by_year(path_fall)
centroids_winter <- centroid_by_year(path_winter)

# ---------- Bounding boxes ----------
bbox_fall <- {
  b <- st_bbox(centroids_fall)
  b[c("xmin","ymin")] <- b[c("xmin","ymin")] - 100000
  b[c("xmax","ymax")] <- b[c("xmax","ymax")] + 100000
  b
}
bbox_winter <- {
  b <- st_bbox(centroids_winter)
  b[c("xmin","ymin")] <- b[c("xmin","ymin")] - 100000
  b[c("xmax","ymax")] <- b[c("xmax","ymax")] + 100000
  b
}

both <- rbind(
  centroids_fall %>% mutate(Season = "Autumn"),
  centroids_winter %>% mutate(Season = "Winter")
)

bbox_both <- {
  b <- st_bbox(both)
  b[c("xmin","ymin")] <- b[c("xmin","ymin")] - 100000
  b[c("xmax","ymax")] <- b[c("xmax","ymax")] + 100000
  b
}

# ---------- Themes & scales ----------
base_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "lightcyan2", color = NA),
    plot.background  = element_rect(fill = "white",      color = NA),
    legend.position  = "bottom"
  )

year_scale <- scale_color_viridis_c(
  option = "plasma", direction = -1, name = "Year",
  limits = c(1999, 2023),
  breaks = seq(1999, 2023, by = 8)
)

fill_year_scale <- scale_fill_viridis_c(
  option = "plasma", direction = -1, name = "Year",
  limits = c(1999, 2023),
  breaks = seq(1999, 2023, by = 8)
)

# ---------- Plot: Autumn (título curto, sem legenda) ----------
p_autumn <- ggplot() +
  geom_sf(data = europe_neighbors_27700, fill = "grey95", color = "grey40", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, fill = NA, color = "grey30", linetype = "dashed", alpha = 0.8) +
  geom_sf(data = centroids_fall, aes(color = year), size = 3) +
  year_scale +
  coord_sf(xlim = c(bbox_fall["xmin"], bbox_fall["xmax"]),
           ylim = c(bbox_fall["ymin"], bbox_fall["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", bar_cols = c("grey60","white")) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering()) +
  labs(title = "Autumn", x = "Longitude", y = "Latitude") +
  base_theme +
  guides(color = "none") +
  theme(legend.position = "none")

# ---------- Plot: Winter (título curto, sem legenda) ----------
p_winter <- ggplot() +
  geom_sf(data = europe_neighbors_27700, fill = "grey95", color = "grey40", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, fill = NA, color = "grey30", linetype = "dashed", alpha = 0.8) +
  geom_sf(data = centroids_winter, aes(color = year), size = 3) +
  year_scale +
  coord_sf(xlim = c(bbox_winter["xmin"], bbox_winter["xmax"]),
           ylim = c(bbox_winter["ymin"], bbox_winter["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", bar_cols = c("grey60","white")) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering()) +
  labs(title = "Winter", x = "Longitude", y = "Latitude") +
  base_theme +
  guides(color = "none") +
  theme(legend.position = "none")

# ---------- Plot: Combined (Autumn + Winter) ----------
p_both <- ggplot() +
  geom_sf(data = europe_neighbors_27700, fill = "grey95", color = "grey40", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, fill = NA, color = "grey30", linetype = "dashed", alpha = 0.8) +
  geom_sf(
    data = both,
    aes(shape = Season, fill = year, size = Season),
    color = "grey25", stroke = 0.25
  ) +
  scale_shape_manual(values = c("Autumn" = 24, "Winter" = 22), name = "Season") +
  scale_size_manual(values = c("Autumn" = 3.0, "Winter" = 3.6), guide = "none") +
  fill_year_scale +
  coord_sf(xlim = c(bbox_both["xmin"], bbox_both["xmax"]),
           ylim = c(bbox_both["ymin"], bbox_both["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", bar_cols = c("grey60","white")) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering()) +
  labs(
    title = "Centroids of fishing activities — Autumn & Winter (1999–2023)",
    x = "Longitude", y = "Latitude",
    caption = "Colours indicate the temporal progression from early (1999) to recent (2023) fishing activity."
  ) +
  base_theme +
  guides(
    shape = guide_legend(order = 0),
    fill  = guide_colorbar(order = 1)
  ) +
  theme(
    plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(t = 8))
  )

# ---------- Final figure ----------
final_fig <- (p_both | (p_autumn / p_winter)) +
  plot_layout(widths = c(1.35, 1.15))

ggsave(
  filename = file.path(out_dir, "centroids_three_panel_figure.png"),
  plot = final_fig,
  width = 18, height = 10, dpi = 300, bg = "white"
)

#
#
#
#

# Versao dos centroides agora sem os mapinhas a direita #

# 📦 Packages
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(viridis)
library(lubridate)

# ---------- Diretório principal ----------
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop")
out_dir <- getwd()

# ---------- Paths ----------
path_fall   <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_fall/proxy_fishing_activity_fall_timeweighted.csv"
path_winter <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_winter/proxy_fishing_activity_winter_timeweighted.csv"
path_ices   <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/chapter2/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp"

# ---------- Base maps (seus shapefiles) ----------
shp_dir <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/shapefiles_shetland_europe_scotland"
path_scotland <- file.path(shp_dir, "OS_Scotland_WGS84.shp")
path_country  <- file.path(shp_dir, "country.shp")

scotland_os <- st_read(path_scotland, quiet = TRUE) %>% st_make_valid()
europe_countries <- st_read(path_country, quiet = TRUE) %>% st_make_valid()

if (is.na(st_crs(scotland_os))) st_crs(scotland_os) <- 4326
if (is.na(st_crs(europe_countries))) st_crs(europe_countries) <- 4326

scotland_os_27700 <- st_transform(scotland_os, 27700)
europe_countries_27700 <- st_transform(europe_countries, 27700)

candidate_cols <- c("name","name_en","country","admin","cntry_name","countryname")
orig_names  <- names(europe_countries_27700)
lower_names <- tolower(orig_names)
idx <- match(candidate_cols, lower_names)
idx <- idx[!is.na(idx)][1]

if (!is.na(idx)) {
  name_col_real <- orig_names[idx]
  europe_neighbors_27700 <- europe_countries_27700 %>%
    filter(.data[[name_col_real]] %in% c("United Kingdom","Ireland","France",
                                         "Belgium","Netherlands","Germany","Norway"))
} else {
  europe_neighbors_27700 <- europe_countries_27700
  message("Não achei coluna de nome em country.shp; usando todos os países.")
}

land_union <- st_union(europe_neighbors_27700)

# ICES em 27700
ICES_Areas <- st_read(path_ices, quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(27700)

# ---------- Helper: read proxy -> sf -> sea-only -> yearly centroid ----------
centroid_by_year <- function(csv_path) {
  df <- read.csv(csv_path, stringsAsFactors = FALSE) %>%
    mutate(year = lubridate::year(as.Date(date))) %>%
    filter(year >= 1999 & year <= 2023)
  
  sf_pts <- st_as_sf(df, coords = c("X_lon","Y_lat"), crs = 4326) %>% st_transform(27700)
  sea_pts <- sf_pts[!st_intersects(sf_pts, land_union, sparse = FALSE), ]
  
  cents <- sea_pts %>%
    group_by(year) %>%
    summarise(geometry = st_centroid(st_union(geometry)), .groups = "drop")
  
  # push-to-sea if on land
  on_land <- st_intersects(cents, land_union, sparse = FALSE)
  if (any(on_land)) {
    for (i in which(on_land)) {
      nearest_sea <- st_nearest_points(cents[i, ], st_boundary(land_union)) %>%
        st_cast("POINT") %>% .[2]
      st_geometry(cents[i, ]) <- nearest_sea
    }
  }
  
  coords <- st_coordinates(cents)
  cents  <- cents %>% mutate(X = coords[,1], Y = coords[,2])
  return(cents)
}

# ---------- Compute centroids ----------
centroids_fall   <- centroid_by_year(path_fall)
centroids_winter <- centroid_by_year(path_winter)

both <- rbind(
  centroids_fall %>% mutate(Season = "Autumn"),
  centroids_winter %>% mutate(Season = "Winter")
)

# ---------- Bounding box único ----------
bbox_both <- {
  b <- st_bbox(both)
  b[c("xmin","ymin")] <- b[c("xmin","ymin")] - 100000
  b[c("xmax","ymax")] <- b[c("xmax","ymax")] + 100000
  b
}

# ---------- Tema e escalas ----------
base_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "lightcyan2", color = NA),
    plot.background  = element_rect(fill = "white",      color = NA),
    legend.position  = "bottom",
    legend.box = "horizontal",              # lado a lado (Season esquerda, Year direita)
    legend.box.just = "center",
    legend.title = element_text(face = "bold", size = 11)
  )

fill_year_scale <- scale_fill_viridis_c(
  option = "plasma", direction = -1, name = "Year",
  limits = c(1999, 2023),
  breaks = seq(1999, 2023, by = 8)
)

# ---------- Plot único (Autumn + Winter) com rótulos ----------
p_both <- ggplot() +
  geom_sf(data = europe_neighbors_27700, fill = "grey95", color = "grey40", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, fill = NA, color = "grey30", linetype = "dashed", alpha = 0.8) +
  geom_sf(
    data = both,
    aes(shape = Season, fill = year, size = Season),
    color = "grey25", stroke = 0.25
  ) +
  # Winter levemente maior
  scale_shape_manual(values = c("Autumn" = 24, "Winter" = 22), name = "Season") +
  scale_size_manual(values = c("Autumn" = 3.0, "Winter" = 3.6), guide = "none") +
  fill_year_scale +
  # 1) rótulos de ano pretos com "linha fina" (segmento)
  geom_segment(
    data = both %>% st_drop_geometry(),
    aes(x = X, y = Y, xend = X, yend = Y + 18000),
    inherit.aes = FALSE,
    linewidth = 0.25,
    colour = "black",
    alpha = 0.9
  ) +
  geom_text(
    data = both %>% st_drop_geometry(),
    aes(x = X, y = Y + 20000, label = year),
    inherit.aes = FALSE,
    size = 3,
    colour = "black"
  ) +
  coord_sf(xlim = c(bbox_both["xmin"], bbox_both["xmax"]),
           ylim = c(bbox_both["ymin"], bbox_both["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", bar_cols = c("grey60","white")) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering()) +
  labs(
    title = "Centroids of fishing activities — Autumn & Winter (1999–2023)",
    x = "Longitude", y = "Latitude",
    caption = "Colours indicate the temporal progression from early (1999) to recent (2023) fishing activity."
  ) +
  base_theme +
  # 3) Legendas lado a lado e maiores na Season
  guides(
    shape = guide_legend(
      order = 0,
      title.position = "top",
      label.position = "bottom",
      override.aes = list(size = 6),   # aumenta triângulo/quadrado na legenda
      nrow = 1
    ),
    fill  = guide_colorbar(
      order = 1,
      title.position = "top",
      barwidth = 12,
      barheight = 0.35
    )
  ) +
  theme(
    plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(t = 8)),
    legend.spacing.x = unit(20, "pt")  # espaço entre Season e Year
  )

print(p_both)

ggsave(
  filename = file.path(out_dir, "centroids_single_map_season_year.png"),
  plot = p_both,
  width = 12, height = 10, dpi = 300, bg = "white"
)

#
#

# =========================================
# Versão SEM rótulos de ano
# =========================================

p_both_no_labels <- ggplot() +
  geom_sf(data = europe_neighbors_27700, fill = "grey95", color = "grey40", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, fill = NA, color = "grey30",
          linetype = "dashed", alpha = 0.8) +
  geom_sf(
    data = both,
    aes(shape = Season, fill = year, size = Season),
    color = "grey25", stroke = 0.25
  ) +
  scale_shape_manual(values = c("Autumn" = 24, "Winter" = 22), name = "Season") +
  scale_size_manual(values = c("Autumn" = 3.0, "Winter" = 3.6), guide = "none") +
  fill_year_scale +
  coord_sf(
    xlim = c(bbox_both["xmin"], bbox_both["xmax"]),
    ylim = c(bbox_both["ymin"], bbox_both["ymax"]),
    expand = FALSE
  ) +
  annotation_scale(location = "bl", bar_cols = c("grey60","white")) +
  annotation_north_arrow(location = "tl",
                         style = north_arrow_fancy_orienteering()) +
  labs(
    title = "Centroids of fishing activities — Autumn & Winter (1999–2023)",
    x = "Longitude", y = "Latitude",
    caption = "Colours indicate the temporal progression from early (1999) to recent (2023) fishing activity."
  ) +
  base_theme +
  guides(
    shape = guide_legend(
      order = 0,
      title.position = "top",
      override.aes = list(size = 6),
      nrow = 1
    ),
    fill = guide_colorbar(
      order = 1,
      title.position = "top",
      barwidth = 12,
      barheight = 0.35
    )
  ) +
  theme(
    plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(t = 8)),
    legend.spacing.x = unit(20, "pt")
  )

# Visualizar
print(p_both_no_labels)

# Salvar
ggsave(
  filename = file.path(out_dir, "centroids_single_map_no_year_labels.png"),
  plot = p_both_no_labels,
  width = 12, height = 10, dpi = 300, bg = "white"
)


#
#
#

# =========================================
# Versão COM rótulos de ano na cor do Year
# (texto colorido pela mesma escala do mapa)
# =========================================
library(ggrepel)

# dados sem geometria para labels
labels_df <- both %>% st_drop_geometry()

p_both_yearlabels_coloured <- ggplot() +
  geom_sf(data = europe_neighbors_27700, fill = "grey95", color = "grey40", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, fill = NA, color = "grey30",
          linetype = "dashed", alpha = 0.8) +
  geom_sf(
    data = both,
    aes(shape = Season, fill = year, size = Season),
    color = "grey25", stroke = 0.25
  ) +
  # rótulos: cor = ano (mesma paleta), com contorno fino
  ggrepel::geom_text_repel(
    data = labels_df,
    aes(x = X, y = Y, label = year, colour = year),
    size = 3,
    fontface = "plain",
    point.padding = 0.15,
    box.padding = 0.25,
    max.overlaps = Inf,
    min.segment.length = 0,
    segment.size = 0.25,
    segment.alpha = 0.6,
    # contorno fino para legibilidade em cima do mapa
    bg.color = "white",
    bg.r = 0.08
  ) +
  scale_shape_manual(values = c("Autumn" = 24, "Winter" = 22), name = "Season") +
  scale_size_manual(values = c("Autumn" = 3.0, "Winter" = 3.6), guide = "none") +
  # Year controla fill e colour juntos (mantém consistência e evita legenda duplicada)
  scale_fill_viridis_c(
    option = "plasma", direction = -1, name = "Year",
    limits = c(1999, 2023),
    breaks = seq(1999, 2023, by = 8)
  ) +
  scale_colour_viridis_c(
    option = "plasma", direction = -1,
    limits = c(1999, 2023),
    guide = "none"   # NÃO cria uma segunda legenda de Year
  ) +
  coord_sf(
    xlim = c(bbox_both["xmin"], bbox_both["xmax"]),
    ylim = c(bbox_both["ymin"], bbox_both["ymax"]),
    expand = FALSE
  ) +
  annotation_scale(location = "bl", bar_cols = c("grey60","white")) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering()) +
  labs(
    title = "Centroids of fishing activities — Autumn & Winter (1999–2023)",
    x = "Longitude", y = "Latitude",
    caption = "Colours indicate the temporal progression from early (1999) to recent (2023) fishing activity."
  ) +
  base_theme +
  guides(
    shape = guide_legend(
      order = 0,
      title.position = "top",
      override.aes = list(size = 6),
      nrow = 1
    ),
    fill = guide_colorbar(
      order = 1,
      title.position = "top",
      barwidth = 12,
      barheight = 0.35
    )
  ) +
  theme(
    plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(t = 8)),
    legend.spacing.x = unit(20, "pt")
  )

print(p_both_yearlabels_coloured)

ggsave(
  filename = file.path(out_dir, "centroids_single_map_coloured_year_labels.png"),
  plot = p_both_yearlabels_coloured,
  width = 12, height = 10, dpi = 300, bg = "white"
)







