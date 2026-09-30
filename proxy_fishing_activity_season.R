##################################################
# Proxy fishing activity (time-weighted centroid)#
# FALL & WINTER  — usando EPSG:27700             #
##################################################

# 📦 Pacotes
library(sf)
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)

# 📁 Diretório de trabalho (ajuste se quiser)
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets")

# 📥 Carregar dataset
df <- read.csv("tracks+vms+season.csv", stringsAsFactors = FALSE)

# ---- Parse seguro de datetime (silencioso) e drop de NAs ----
df <- df %>%
  mutate(datetime = ymd_hms(datetime, quiet = TRUE, tz = "UTC")) %>%
  filter(!is.na(datetime))

# -------------------------
# Funções auxiliares
# -------------------------
# Converte um ponto (lon,lat) de 4326 para 27700 e retorna vetor c(X, Y)
lonlat_to_27700 <- function(lon, lat) {
  st_coordinates(
    st_transform(st_sfc(st_point(c(lon, lat)), crs = 4326), 27700)
  )[1,]
}

# Constrói grid 5x5 km em 27700, a partir do bbox de uma camada em 27700,
# com clip pelos limites fornecidos (em graus, convertidos para 27700)
make_grid_5km_27700 <- function(bbox_sfc_27700, x_max_lon = 7, y_max_lat = 64, y_min_lat) {
  # converter limites para 27700
  xlim_e <- lonlat_to_27700(x_max_lon, 0)[1]     # limite leste
  ylim_n <- lonlat_to_27700(0, y_max_lat)[2]     # limite norte
  ylim_s <- lonlat_to_27700(0, y_min_lat)[2]     # limite sul
  
  bb <- st_bbox(bbox_sfc_27700)
  bb["xmax"] <- min(bb["xmax"], xlim_e)
  bb["ymax"] <- min(bb["ymax"], ylim_n)
  bb["ymin"] <- max(bb["ymin"], ylim_s)
  
  # cria a grade 5 km (polígonos) e monta o sf com grid_id
  grid_polys <- st_make_grid(st_as_sfc(bb),
                             cellsize = c(5000, 5000),
                             what = "polygons",
                             square = TRUE)
  grid <- st_sf(grid_id = seq_along(grid_polys), geometry = grid_polys)
  st_crs(grid) <- 27700
  grid
}

# Calcula proxy por season: time-weighted centroid por (grid_id, date, VE_ID)
make_proxy_timeweighted <- function(df_season_27700, season_name, y_min_lat) {
  
  # 1) BBox base (27700)
  bbox_sfc <- st_as_sfc(st_bbox(df_season_27700))
  
  # 2) Grid 5x5 km em 27700 (recorte por limites)
  grid <- make_grid_5km_27700(bbox_sfc,
                              x_max_lon = 7,
                              y_max_lat = 64,
                              y_min_lat = y_min_lat)
  
  # 3) Máscara de terra (27700) e remoção de células que intersectam terra
  world <- ne_countries(scale = "medium", returnclass = "sf")
  land_union_27700 <- world %>%
    filter(name_en %in% c("United Kingdom","Ireland","France",
                          "Belgium","Netherlands","Germany","Norway")) %>%
    st_transform(27700) %>%
    st_union()
  
  grid_sea_only <- grid[!st_intersects(grid, land_union_27700, sparse = FALSE), ]
  grid_sea_only <- st_sf(grid_id = grid_sea_only$grid_id, geometry = st_geometry(grid_sea_only))
  
  # 4) Join pontos -> grid (em 27700)
  joined <- st_join(df_season_27700, grid_sea_only, join = st_intersects)
  
  # 5) Calcular peso = tempo até o próximo ping (cap = 30 min) por (VE_ID, grid_id, date)
  cap_s <- 30 * 60
  
  # coordenadas dos pings em 27700
  coords_m <- st_coordinates(joined)
  
  joined2 <- joined %>%
    mutate(
      X_m = coords_m[,1],
      Y_m = coords_m[,2],
      # datetime já é POSIXct; só garantir date:
      date = as.Date(datetime)
    ) %>%
    st_drop_geometry() %>%
    filter(!is.na(grid_id)) %>%
    arrange(VE_ID, grid_id, date, datetime) %>%
    group_by(grid_id, date, VE_ID) %>%
    mutate(
      dt_s_next = as.numeric(difftime(lead(datetime), datetime, units = "secs")),
      w_s = pmin(coalesce(dt_s_next, 0), cap_s)   # último ping do grupo -> 0
    ) %>%
    ungroup()
  
  # 6) Agregar: centróide ponderado por tempo (27700) + métricas auxiliares
  proxy <- joined2 %>%
    group_by(grid_id, date, VE_ID) %>%
    summarise(
      n_pings = n(),
      minutes_covered = sum(w_s, na.rm = TRUE) / 60,
      Xw_27700 = ifelse(sum(w_s, na.rm = TRUE) > 0,
                        sum(X_m * w_s, na.rm = TRUE) / sum(w_s, na.rm = TRUE),
                        mean(X_m, na.rm = TRUE)),
      Yw_27700 = ifelse(sum(w_s, na.rm = TRUE) > 0,
                        sum(Y_m * w_s, na.rm = TRUE) / sum(w_s, na.rm = TRUE),
                        mean(Y_m, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(season = season_name)
  
  # 7) Também em lon/lat (WGS84) para mapas
  cent_sf <- st_as_sf(proxy, coords = c("Xw_27700","Yw_27700"), crs = 27700) %>%
    st_transform(4326)
  xy_ll <- st_coordinates(cent_sf)
  proxy$X_lon <- xy_ll[,1]
  proxy$Y_lat <- xy_ll[,2]
  
  # 8) Salvar CSV por season
  out_csv <- paste0("proxy_fishing_activity_", season_name, "_timeweighted.csv")
  write.csv(proxy, out_csv, row.names = FALSE)
  cat("✅ CSV salvo como:", out_csv, "\n")
  
  # 9) Salvar o grid (Shapefile) com dsn=dir e layer=nome (evita o warning do GDAL)
  st_write(
    grid_sea_only,
    dsn   = getwd(),
    layer = paste0("grid_5km_", season_name, "_sea_only_27700"),
    driver = "ESRI Shapefile",
    delete_layer = TRUE,
    quiet = TRUE
  )
  cat("✅ Shapefile salvo como: ", file.path(getwd(), paste0("grid_5km_", season_name, "_sea_only_27700.shp")), "\n", sep = "")
  
  return(proxy)
}

# --------------------------------
# Preparo comum (sf em 27700)
# --------------------------------
df_sf_27700 <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(27700)

# Filtrar por season e anos (1999–2023), já no sf 27700
df_fall_27700 <- df_sf_27700 %>%
  filter(season == "fall", year >= 1999, year <= 2023)

df_winter_27700 <- df_sf_27700 %>%
  filter(season == "winter", year >= 1999, year <= 2023)

# -------------------------
# Rodar FALL (y_min_lat = 52) e WINTER (y_min_lat = 45)
# -------------------------
proxy_fall   <- make_proxy_timeweighted(df_fall_27700,   season_name = "fall",   y_min_lat = 52)
proxy_winter <- make_proxy_timeweighted(df_winter_27700, season_name = "winter", y_min_lat = 45)

# -------------------------
# CSV combinado
# -------------------------
proxy_all <- bind_rows(proxy_fall, proxy_winter)
write.csv(proxy_all, "proxy_fishing_activity_all_timeweighted.csv", row.names = FALSE)
cat("✅ CSV combinado salvo como: proxy_fishing_activity_all_timeweighted.csv\n")

#
#
#

# Auditoria dos dados antes e apos proxy #

library(dplyr)

df_before <- read.csv("tracks+vms+season.csv", stringsAsFactors = FALSE)

# Total de pings
n_before_total <- nrow(df_before)

# Por season
n_before_season <- df_before %>%
  count(season, name = "n_pings_before")

n_before_total
n_before_season


df_after <- read.csv("proxy_fishing_activity_all_timeweighted.csv",
                     stringsAsFactors = FALSE)

# Total de proxies
n_after_total <- nrow(df_after)

# Por season
n_after_season <- df_after %>%
  count(season, name = "n_proxy_records")

n_after_total
n_after_season


summary_proxy_counts <- n_before_season %>%
  full_join(n_after_season, by = "season") %>%
  mutate(
    n_pings_before  = replace_na(n_pings_before, 0),
    n_proxy_records = replace_na(n_proxy_records, 0),
    reduction_ratio = round(n_proxy_records / n_pings_before, 4)
  )

summary_proxy_counts


write.csv(summary_proxy_counts,
          "audit_proxy_counts_before_after_by_season.csv",
          row.names = FALSE)








