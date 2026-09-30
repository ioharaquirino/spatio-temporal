
# Unir VMS com o Plotter-tracks data #

# Uniao de vessels do VMS data que nao participaram na coleta de dados do plotter data #

# Diretório
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets")


library(dplyr)
library(stringr)
library(readr)

# --- Helpers ---
norm <- function(x) stringr::str_to_lower(stringr::str_squish(as.character(x)))

# =====================
# 1) Dados de entrada
# =====================

# Plotter data (NÃO filtrar! Mantém todos os anos)
df_tracks <- read.csv("tracks_final.csv") %>%
  mutate(year = as.integer(year))

# VMS data
df_vms <- read.csv("vms_final.csv") %>%
  rename(
    year = Year,
    month = Month,
    lon = Longitude,
    lat = Latitude,
    datetime = Datetime,
    speed_kn_2 = Speed
  ) %>%
  mutate(year = as.integer(year))

# =====================
# 2) Comparação só no período 2013–2022
# =====================

# Versão filtrada do tracks apenas para comparar com VMS
df_tracks_periodo <- df_tracks %>%
  filter(year >= 2013, year <= 2022)

# Criar chaves VE_ID–year
plotter_keys_yr <- df_tracks_periodo %>%
  transmute(VE_ID_norm = norm(VE_ID), year) %>%
  distinct()

vms_keys_yr <- df_vms %>%
  transmute(VE_ID_norm = norm(VE_ID), year) %>%
  distinct()

# =====================
# 3) Seleção do que adicionar do VMS
# =====================

# (a) Vessels que só aparecem no VMS
vessels_only_in_vms <- setdiff(unique(vms_keys_yr$VE_ID_norm),
                               unique(plotter_keys_yr$VE_ID_norm))

vms_rows_vessels_only <- df_vms %>%
  mutate(VE_ID_norm = norm(VE_ID)) %>%
  filter(VE_ID_norm %in% vessels_only_in_vms) %>%
  select(-VE_ID_norm)

# (b) Vessels que existem no plotter mas têm anos exclusivos no VMS
vessels_in_plotter <- plotter_keys_yr %>% distinct(VE_ID_norm)

vms_only_vessel_year <- anti_join(
  vms_keys_yr, plotter_keys_yr, by = c("VE_ID_norm","year")
) %>%
  semi_join(vessels_in_plotter, by = "VE_ID_norm")

vms_rows_plotter_vessels_years_only <- df_vms %>%
  mutate(VE_ID_norm = norm(VE_ID)) %>%
  semi_join(vms_only_vessel_year, by = c("VE_ID_norm","year")) %>%
  select(-VE_ID_norm)

# Dataset final do VMS que deve ser adicionado
vms_rows_to_add <- bind_rows(
  vms_rows_vessels_only,
  vms_rows_plotter_vessels_years_only
) %>%
  mutate(data_source = "vms")

# =====================
# 4) União final
# =====================

# Aqui usamos o df_tracks completo (todos os anos), apenas marcando origem
tracks_combined <- bind_rows(
  df_tracks %>% mutate(data_source = "plotter"),
  vms_rows_to_add
)

# =====================
# 5) Checagens
# =====================

# Checar se há duplicações VE_ID–year entre tracks e VMS adicionados
stopifnot(
  nrow(
    inner_join(
      vms_rows_to_add %>% mutate(VE_ID_norm = norm(VE_ID)) %>% distinct(VE_ID_norm, year),
      plotter_keys_yr,
      by = c("VE_ID_norm","year")
    )
  ) == 0
)

# Resuminho
dplyr::count(tracks_combined, data_source)

# =====================
# 6) Exportar (opcional)
# =====================
write_csv(vms_rows_to_add, "vms_rows_to_add.csv")
write_csv(tracks_combined, "tracks_combined.csv")

# Diagnostico data # 

diagnostico <- data.frame(
  Fonte = c("Plotter data (original)",
            "VMS data (original)",
            "VMS exclusivo (a adicionar)",
            "Total combinado"),
  Linhas = c(nrow(df_tracks),
             nrow(df_vms),
             nrow(vms_rows_to_add),
             nrow(tracks_combined))
)

print(diagnostico)


