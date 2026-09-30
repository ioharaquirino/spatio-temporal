######################
#                    #  
# Ocupacao da celula #
#                    #
######################

# What am I exploring? #

# (A) Linha temporal #
# tendência geral: o tempo médio por célula aumenta ou diminui?
# mudanças estruturais (ex.: picos e estabilização).
# diferenças sazonais (autumn vs winter).

# (B) Número de células ativas por mes #
# percepção de amplitude do esforço espacial (como a frota “espalha” seu esforço por dia/ano);
# permite relacionar picos de concentração (poucas células + alto tempo médio) com mudanças espaço-temporais da KDE.


#########################
#   Autumn | winter)    #
#########################

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)
library(scales)
library(patchwork)

# ------ Caminhos ------
proxy_fall   <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_fall/proxy_fishing_activity_fall_timeweighted.csv"
proxy_winter <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_winter/proxy_fishing_activity_winter_timeweighted.csv"

# ------ Detectar delimitador e ler ------
detect_delim <- function(path) {
  first <- readr::read_lines(path, n_max = 1)
  counts <- c(","  = stringr::str_count(first, ","),
              ";"  = stringr::str_count(first, ";"),
              "\t" = stringr::str_count(first, "\t"))
  names(which.max(counts))
}
read_any <- function(path) {
  delim <- detect_delim(path)
  readr::read_delim(path, delim = delim, show_col_types = FALSE,
                    locale = readr::locale(encoding = "UTF-8"),
                    col_types = cols(.default = col_character()))
}

# ------ Leitor robusto do proxy (usa APENAS a célula existente) ------
read_proxy <- function(path) {
  raw <- read_any(path)
  names(raw) <- trimws(tolower(names(raw)))
  
  date_col   <- intersect(names(raw), c("date","data","datetime","timestamp","time"))[1]
  season_col <- "season"
  proxy_col  <- intersect(names(raw), c("minutes_covered","minutes_c","minutes","proxy","proxy_value","activity_proxy"))[1]
  ve_col     <- intersect(names(raw), c("ve_id","veid","vessel_id","vessel"))[1]
  cell_col   <- intersect(names(raw), c("cell_id","cellid","grid_id","gridid","cell","grid","cell_key","cellcode","id_cell"))[1]
  
  if (is.na(date_col))   stop("Não encontrei coluna de data em: ", basename(path))
  if (is.na(proxy_col))  stop("Não encontrei coluna de proxy (minutos) em: ", basename(path))
  if (is.na(ve_col))     stop("Não encontrei coluna de VE_ID em: ", basename(path))
  if (!season_col %in% names(raw)) stop("Não encontrei coluna 'season' em: ", basename(path))
  if (is.na(cell_col))   stop("Não encontrei coluna de célula (ex.: cell_id/grid_id) em: ", basename(path))
  
  # parse datas
  x  <- raw[[date_col]] %>% stringr::str_replace_all("\\u00A0"," ") %>% stringr::str_squish()
  dt <- as.Date(x, format = "%d/%m/%Y")
  na1 <- is.na(dt); if (any(na1)) dt[na1] <- as.Date(sub(" .*","",x[na1]), format="%d/%m/%Y")
  na2 <- is.na(dt); if (any(na2)) dt[na2] <- suppressWarnings(lubridate::ymd(x[na2]))
  na3 <- is.na(dt)
  if (any(na3)) {
    num <- suppressWarnings(as.numeric(x[na3])); is_num <- !is.na(num)
    if (any(is_num)) dt[na3][is_num] <- as.Date(num[is_num], origin = "1899-12-30")
  }
  
  out <- tibble(
    season     = tolower(trimws(raw[[season_col]])),
    date       = dt,
    year       = lubridate::year(dt),
    VE_ID      = suppressWarnings(as.integer(readr::parse_number(raw[[ve_col]]))),
    proxy_min  = suppressWarnings(readr::parse_number(raw[[proxy_col]],
                                                      locale = readr::locale(decimal_mark = ".", grouping_mark = ","))),
    cell_id    = as.character(raw[[cell_col]])
  ) %>%
    filter(!is.na(date),
           season %in% c("fall","winter"),
           !is.na(year), year >= 1999, year <= 2023)
  
  message(basename(path), ": mantidas ", nrow(out), " linhas válidas.")
  out
}

# ------ Ler e unir ------
pf <- read_proxy(proxy_fall)
pw <- read_proxy(proxy_winter)

proxy_data <- bind_rows(pf, pw) %>%
  mutate(season = dplyr::recode(season, "fall" = "autumn"),
         season = factor(season, levels = c("autumn","winter")))

# ------ Paleta fixa por estação ------
pal_season <- c(autumn = "#D35400", winter = "#1F4E79")

# ==============================
# A) Tendência do tempo médio por célula (anual) — com smoothing
# ==============================
df_mean <- proxy_data %>%
  filter(proxy_min > 0) %>%
  group_by(year, season) %>%
  summarise(mean_proxy = mean(proxy_min, na.rm = TRUE), .groups = "drop")

pA <- ggplot(df_mean, aes(x = year, y = mean_proxy, color = season)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_point(size = 1.8) +
  geom_smooth(method = "loess", span = 0.3, se = TRUE, linewidth = 0.9) +
  facet_wrap(~season, ncol = 2, scales = "free_y") +   # <- menos "achatado"
  scale_color_manual(values = pal_season, guide = "none") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "Mean time occupied per cell", x = "Year", y = "Minutes") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

# ==============================
# B) Nº de células ativas por ano — com smoothing
# ==============================

df_cells_y <- proxy_data %>%
  filter(proxy_min > 0) %>%
  group_by(year, season) %>%
  summarise(active_cells = n_distinct(cell_id), .groups = "drop")

pB <- ggplot(df_cells_y, aes(x = year, y = active_cells)) +
  # 1) DADOS BRUTOS: linha cinza por baixo
  geom_line(
    aes(group = season),
    colour    = "grey70",
    linewidth = 0.6,
    alpha     = 0.9
  ) +
  # 2) SMOOTH: linha colorida + faixa cinza completa (SE)
  geom_smooth(
    aes(color = season),
    method    = "loess",
    span      = 0.4,
    se        = TRUE,      # 👈 faixa cinza ativada
    linewidth = 1.2
  ) +
  # 3) PONTOS (cores por estação) por cima
  geom_point(
    aes(color = season),
    size  = 1.5,
    alpha = 0.9
  ) +
  facet_wrap(~season, ncol = 2, scales = "free_y") +
  scale_color_manual(values = pal_season, guide = "none") +
  scale_x_continuous(
    breaks  = seq(1999, 2023, by = 2),
    limits  = c(1999, 2023)
  ) +
  # 4) Eixo Y: deixa a banda passar abaixo de 0, mas labels começam em 0
  scale_y_continuous(
    limits = c(-50, NA),                 # 👈 permite continuar a banda sem cortar
    breaks = seq(0, 1000, by = 100),     # 👈 ticks só >= 0
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Active cells per year",
    x     = "Year",
    y     = "Active cells (annual)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

# ==============================
# Painel único (A / B) — com tags A, B (sem título geral)
# ==============================
# Painel
panel <- (pA / pB) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(face = "bold", size = 14))
  )

# Caminho da Área de Trabalho
out_path <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/time_occupied_by_cell_panel_AB.png"

ggsave(
  filename = out_path,
  plot     = panel,
  width    = 12,
  height   = 10,
  dpi      = 300
)

# Salvar (aumente a altura para dar ainda mais “respiro” vertical)
ggsave("time_occupied_by_cell_panel_AB.png", panel, width = 12, height = 10, dpi = 300)



#
#
#

######################
#                    #  
# Ocupacao da celula #
#                    #
######################

# What am I exploring? #

# (A) Linha temporal #
# tendência geral: o tempo médio por célula aumenta ou diminui?
# mudanças estruturais (ex.: picos e estabilização).
# diferenças sazonais (autumn vs winter).

# (B) Número de células ativas por mes #
# percepção de amplitude do esforço espacial (como a frota “espalha” seu esforço por dia/ano);
# permite relacionar picos de concentração (poucas células + alto tempo médio) com mudanças espaço-temporais da KDE.


#########################
#   Autumn | Winter     #
#########################

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)
library(scales)
library(patchwork)

# ------ Caminhos ------
proxy_fall   <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_fall/proxy_fishing_activity_fall_timeweighted.csv"
proxy_winter <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_winter/proxy_fishing_activity_winter_timeweighted.csv"

# ------ Detectar delimitador e ler ------
detect_delim <- function(path) {
  first <- readr::read_lines(path, n_max = 1)
  counts <- c(","  = stringr::str_count(first, ","),
              ";"  = stringr::str_count(first, ";"),
              "\t" = stringr::str_count(first, "\t"))
  names(which.max(counts))
}
read_any <- function(path) {
  delim <- detect_delim(path)
  readr::read_delim(path, delim = delim, show_col_types = FALSE,
                    locale = readr::locale(encoding = "UTF-8"),
                    col_types = cols(.default = col_character()))
}

# ------ Leitor robusto do proxy (usa APENAS a célula existente) ------
read_proxy <- function(path) {
  raw <- read_any(path)
  names(raw) <- trimws(tolower(names(raw)))
  
  date_col   <- intersect(names(raw), c("date","data","datetime","timestamp","time"))[1]
  season_col <- "season"
  proxy_col  <- intersect(names(raw), c("minutes_covered","minutes_c","minutes","proxy","proxy_value","activity_proxy"))[1]
  ve_col     <- intersect(names(raw), c("ve_id","veid","vessel_id","vessel"))[1]
  cell_col   <- intersect(names(raw), c("cell_id","cellid","grid_id","gridid","cell","grid","cell_key","cellcode","id_cell"))[1]
  
  if (is.na(date_col))   stop("Não encontrei coluna de data em: ", basename(path))
  if (is.na(proxy_col))  stop("Não encontrei coluna de proxy (minutos) em: ", basename(path))
  if (is.na(ve_col))     stop("Não encontrei coluna de VE_ID em: ", basename(path))
  if (!season_col %in% names(raw)) stop("Não encontrei coluna 'season' em: ", basename(path))
  if (is.na(cell_col))   stop("Não encontrei coluna de célula (ex.: cell_id/grid_id) em: ", basename(path))
  
  # parse datas
  x  <- raw[[date_col]] %>% stringr::str_replace_all("\\u00A0"," ") %>% stringr::str_squish()
  dt <- as.Date(x, format = "%d/%m/%Y")
  na1 <- is.na(dt); if (any(na1)) dt[na1] <- as.Date(sub(" .*","",x[na1]), format="%d/%m/%Y")
  na2 <- is.na(dt); if (any(na2)) dt[na2] <- suppressWarnings(lubridate::ymd(x[na2]))
  na3 <- is.na(dt)
  if (any(na3)) {
    num <- suppressWarnings(as.numeric(x[na3])); is_num <- !is.na(num)
    if (any(is_num)) dt[na3][is_num] <- as.Date(num[is_num], origin = "1899-12-30")
  }
  
  out <- tibble(
    season     = tolower(trimws(raw[[season_col]])),
    date       = dt,
    year       = lubridate::year(dt),
    VE_ID      = suppressWarnings(as.integer(readr::parse_number(raw[[ve_col]]))),
    proxy_min  = suppressWarnings(readr::parse_number(raw[[proxy_col]],
                                                      locale = readr::locale(decimal_mark = ".", grouping_mark = ","))),
    cell_id    = as.character(raw[[cell_col]])
  ) %>%
    filter(!is.na(date),
           season %in% c("fall","winter"),
           !is.na(year), year >= 1999, year <= 2023)
  
  message(basename(path), ": mantidas ", nrow(out), " linhas válidas.")
  out
}

# ------ Ler e unir ------
pf <- read_proxy(proxy_fall)
pw <- read_proxy(proxy_winter)

proxy_data <- bind_rows(pf, pw) %>%
  mutate(
    season = dplyr::recode(season, "fall" = "autumn"),
    season = factor(season, levels = c("autumn","winter"))
  )

# ------ Paleta fixa por estação ------
pal_season <- c(autumn = "#D35400", winter = "#1F4E79")

# ------ LABS DO FACET (capitalize) ------
facet_labs <- c(autumn = "Autumn", winter = "Winter")

# ==============================
# A) Tendência do tempo médio por célula (anual) — com smoothing
# ==============================
df_mean <- proxy_data %>%
  filter(proxy_min > 0) %>%
  group_by(year, season) %>%
  summarise(mean_proxy = mean(proxy_min, na.rm = TRUE), .groups = "drop")

pA <- ggplot(df_mean, aes(x = year, y = mean_proxy, color = season)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_point(size = 1.8) +
  geom_smooth(method = "loess", span = 0.3, se = TRUE, linewidth = 0.9) +
  facet_wrap(~season, ncol = 2, labeller = labeller(season = facet_labs)) +  # <- FIX: capitalize + same y
  scale_color_manual(values = pal_season, guide = "none") +
  scale_x_continuous(
    breaks = seq(1999, 2023, by = 2),
    limits = c(1999, 2023)
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "Mean time occupied per cell", x = "Year", y = "Minutes") +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

# ==============================
# B) Nº de células ativas por ano — com smoothing
# ==============================
df_cells_y <- proxy_data %>%
  filter(proxy_min > 0) %>%
  group_by(year, season) %>%
  summarise(active_cells = n_distinct(cell_id), .groups = "drop")

pB <- ggplot(df_cells_y, aes(x = year, y = active_cells)) +
  # 1) DADOS BRUTOS: linha cinza por baixo
  geom_line(
    aes(group = season),
    colour    = "grey70",
    linewidth = 0.6,
    alpha     = 0.9
  ) +
  # 2) SMOOTH: linha colorida + faixa cinza completa (SE)
  geom_smooth(
    aes(color = season),
    method    = "loess",
    span      = 0.4,
    se        = TRUE,
    linewidth = 1.2
  ) +
  # 3) PONTOS (cores por estação) por cima
  geom_point(
    aes(color = season),
    size  = 1.5,
    alpha = 0.9
  ) +
  facet_wrap(~season, ncol = 2, labeller = labeller(season = facet_labs)) + # <- FIX: capitalize + same y
  scale_color_manual(values = pal_season, guide = "none") +
  scale_x_continuous(
    breaks  = seq(1999, 2023, by = 2),
    limits  = c(1999, 2023)
  ) +
  # Eixo Y FIXO entre as estações (sem free_y):
  # - mantém banda SE sem cortar no 0, mas ticks a partir de 0
  scale_y_continuous(
    limits = c(-50, NA),
    breaks = seq(0, 1000, by = 100),
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Active cells per year",
    x     = "Year",
    y     = "Active cells (annual)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

# ==============================
# Painel único (A / B) — com tags A, B (sem título geral)
# ==============================
panel <- (pA / pB) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(face = "bold", size = 14))
  )

# Caminho da Área de Trabalho
out_path <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/time_occupied_by_cell_panel_AB.png"

ggsave(
  filename = out_path,
  plot     = panel,
  width    = 12,
  height   = 10,
  dpi      = 300
)

# Salvar também no working directory
ggsave("time_occupied_by_cell_panel_AB.png", panel, width = 12, height = 10, dpi = 300)
