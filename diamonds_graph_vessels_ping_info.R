#########################
#                       #
#    DIAMONDS GRAPH     #
#                       #
#########################

# 📁 Diretório
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets")

# 📥 Carregar dataset
df <- read.csv("tracks+vms_final.csv")

library(dplyr)
library(ggplot2)
library(ggnewscale)
library(lubridate)


# Converter datetime corretamente
df <- df %>% mutate(datetime = ymd_hms(datetime))

# --- Criar tabela de presença diária ---
daily_presence <- df %>%
  mutate(
    datetime = ymd_hms(datetime),
    year = year(datetime),
    julian_day = yday(datetime)
  ) %>%
  filter(!is.na(year), year >= 1999, year <= 2023) %>%
  group_by(season, year, julian_day) %>%
  summarise(
    vessels_present = n_distinct(VE_ID),
    total_pings = n(),
    .groups = "drop"
  )

# Diagnostico vessels and pings #

diagnostico <- daily_presence %>%
  summarise(
    max_vessels = max(vessels_present, na.rm = TRUE),
    q95_vessels = quantile(vessels_present, 0.95, na.rm = TRUE),
    max_pings   = max(total_pings, na.rm = TRUE),
    min_pings   = min(total_pings, na.rm = TRUE),
    q95_pings   = quantile(total_pings, 0.95, na.rm = TRUE),
    mean_pings  = mean(total_pings, na.rm = TRUE),
    median_pings = median(total_pings, na.rm = TRUE)
  )

print(diagnostico)


# --- Filtrar apenas fall e winter ---
daily_presence_filtered <- daily_presence %>%
  filter(season %in% c("fall","winter")) %>%
  mutate(
    year = as.integer(year),
    julian_day = as.integer(julian_day)
  )

# --- Plot ---
ggplot(daily_presence_filtered, aes(x = julian_day, y = as.factor(year))) +

  # === CAMADA 1: PINGS (diamantes coloridos, sempre maiores) ===
  geom_point(
    aes(size = total_pings, fill = total_pings),
    shape = 23, colour = "black", alpha = 0.7, show.legend = TRUE
  ) +
  scale_size_continuous(
    range  = c(3.5, 7),                  
    limits = c(1, 20565),
    breaks = c(1000, 5000, 10000, 15000, 20000),
    name   = "Colored diamonds size = Total pings",
    guide  = guide_legend(order = 2, nrow = 1,
                          override.aes = list(shape = 23, fill = "grey70"))
  ) +
  scale_fill_gradientn(
    colours = c("yellow", "orange", "red", "darkred"),
    values = scales::rescale(c(1, 5000, 10000, 15000, 20565)),
    name   = "Colored diamonds fill = Total pings",
    breaks = c(5000, 10000, 15000, 20565),
    labels = c("≤5k", "5k–10k", "10k–15k", "15k–20.5k"),
    guide  = guide_colourbar(order = 3, barwidth = 10, barheight = 0.3)
  ) +
  
  # === REINICIAR ESCALA DE TAMANHO PARA VESSELS ===
  ggnewscale::new_scale("size") +
  
  # === CAMADA 2: VESSELS (diamantes pretos, menores e por cima) ===
  geom_point(
    aes(size = vessels_present),
    shape = 23, fill = "black", colour = "black", alpha = 0.9, show.legend = TRUE
  ) +
  scale_size_continuous(
    range  = c(1, 3),                     # sempre menor que os coloridos
    limits = c(1, 16),
    breaks = c(2, 6, 12, 16),
    name   = "Black diamonds size = Vessels per day",
    guide  = guide_legend(order = 1, nrow = 1,
                          override.aes = list(shape = 23, fill = "black"))
  ) +
  
  facet_wrap(~season, ncol = 2, scales = "free_x") +
  labs(
    title = "Daily presence of vessels and pings by season",
    x = "Julian Day",
    y = "Year"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "center",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text  = element_text(size = 7)
  )


# Checagem de possiveis outliers #

# Quais dias "repetem" em muitos anos?

# por season e julian_day, quantos anos têm presença

streaks <- daily_presence %>%
  filter(season %in% c("fall","winter")) %>%
  group_by(season, julian_day) %>%
  summarise(
    n_years = n_distinct(year),      # em quantos anos esse dia apareceu
    n_points = n(),                  # total de pontos (ano-dia) agregados
    sum_pings = sum(total_pings),    # pings totais (across anos) nesse dia
    med_vessels = median(vessels_present),
    .groups = "drop"
  ) %>%
  arrange(desc(n_years), desc(sum_pings))

head(streaks, 20)  # top 20 dias com maior recorrência

# Marcar suspeitos automaticamente #

thr <- quantile(streaks$n_years, 0.95, na.rm = TRUE)
suspects <- streaks %>% filter(n_years >= thr)
suspects

# Transformar para calendario (mes-dia) e checar padrao de semana

to_mmdd <- function(yday) format(as.Date(yday - 1, origin = "2001-01-01"), "%m-%d")
suspects <- suspects %>%
  mutate(
    mmdd   = to_mmdd(julian_day),
    wday   = lubridate::wday(as.Date(julian_day - 1, origin = "2001-01-01"),
                             label = TRUE, abbr = TRUE)
  )
suspects %>% arrange(season, julian_day)

# Pegar os registros (ano-dia) que formam cada linha 

# todos os ano-dias que compõem os suspeitos
suspect_days <- suspects %>% select(season, julian_day)

linhas_detalhe <- daily_presence %>%
  semi_join(suspect_days, by = c("season","julian_day")) %>%
  arrange(season, julian_day, year)

# Se quiser ver um exemplo específico (dia mais recorrente):
top_day <- suspects %>% slice_max(n_years, n = 1)
contrib_top <- daily_presence %>%
  semi_join(top_day, by = c("season","julian_day")) %>%
  arrange(year)
contrib_top

# SE decidir remover essas linhas do grafico

daily_presence_sem_linhas <- daily_presence %>%
  anti_join(suspect_days, by = c("season","julian_day"))


# Um único barco dominando o dia? #

dominance <- df %>%
  mutate(year = year(datetime), julian_day = yday(datetime)) %>%
  filter(season %in% c("fall","winter")) %>%
  group_by(season, year, julian_day, VE_ID) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  group_by(season, year, julian_day) %>%
  summarise(max_pct = max(pct), .groups = "drop") %>%
  arrange(desc(max_pct))
head(dominance, 20)


#
#

tod <- df %>%
  mutate(year = year(datetime),
         julian_day = yday(datetime),
         hour = lubridate::hour(datetime)) %>%
  semi_join(suspects, by = c("season","julian_day")) %>%
  count(season, julian_day, hour)

View (tod)


#
#

midnight_fall293 <- df %>%
  mutate(year = lubridate::year(datetime),
         julian_day = lubridate::yday(datetime),
         hour = lubridate::hour(datetime),
         minute = lubridate::minute(datetime),
         second = lubridate::second(datetime)) %>%
  filter(season == "fall", julian_day == 293, hour == 0)

# distribuição de minutos/segundos
count_ms <- midnight_fall293 %>%
  count(minute, second) %>%
  arrange(desc(n))
head(count_ms, 10)

# % dos registros exatamente às 00:00:00
share_exact_midnight <- midnight_fall293 %>%
  summarise(pct_exact = mean(minute==0 & second==0))
share_exact_midnight


by_vessel_midnight <- midnight_fall293 %>%
  count(VE_ID) %>%
  arrange(desc(n))
top5 <- head(by_vessel_midnight, 5)
top5; sum(top5$n) / sum(by_vessel_midnight$n)  # % dominado pelos 5


#
#
#

#########################
# DIAMONDS (Proxy-only) — usando date "DD/MM/YYYY"
#########################

library(dplyr)
library(ggplot2)
library(ggnewscale)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

# ------ Caminhos ------
proxy_fall   <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/dataset_final/proxy_grid_fall/proxy_fishing_activity_fall.csv"
proxy_winter <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/dataset_final/proxy_grid_winter/proxy_fishing_activity_winter.csv"

`%nin%` <- Negate(`%in%`)

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

# ------ Leitor robusto do proxy ------
read_proxy <- function(path) {
  raw <- read_any(path)
  
  # padroniza nomes (minúsculo + trim)
  names(raw) <- trimws(tolower(names(raw)))
  
  # colunas esperadas (com tolerância)
  date_col   <- intersect(names(raw), c("date","data","datetime","timestamp","time"))[1]
  season_col <- "season"
  proxy_col  <- intersect(names(raw), c("minutes_covered","minutes_c","minutes","proxy","proxy_value","activity_proxy"))[1]
  ve_col     <- intersect(names(raw), c("ve_id","veid","vessel_id","vessel"))[1]
  
  if (is.na(date_col))   stop("Não encontrei coluna de data em: ", basename(path))
  if (is.na(proxy_col))  stop("Não encontrei coluna de proxy (minutos) em: ", basename(path))
  if (is.na(ve_col))     stop("Não encontrei coluna de VE_ID em: ", basename(path))
  if (!season_col %in% names(raw)) stop("Não encontrei coluna 'season' em: ", basename(path))
  
  # limpar a string das datas (remover NBSP/esp. ocultos)
  x <- raw[[date_col]] %>%
    stringr::str_replace_all("\\u00A0", " ") %>%
    stringr::str_squish()
  
  # 1) parse explícito DD/MM/YYYY (seu formato)
  dt <- as.Date(x, format = "%d/%m/%Y")
  
  # 2) fallback: se tiver hora junto (ex.: 22/10/2020 03:15)
  na1 <- is.na(dt)
  if (any(na1)) {
    x2 <- sub(" .*", "", x[na1])                        # corta horário se houver
    dt[na1] <- as.Date(x2, format = "%d/%m/%Y")
  }
  
  # 3) fallback ISO/ymd
  na2 <- is.na(dt)
  if (any(na2)) dt[na2] <- suppressWarnings(lubridate::ymd(x[na2]))
  
  # 4) fallback serial do Excel
  na3 <- is.na(dt)
  if (any(na3)) {
    num <- suppressWarnings(as.numeric(x[na3]))
    is_num <- !is.na(num)
    if (any(is_num)) dt[na3][is_num] <- as.Date(num[is_num], origin = "1899-12-30")
  }
  
  # exemplos de datas que ainda falharam (se houver)
  na_final <- is.na(dt)
  if (any(na_final)) {
    msg <- paste(utils::head(unique(x[na_final]), 10), collapse = " | ")
    message(basename(path), " — exemplos de datas não parseadas: ", msg)
  }
  
  out <- tibble(
    season     = tolower(trimws(raw[[season_col]])),
    date       = dt,
    year       = lubridate::year(dt),
    julian_day = lubridate::yday(dt),
    VE_ID      = suppressWarnings(as.integer(readr::parse_number(raw[[ve_col]]))),
    proxy_min  = suppressWarnings(readr::parse_number(raw[[proxy_col]],
                                                      locale = readr::locale(decimal_mark = ".", grouping_mark = ",")))
  ) %>%
    filter(!is.na(date),
           season %in% c("fall","winter"),
           !is.na(year), year >= 1999, year <= 2023)
  
  # diagnóstico
  message(basename(path), ": mantidas ", nrow(out), " linhas (", sum(is.na(dt)), " datas inválidas removidas).")
  
  out
}

# ------ Ler fall e winter ------
pf <- read_proxy(proxy_fall)
pw <- read_proxy(proxy_winter)
proxy_all <- bind_rows(pf, pw)

# ------ Agregar por dia ------
daily <- proxy_all %>%
  group_by(season, year, julian_day) %>%
  summarise(
    vessels_present = n_distinct(VE_ID),
    proxy_minutes   = sum(proxy_min, na.rm = TRUE),
    .groups = "drop"
  )

# ------ Diagnóstico ------
diagnostico <- daily %>%
  summarise(
    max_vessels = max(vessels_present, na.rm = TRUE),
    q95_vessels = quantile(vessels_present, 0.95, na.rm = TRUE),
    max_proxy   = max(proxy_minutes, na.rm = TRUE),
    q95_proxy   = quantile(proxy_minutes[proxy_minutes > 0], 0.95, na.rm = TRUE),
    med_proxy   = median(proxy_minutes[proxy_minutes > 0], na.rm = TRUE)
  )
print(diagnostico)

# ------ Escalas e Plot ------
max_v     <- max(daily$vessels_present, na.rm = TRUE)
v_breaks  <- unique(sort(c(2,4,6,8,12,max_v)))

ps <- daily$proxy_minutes[daily$proxy_minutes > 0]
stopifnot(length(ps) > 0)
proxy_breaks <- pretty(ps, n = 5)
proxy_vals   <- as.numeric(quantile(ps, probs = c(.10,.50,.85,.99), na.rm = TRUE))

p <- ggplot(daily, aes(x = julian_day, y = as.factor(year))) +
  geom_point(
    data = subset(daily, proxy_minutes > 0),
    aes(size = proxy_minutes, fill = proxy_minutes),
    shape = 23, colour = "black", alpha = 0.75,
    position = position_jitter(width = 0.15, height = 0.08),
    show.legend = TRUE
  ) +
  scale_size_continuous(
    range  = c(3.5, 7),
    limits = c(min(ps, na.rm = TRUE), max(ps, na.rm = TRUE)),
    breaks = proxy_breaks,
    name   = "Colored diamonds size = Fishing-activity proxy (minutes)",
    guide  = guide_legend(order = 2, nrow = 1,
                          override.aes = list(shape = 23, fill = "grey70"))
  ) +
  scale_fill_gradientn(
    colours = c("yellow","orange","red","darkred"),
    values  = scales::rescale(proxy_vals),
    name    = "Colored diamonds fill = Proxy (minutes)",
    breaks  = proxy_breaks,
    labels  = scales::comma(proxy_breaks),
    guide   = guide_colourbar(order = 3, barwidth = 10, barheight = 0.3)
  ) +
  ggnewscale::new_scale("size") +
  geom_point(
    aes(size = vessels_present),
    shape = 23, fill = "black", colour = "black", alpha = 0.9,
    position = position_jitter(width = 0.15, height = 0.08),
    show.legend = TRUE
  ) +
  scale_size_continuous(
    range  = c(1, 3),
    limits = c(1, max_v),
    breaks = v_breaks,
    name   = "Black diamonds size = Vessels per day",
    guide  = guide_legend(order = 1, nrow = 1,
                          override.aes = list(shape = 23, fill = "black"))
  ) +
  facet_wrap(~season, ncol = 2, scales = "free_x") +
  labs(title = "Daily presence of vessels and fishing-activity proxy (minutes) by season",
       x = "Julian Day", y = "Year") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 7),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "center",
        legend.title = element_text(size = 9, face = "bold"),
        legend.text  = element_text(size = 7))

print(p)
# ggsave("diamonds_proxy_daily.png", p, width = 10, height = 7, dpi = 300)

#
#
#

# Usando agora mes e nao julian days #

####################################################
#                                                  #
# DIAMONDS (Proxy-only) — robusto + Year invertido #
#                                                  #
####################################################

library(dplyr)
library(ggplot2)
library(ggnewscale)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)
library(scales)

# ------ Caminhos ------
proxy_fall   <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_fall/proxy_fishing_activity_fall_timeweighted.csv"
proxy_winter <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_winter/proxy_fishing_activity_winter_timeweighted.csv"

`%nin%` <- Negate(`%in%`)

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
  readr::read_delim(
    path,
    delim = delim,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8"),
    col_types = cols(.default = col_character())
  )
}

# ------ Leitor robusto do proxy ------
read_proxy <- function(path) {
  raw <- read_any(path)
  names(raw) <- trimws(tolower(names(raw)))
  
  date_col   <- intersect(names(raw), c("date","data","datetime","timestamp","time"))[1]
  season_col <- "season"
  proxy_col  <- intersect(names(raw), c("minutes_covered","minutes_c","minutes","proxy","proxy_value","activity_proxy","proxy_min","proxy_minutes"))[1]
  ve_col     <- intersect(names(raw), c("ve_id","veid","vessel_id","vessel","vesselid"))[1]
  
  if (is.na(date_col))   stop("Não encontrei coluna de data em: ", basename(path))
  if (is.na(proxy_col))  stop("Não encontrei coluna de proxy (minutos) em: ", basename(path))
  if (is.na(ve_col))     stop("Não encontrei coluna de VE_ID em: ", basename(path))
  if (!season_col %in% names(raw)) stop("Não encontrei coluna 'season' em: ", basename(path))
  
  # ---- parse de date (robusto) ----
  x <- raw[[date_col]] %>%
    stringr::str_replace_all("\\u00A0", " ") %>%
    stringr::str_squish()
  
  dt <- as.Date(x, format = "%d/%m/%Y")
  na1 <- is.na(dt)
  if (any(na1)) {
    x2 <- sub(" .*", "", x[na1])  # remove tempo se existir
    dt[na1] <- as.Date(x2, format = "%d/%m/%Y")
  }
  na2 <- is.na(dt)
  if (any(na2)) dt[na2] <- suppressWarnings(lubridate::ymd(x[na2]))
  na3 <- is.na(dt)
  if (any(na3)) {
    num <- suppressWarnings(as.numeric(x[na3]))
    is_num <- !is.na(num)
    if (any(is_num)) dt[na3][is_num] <- as.Date(num[is_num], origin = "1899-12-30")
  }
  
  na_final <- is.na(dt)
  if (any(na_final)) {
    msg <- paste(utils::head(unique(x[na_final]), 10), collapse = " | ")
    message(basename(path), " — exemplos de datas não parseadas: ", msg)
  }
  
  # ---- parse proxy_min (ponto OU vírgula) ----
  proxy_num_dot <- readr::parse_number(
    raw[[proxy_col]],
    locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
  )
  proxy_num_comma <- readr::parse_number(
    raw[[proxy_col]],
    locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
  )
  proxy_min <- ifelse(is.na(proxy_num_dot), proxy_num_comma, proxy_num_dot)
  
  out <- tibble(
    season     = tolower(trimws(raw[[season_col]])),
    date       = dt,
    year       = lubridate::year(dt),
    julian_day = lubridate::yday(dt),
    VE_ID      = stringr::str_squish(as.character(raw[[ve_col]])), # NÃO força integer
    proxy_min  = proxy_min
  ) %>%
    filter(
      !is.na(date),
      season %in% c("fall","winter"),
      !is.na(year), year >= 1999, year <= 2023
    )
  
  message(basename(path), ": mantidas ", nrow(out), " linhas (",
          sum(na_final), " datas inválidas removidas).")
  out
}

# ------ Ler fall e winter ------
pf <- read_proxy(proxy_fall)
pw <- read_proxy(proxy_winter)

# renomear estação: fall -> Autumn; winter -> Winter
proxy_all <- bind_rows(pf, pw) %>%
  mutate(
    season = dplyr::recode(season, "fall" = "Autumn", "winter" = "Winter"),
    season = factor(season, levels = c("Autumn","Winter"))
  )

# ------ Agregar por DIA ------
daily <- proxy_all %>%
  group_by(season, year, date) %>%
  summarise(
    vessels_present = n_distinct(VE_ID),
    proxy_minutes   = sum(proxy_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    month_num = lubridate::month(date),                          # 1..12
    month_lab = factor(month.abb[month_num], levels = month.abb) # "Jan".."Dec"
  )

# ------ Diagnóstico ------
ps <- daily$proxy_minutes[daily$proxy_minutes > 0]
stopifnot(length(ps) > 0)

diagnostico <- daily %>%
  summarise(
    max_vessels = max(vessels_present, na.rm = TRUE),
    q95_vessels = quantile(vessels_present, 0.95, na.rm = TRUE),
    max_proxy   = max(proxy_minutes, na.rm = TRUE),
    q95_proxy   = quantile(proxy_minutes[proxy_minutes > 0], 0.95, na.rm = TRUE),
    med_proxy   = median(proxy_minutes[proxy_minutes > 0], na.rm = TRUE)
  )
print(diagnostico)

# ------ Escalas ------
max_v        <- max(daily$vessels_present, na.rm = TRUE)
v_breaks     <- unique(sort(c(2,4,6,8,12,max_v)))
proxy_breaks <- pretty(ps, n = 5)
proxy_vals   <- as.numeric(quantile(ps, probs = c(.10,.50,.85,.99), na.rm = TRUE))

# ------ Year invertido ------
daily <- daily %>%
  mutate(year_f = factor(year, levels = rev(sort(unique(year)))))

# ------ Plot ------
p <- ggplot(daily, aes(x = month_lab, y = year_f)) +
  geom_point(
    data = subset(daily, proxy_minutes > 0),
    aes(size = proxy_minutes, fill = proxy_minutes),
    shape = 23, colour = "black", alpha = 0.75,
    position = position_jitter(width = 0.25, height = 0.08),
    show.legend = TRUE
  ) +
  scale_size_continuous(
    range  = c(3.5, 7),
    limits = c(min(ps, na.rm = TRUE), max(ps, na.rm = TRUE)),
    breaks = proxy_breaks,
    name   = "Colored diamonds size = Fishing-activity proxy (minutes)",
    guide  = guide_legend(order = 2, nrow = 1,
                          override.aes = list(shape = 23, fill = "grey70"))
  ) +
  scale_fill_gradientn(
    colours = c("yellow","orange","red","darkred"),
    values  = scales::rescale(proxy_vals),
    name    = "Colored diamonds fill = Proxy (minutes)",
    breaks  = proxy_breaks,
    labels  = scales::comma(proxy_breaks),
    guide   = guide_colourbar(order = 3, barwidth = 10, barheight = 0.3)
  ) +
  ggnewscale::new_scale("size") +
  geom_point(
    aes(size = vessels_present),
    shape = 23, fill = "black", colour = "black", alpha = 0.9,
    position = position_jitter(width = 0.25, height = 0.08),
    show.legend = TRUE
  ) +
  scale_size_continuous(
    range  = c(1, 3),
    limits = c(1, max_v),
    breaks = v_breaks,
    name   = "Black diamonds size = Vessels per day",
    guide  = guide_legend(order = 1, nrow = 1,
                          override.aes = list(shape = 23, fill = "black"))
  ) +
  facet_wrap(~season, ncol = 2, scales = "free_x") +
  scale_x_discrete(drop = TRUE) +
  labs(
    title = "Daily presence of vessels and fishing-activity proxy by season",
    x = "Month", y = "Year"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "center",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text  = element_text(size = 7)
  )

print(p)

# Se quiser salvar:
ggsave("diamonds_proxy_daily_by_month_faceted.png", p, width = 10, height = 7, dpi = 300)










