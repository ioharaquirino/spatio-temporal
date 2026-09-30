
# GINI INDEX SEASONS #

# FALL #

# 📦 Pacotes
library(dplyr)
library(lubridate)
library(ggplot2)
library(ineq)

# 📁 Diretório (exemplo: FALL)
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/dataset_final/proxy_grid_fall")

# 📥 Carregar proxy (FALL)
proxy <- read.csv("proxy_fishing_activity_fall.csv") %>%
  mutate(date = as.Date(date),
         year = year(date)) %>%
  filter(year >= 1999 & year <= 2023)

# 🔢 Número de dias de presença por célula por ano
presenca_por_celula <- proxy %>%
  group_by(year, grid_id) %>%
  summarise(n_dias = n_distinct(date), .groups = "drop")

# 📊 Gini simples por ano
gini_por_ano <- presenca_por_celula %>%
  group_by(year) %>%
  summarise(
    gini_index = round(Gini(n_dias), 4),
    n_cells = n(),
    .groups = "drop"
  )

# 💾 Exportar tabela
write.csv(gini_por_ano, "GINI_index_by_year_fall.csv", row.names = FALSE)

# 📈 Plot evolução do Gini
grafico_gini <- ggplot(gini_por_ano, aes(x = year, y = gini_index)) +
  geom_line(color = "#d95f02", size = 1) +
  geom_point(color = "#d95f02", size = 2) +
  geom_smooth(method = "loess", color = "#1b9e77",
              fill = "#1b9e77", alpha = 0.3, se = TRUE) +
  labs(
    title = "Gini Index over Time - FALL (1999–2023)",
    x = "Year", y = "Gini Index"
  ) +
  theme_minimal(base_size = 14)

# 💾 Salvar gráfico
ggsave("GINI_index_over_time_fall.png", grafico_gini, width = 8, height = 6, dpi = 300)

cat("✅ Gini index para FALL calculado e salvo!\n",
    "- Tabela: GINI_index_by_year_fall.csv\n",
    "- Gráfico: GINI_index_over_time_fall.png\n")


#
#
#

# WINTER #

# 📦 Pacotes
library(dplyr)
library(lubridate)
library(ggplot2)
library(ineq)

# 📁 Diretório (exemplo: WINTER)
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/dataset_final/proxy_grid_winter")

# 📥 Carregar proxy (WINTER)
proxy <- read.csv("proxy_fishing_activity_winter.csv") %>%
  mutate(date = as.Date(date),
         year = year(date)) %>%
  filter(year >= 1999 & year <= 2023)

# 🔢 Número de dias de presença por célula por ano
presenca_por_celula <- proxy %>%
  group_by(year, grid_id) %>%
  summarise(n_dias = n_distinct(date), .groups = "drop")

# 📊 Gini simples por ano
gini_por_ano <- presenca_por_celula %>%
  group_by(year) %>%
  summarise(
    gini_index = round(Gini(n_dias), 4),
    n_cells = n(),
    .groups = "drop"
  )

# 💾 Exportar tabela
write.csv(gini_por_ano, "GINI_index_by_year_winter.csv", row.names = FALSE)

# 📈 Plot evolução do Gini
grafico_gini <- ggplot(gini_por_ano, aes(x = year, y = gini_index)) +
  geom_line(color = "#d95f02", size = 1) +
  geom_point(color = "#d95f02", size = 2) +
  geom_smooth(method = "loess", color = "#1b9e77",
              fill = "#1b9e77", alpha = 0.3, se = TRUE) +
  labs(
    title = "Gini Index over Time - WINTER (1999–2023)",
    x = "Year", y = "Gini Index"
  ) +
  theme_minimal(base_size = 14)

# 💾 Salvar gráfico
ggsave("GINI_index_over_time_winter.png", grafico_gini, width = 8, height = 6, dpi = 300)

cat("✅ Gini index para WINTER calculado e salvo!\n",
    "- Tabela: GINI_index_by_year_winter.csv\n",
    "- Gráfico: GINI_index_over_time_winter.png\n")


#
#
#

# Agora GINI com as cores corretas e salvando junto as seasons e com smothie #

# ===============================
# Gini Index — Autumn vs Winter (com smooth)
# ===============================

library(dplyr)
library(lubridate)
library(ggplot2)
library(ineq)
library(patchwork)
library(scales)

# Entradas
fall_path   <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_fall/proxy_fishing_activity_fall_timeweighted.csv"
winter_path <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/all_datasets/proxy_grid_winter/proxy_fishing_activity_winter_timeweighted.csv"

# Saída (Desktop)
out_dir <- "C:/Users/SH01IQ/OneDrive - UHI/Desktop"

# Cores
col_fall   <- "#d95f02"  # laranja escuro
col_winter <- "#1b4f72"  # azul escuro

# -------------------------
# Função: Gini por ano
# -------------------------
compute_gini <- function(csv_path, season_label){
  df <- read.csv(csv_path) %>%
    mutate(date = as.Date(date),
           year = year(date)) %>%
    filter(year >= 1999, year <= 2023)
  
  # dias de presença por célula e ano
  presenca_por_celula <- df %>%
    group_by(year, grid_id) %>%
    summarise(n_dias = n_distinct(date), .groups = "drop")
  
  # se algum ano ficar sem célula, garantir linha com NA/0
  years_all <- data.frame(year = 1999:2023)
  gini_por_ano <- presenca_por_celula %>%
    group_by(year) %>%
    summarise(
      gini_index = if (n() > 0) round(Gini(n_dias), 4) else NA_real_,
      n_cells    = n(),
      .groups    = "drop"
    ) %>%
    right_join(years_all, by = "year") %>%
    arrange(year) %>%
    mutate(
      season  = season_label,
      n_cells = ifelse(is.na(n_cells), 0L, n_cells)
    )
  
  gini_por_ano
}

# Rodar
autumn_year <- compute_gini(fall_path,   "Autumn")
winter_year <- compute_gini(winter_path, "Winter")

# CSVs no Desktop (mantidos como FALL/WINTER)
write.csv(autumn_year, file.path(out_dir, "GINI_index_by_year_FALL.csv"),   row.names = FALSE)
write.csv(winter_year, file.path(out_dir, "GINI_index_by_year_WINTER.csv"), row.names = FALSE)

# Combinar para limites comuns
all_years <- bind_rows(autumn_year, winter_year)

# Eixos
x_breaks <- seq(1999, 2023, by = 2)
x_limits <- c(1999, 2023)

yrange <- function(v){
  v <- v[is.finite(v)]
  if (length(v) == 0) return(c(0,1))
  rng <- range(v, na.rm = TRUE)
  pad <- 0.05 * diff(rng)
  if (pad == 0) pad <- 0.05
  c(max(0, rng[1]-pad), min(1, rng[2]+pad))  # Gini ∈ [0,1]
}
ylim_gini <- yrange(all_years$gini_index)

theme_base <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

# função com smoothie
p_metric <- function(df, col, title_str){
  ggplot(df, aes(x = year, y = gini_index)) +
    geom_line(color = col, linewidth = 1) +
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
    coord_cartesian(ylim = ylim_gini) +
    labs(x = "Year", y = "Gini Index") +
    ggtitle(title_str) +
    theme_base
}

p_autumn <- p_metric(autumn_year, col_fall,   "Autumn")
p_winter <- p_metric(winter_year, col_winter, "Winter")

final_plot <- p_autumn + p_winter + plot_layout(ncol = 2) &
  theme(legend.position = "none")

# Salvar no Desktop
outfile_png <- file.path(out_dir, "GINI_Autumn_vs_Winter_smooth.png")
outfile_pdf <- file.path(out_dir, "GINI_Autumn_vs_Winter_smooth.pdf")
ggsave(outfile_png, final_plot, width = 12, height = 6, dpi = 300)
ggsave(outfile_pdf, final_plot, width = 12, height = 6)



