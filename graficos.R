# PACOTES: ----------------------------------------------------------------
library(magrittr, include.only = '%>%')
library(ggplot2)
library(plotly)
library(crosstalk)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(htmltools)


# IMPORTAÇÃO E ORANIZAÇÃO DOS DADOS: --------------------------------------
lcoh <- readr::read_csv(file = 'lcoh.csv') %>% 
  janitor::clean_names() %>% 
  dplyr::filter(units == '£/kg H2') %>% 
  dplyr::transmute(tec = ifelse(tech == 'Alkaline', 'ALK', 'PEM'),
                   cenario = dplyr::case_when(
                     scenario == 'Low' ~ 'Otimista',
                     scenario == 'Central' ~ 'Provável',
                     scenario == 'High' ~ 'Pessimista'),
                   ano = build_year,
                   fator_capacidade = capacity_factor,
                   preco_eletricidade = electricity_price/1000*7.03,
                   tma = discount_factor,
                   lcoh = lcoh*7.03,
                   componente_capex = lcoh_capex*7.03,
                   componente_eletricidade = lcoh_electricity_cost*7.03,
                   componente_opex_fixo = lcoh_fixed_opex*7.03,
                   componente_opex_variavel = lcoh_variable_opex*7.03) %>% 
  dplyr::mutate(componente_capex = componente_capex/lcoh,
                componente_eletricidade = componente_eletricidade/lcoh,
                componente_opex_fixo = componente_opex_fixo/lcoh,
                componente_opex_variavel = componente_opex_variavel/lcoh)

# supondo que seu data.frame se chame df
df <- lcoh 

# 1) Preparação dos dados (tipos e rótulos)
df_plot <- df %>%
  mutate(
    tec = factor(tec, levels = c("ALK", "PEM")),
    cenario = factor(cenario, levels = c("Provável", "Otimista", "Pessimista")),
    ano = as.integer(ano),
    fator_capacidade = as.numeric(fator_capacidade),
    tma = as.numeric(tma),
    preco_eletricidade = as.numeric(preco_eletricidade)) %>% 
  dplyr::mutate(
    fator_capacidade_nome = scales::percent(fator_capacidade,
                                            decimal.mark = ',',
                                            accuracy = 0.1),
    tma_nome = scales::percent(tma,
                               decimal.mark = ',',
                               accuracy = 0.1),
    preco_eletricidade_nome = scales::number(preco_eletricidade,
                                        decimal.mark = ',',
                                        accuracy = 0.00001),
  )

# 2) SharedData (crosstalk)
sd <- SharedData$new(df_plot, key = ~paste0(tec, "-", cenario, "-", ano, "-", fator_capacidade, "-", tma, "-", preco_eletricidade),
                     group = "lcoh")

# 3) Filtros (botões/dropdowns)
# - filter_select cria um seletor (dropdown). Para "botão" visual, depois dá pra estilizar.
f_cenario <- filter_select(
  id = "cenario",
  label = "Cenário",
  sharedData = sd,
  group = ~cenario,
  multiple = FALSE
)

f_fc <- filter_select(
  id = "fc",
  label = "Fator de capacidade (% de Utilização)",
  sharedData = sd,
  group = ~fator_capacidade_nome,
  multiple = FALSE
)

f_tma <- filter_select(
  id = "tma",
  label = "TMA (% ao ano)",
  sharedData = sd,
  group = ~tma_nome,
  multiple = FALSE
)

f_preco <- filter_select(
  id = "preco",
  label = "Preço da Eletricidade (R$/kWh)",
  sharedData = sd,
  group = ~preco_eletricidade_nome,
  multiple = FALSE
)

# 4) Gráfico (ggplot -> ggplotly)
p <- ggplot(sd, aes(x = ano, y = lcoh, color = tec, group = tec)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 0.9) +
  labs(
    x = "Ano",
    y = "LCOH Verde (R$/kg)",
    color = ""
  ) +
  theme_minimal()

p_int <- ggplotly(p, tooltip = c("x", "y", "colour"))

# 5) Layout final (filtros em cima + gráfico)
# (Se quiser em colunas, posso te passar uma versão com htmltools::div e CSS.)
browsable(
  tagList(
    tags$div(
      style = "display:flex; gap:16px; flex-wrap:wrap; margin-bottom:12px;",
      tags$div(style="min-width:220px;", f_cenario),
      tags$div(style="min-width:220px;", f_fc),
      tags$div(style="min-width:220px;", f_tma),
      tags$div(style="min-width:220px;", f_preco)
    ),
    p_int
  )
)
