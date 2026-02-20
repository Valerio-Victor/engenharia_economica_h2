# PACOTES: ----------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)


# IMPORTAÇÃO: -------------------------------------------------------------
lcoh_verde <- readr::read_csv(file = 'lcoh.csv') %>% 
  janitor::clean_names() %>% 
  dplyr::filter(units == '£/kg H2') %>% 
  dplyr::transmute(tec = ifelse(tech == 'Alkaline', 'ALK', 'PEM'),
                   cenario = dplyr::case_when(
                     scenario == 'Low' ~ 'Otimista',
                     scenario == 'Central' ~ 'Provável',
                     scenario == 'High' ~ 'Pessimista'),
                   ano = build_year,
                   fator_capacidade = capacity_factor*100,
                   preco_eletricidade = electricity_price/1000*7.03,
                   tma = discount_factor*100,
                   lcoh = lcoh*7.03,
                   componente_capex = lcoh_capex*7.03,
                   componente_eletricidade = lcoh_electricity_cost*7.03,
                   componente_opex_fixo = lcoh_fixed_opex*7.03,
                   componente_opex_variavel = lcoh_variable_opex*7.03) %>% 
  dplyr::mutate(componente_capex = componente_capex/lcoh*100,
                componente_eletricidade = componente_eletricidade/lcoh*100,
                componente_opex_fixo = componente_opex_fixo/lcoh*100,
                componente_opex_variavel = componente_opex_variavel/lcoh*100)

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
skin = 'green',

dashboardHeader(title = 'Calculadora LCOH'),
  
dashboardSidebar(
  sidebarMenu(
    menuItem('Hidrogênio Verde', tabName = 'pag1', icon = icon('calculator')),
    menuItem('Hidrogênio Azul', tabName = 'pag2', icon = icon('calculator'))
  )
),

dashboardBody(
  tabItems(
    tabItem(tabName = 'pag1',
            fluidRow(
              box(title = 'CONFIGURAÇÃO  DO CENÁRIO', 
                  status = 'success', 
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = 4,
                  column(width = 12,
                         fluidRow(selectInput(inputId = 'cenario_verde',
                                              label = 'Cenário de Análise',
                                              choices = unique(lcoh_verde$cenario),
                                              selected = 'Provável')),
                         fluidRow(sliderInput(inputId = 'fator_capacidade_verde',
                                              label = 'Fator de Capacidade (%)',
                                              min = min(lcoh_verde$fator_capacidade),
                                              max = max(lcoh_verde$fator_capacidade),
                                              value = 85,
                                              step = 5,
                                              sep = ',',
                                              post = '%')),
                         fluidRow(sliderInput(inputId = 'eletricidade_verde',
                                              label = 'Preço da Eletricidade (R$/kWh)',
                                              min = min(lcoh_verde$preco_eletricidade),
                                              max = max(lcoh_verde$preco_eletricidade),
                                              step = 0.03515,
                                              value = 0.91390,
                                              sep = ',',
                                              pre = 'R$ ')),
                         fluidRow(sliderInput(inputId = 'tma_verde',
                                              label = 'Taxa Mínima de Atratividade (TMA)',
                                              min = min(lcoh_verde$tma),
                                              max = max(lcoh_verde$tma),
                                              value = max(lcoh_verde$tma),
                                              sep = ',',
                                              post = '%')),
                         fluidRow(checkboxGroupInput(inputId = 'tecnologia_verde',
                                                     label = 'Tecnologias de Eletrólise',
                                                     choices = unique(lcoh_verde$tec),
                                                     selected = unique(lcoh_verde$tec))),
                         fluidRow(actionButton(inputId = 'calcular_lcoh_verde',
                                               label = 'Calcular LCOH',
                                               icon = icon('calculator'))))),
              
              box(title = 'PREVISÃO DE LCOH', 
                  status = 'success', 
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = 8,
                  column(width = 12,
                  fluidRow(plotlyOutput(outputId = 'graf_lcoh_verde'))))
              
)
)
),
tags$head(
  tags$script(HTML("
      $(document).on('shiny:connected', function() {
        $('body').addClass('sidebar-collapse');
        $('.sidebar-toggle').off();
      });
    ")),
  tags$style(HTML("
      .content-wrapper, .right-side, .main-footer {
        margin-left: 0 !important;
      }
    ")),
  tags$style(HTML("

    /* Barra preenchida */
    .irs-bar {
      background: #00a65a !important;
      border-top: 1px solid #00a65a !important;
      border-bottom: 1px solid #00a65a !important;
    }

    /* Botão (handle) */
    .irs-handle {
      border: 2px solid #00a65a !important;
      background: #ffffff !important;
    }

    /* Linha da barra */
    .irs-line {
      background: #00a65a !important;
    }

    /* Número exibido no tooltip */
    .irs-single {
      background: #00a65a !important;
    }

  ")),
  tags$style(HTML("

  /* Cor do checkbox quando marcado */
  input[type='checkbox']:checked {
    accent-color: #00a65a;
  }

")),
  tags$style(HTML("

/* Borda do select fechado */
.selectize-control.single .selectize-input {
  border-color: #00a65a !important;
}

/* Borda quando clica */
.selectize-control.single .selectize-input.focus {
  border-color: #00a65a !important;
  box-shadow: 0 0 5px rgba(0,166,90,0.5) !important;
}

/* Item selecionado dentro do campo */
.selectize-control.single .selectize-input > div {
  background: #00a65a !important;
  color: white !important;
}

/* Opção ao passar o mouse */
.selectize-dropdown .option:hover {
  background-color: #00a65a !important;
  color: white !important;
}

/* Opção selecionada */
.selectize-dropdown .active {
  background-color: #00a65a !important;
  color: white !important;
}

"))
)
)
)



# SERVER ------------------------------------------------------------------
server <- function(input, output) { 
  observeEvent(input$calcular_lcoh_verde, {
    
    showNotification("LCOH calculado para o cenário!", type = "message")
    
    graf <- lcoh_verde %>% 
      dplyr::filter(cenario == input$cenario_verde) %>% 
      dplyr::filter(fator_capacidade == input$fator_capacidade_verde) %>% 
      dplyr::filter(preco_eletricidade == input$eletricidade_verde) %>% 
      dplyr::filter(tma == input$tma_verde) %>% 
      dplyr::filter(tec %in% input$tecnologia_verde) %>% 
      ggplot() + 
      geom_line(aes(x = ano , y = lcoh, color = tec, group = 1,
                    text = paste0(
                      'LCOH: ', scales::number(lcoh, 
                                               accuracy = 0.01, 
                                               decimal.mark = ',',
                                               suffix = ' R$/kg H₂'),
                      '<br>Proporção do CAPEX: ', scales::number(componente_capex, 
                                                                 accuracy = 0.01, 
                                                                 decimal.mark = ',',
                                                                 suffix = ' %'),
                      '<br>Proporção do Eletricidade: ', scales::number(componente_eletricidade, 
                                                                        accuracy = 0.01, 
                                                                        decimal.mark = ',',
                                                                        suffix = ' %'),
                      '<br>Proporção do OPEX Fixo: ', scales::number(componente_opex_fixo, 
                                                                     accuracy = 0.01, 
                                                                     decimal.mark = ',',
                                                                     suffix = ' %'),
                      '<br>Proporção do OPEX Variável: ', scales::number(componente_opex_variavel, 
                                                                         accuracy = 0.01, 
                                                                         decimal.mark = ',',
                                                                         suffix = ' %')
                    ))) + 
      geom_point(aes(x = ano , y = lcoh, color = tec, group = 1,
                     text = paste0(
                       'LCOH: ', scales::number(lcoh, 
                                                accuracy = 0.01, 
                                                decimal.mark = ',',
                                                suffix = ' R$/kg H₂'),
                       '<br>Proporção do CAPEX: ', scales::number(componente_capex, 
                                                                  accuracy = 0.01, 
                                                                  decimal.mark = ',',
                                                                  suffix = ' %'),
                       '<br>Proporção do Eletricidade: ', scales::number(componente_eletricidade, 
                                                                         accuracy = 0.01, 
                                                                         decimal.mark = ',',
                                                                         suffix = ' %'),
                       '<br>Proporção do OPEX Fixo: ', scales::number(componente_opex_fixo, 
                                                                      accuracy = 0.01, 
                                                                      decimal.mark = ',',
                                                                      suffix = ' %'),
                       '<br>Proporção do OPEX Variável: ', scales::number(componente_opex_variavel, 
                                                                          accuracy = 0.01, 
                                                                          decimal.mark = ',',
                                                                          suffix = ' %')
                     ))) +
      labs(title = '',
           x = 'Anos',
           y = 'Estimativa de LCOH (R$/kg H₂)',
           color = '') + 
      ggplot2::scale_color_manual(values = c('PEM' = '#2e7d32', 
                                             'ALK' = '#3e4a3d')) + 
      ggplot2::theme_light()
    
    output$graf_lcoh_verde <- renderPlotly({
      ggplotly(graf, tooltip = 'text')
    })
  })
  
}

# APP ---------------------------------------------------------------------
shinyApp(ui, server)



