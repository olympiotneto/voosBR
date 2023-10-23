library(shiny)
library(tidyverse)



 # dados <- readRDS("data/brflights.rds")


ui <- navbarPage(
  title = "Voos BR",
  tabPanel(
    title =  "Visão geral",
    titlePanel("Visão geral"),
    hr(),

    # gráfico de num voos por dia
      dateRangeInput(
        inputId = "vg_periodo",
        label = "Selecione um período",
        start = "2019-01-01",
        end = "2023-07-31",
        min = "2019-01-01",
        max = "2023-07-31",
        language = "pt-BR",
        format = "dd/mm/yyyy",
        separator = "a"
      ),

      fluidPage(
        column(
          width = 4,
          # nr de voos total
          # nr de chegadas
          # nr saídas
          # nr empreas aereas que atuaram no período
          # nr de aeroportos com voos no período
        ),
        column(
          width = 8,
          plotOutput("vg_serie_historica_partidas"),
          plotOutput("vg_serie_historica_chegadas")
        )
      )

    ),

  tabPanel(
    title = "Partidas"
  ),

  tabPanel(
    title = "Chegadas"
  ),
  tabPanel(
    title = "Aeroportos"
  ),

  tabPanel(
    title = "Companhias aéreas"
  ),

  tabPanel(
    title = "Localidade"
  ),

  tabPanel(
    title  = "Sobre"
  )

)





# ui <- fluidPage(
#   titlePanel("Painel de voos do Brasil"),
#   hr(),
#   selectInput(
#     inputId = "ano",
#     label = "Selecione um ano",
#     # choices = unique(c(
#     #   dados$planned_departure_date_year,
#     #   dados$actual_departure_date_year
#     #   )
#     # )
#     choices = 2019:2023
#   ),
#   plotOutput("numero_voos_saem_mes"),
#   hr(),
#   dateRangeInput(
#     inputId = "periodo",
#     label = "Selecione um período",
#     start = "2023-01-01",
#     end = "2023-01-01",
#     min = "2019-01-01",
#     max = "2023-07-31",
#     language = "pt-BR",
#     format = "dd/mm/yyyy",
#     separator = "a"
#   ),
#   tableOutput("tabela_voos")
#
# )

server <- function(input, output, session) {

  con <- RSQLite::dbConnect(
    RSQLite::SQLite(),
    "brflights.sqlite"
  )

  tab_voos <- tbl(con, "tab_voos")

  output$vg_serie_historica_partidas <- renderPlot({

    tab_voos |>
      filter(
        # quando usa db para acessar dados, precisa do operador !! antes dos inputs

        actual_departure_date >= !!input$vg_periodo[1],
        actual_departure_date <= !!input$vg_periodo[2]
      ) |>
      count(planned_departure_date_ym) |>
      collect() |>
      mutate(
        planned_departure_date_ym = as.Date(planned_departure_date_ym)
      ) |>
      ggplot(aes(x = planned_departure_date_ym, y = n)) +
      geom_line(color="royalblue") +
      geom_point(color="royalblue")+
      theme_minimal()
  })
#
#
# output$tabela_voos <- renderTable({
#
#   tab_voos |>
#     filter(
#       actual_departure_date >= !!input$periodo[1],
#       actual_departure_date <= !!input$periodo[2]
#     ) |>
#     head(20) |>
#     select(
#       airline,
#       flight_number,
#       origin_airport,
#       actual_departure_date,
#       actual_arrival_date,
#     ) |>
#     collect()
#
# })
#
 }

shinyApp(ui, server)
