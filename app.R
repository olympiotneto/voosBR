library(shiny)
library(tidyverse)
library(bs4Dash)



####Versão bs4Dash

ui <- bs4DashPage(
  dark = NULL,
  help = NULL,
  header = bs4DashNavbar(
    title = "Voos BR"
  ),
  sidebar = bs4DashSidebar(
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        text = "Visão Geral",
        tabName = "visao_geral",
        icon = icon("eye")

      ),
      bs4SidebarMenuItem(
        text = "Partidas"

      ),
      bs4SidebarMenuItem(
        text = "Chegadas"

      ),
      bs4SidebarMenuItem(
        text = "Aeroportos"
      ),
      bs4SidebarMenuItem(
        text = "Companhias aéreas"
      ),
      bs4SidebarMenuItem(
        text = "Localidade"
      ),
      bs4SidebarMenuItem(
        text = "Sobre"
      )
    )
  ),
  body = bs4DashBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "custom.css"
      )
    ),
    bs4TabItems(
      bs4TabItem(
        tabName = "visao_geral",
        titlePanel("Visão Geral"),
        hr(),
        #valuebox sempre vai dentro de uma fluidRow
        fluidRow(
          bs4ValueBoxOutput(
            "vg_num_voos",
            width = 4
            ),
          bs4ValueBoxOutput(
            "vg_num_voos_domesticos",
            width = 4
          ),
          bs4ValueBoxOutput(
            "vg_num_voos_inter",
            width = 4
          )
        ),
        plotOutput("vg_serie_voos")
        # fluidRow(
        #   column(
        #     width = 4
        #     # nr de voos total
        #     # nr de chegadas
        #     # nr saídas
        #     # nr empreas aereas que atuaram no período
        #     # nr de aeroportos com voos no período
        #   ),
        #   column(
        #     width = 8,
        #     plotOutput("vg_serie_historica_partidas"),
        #     plotOutput("vg_serie_historica_chegadas")
        #   )
        # )
      )
    )
  )
)



server <- function(input, output, session) {

  con <- RSQLite::dbConnect(
    RSQLite::SQLite(),
    "brflights.sqlite"
  )

  tab_voos <- tbl(con, "tab_voos")
  tab_aeroportos <- tbl(con, "tab_aeroportos")

  output$vg_num_voos <- renderbs4ValueBox({

    valor <- tab_voos |>
      contar_linhas() |>
      formatar_numero()

    bs4ValueBox(
      value = valor ,
      subtitle = "Total de voos" ,
      icon = icon("plane") ,
      color = "lightblue"
    )

  })

    output$vg_num_voos_domesticos <- renderbs4ValueBox({

      valor <- tab_voos |>
        filter(
          flight_type == "N"
        ) |>
        contar_linhas() |>
        formatar_numero()

      bs4ValueBox(
        value = valor,
        subtitle = "Número de voos domésticos" ,
        icon = icon("house") ,
        color = "lightblue"
      )


  })

    output$vg_num_voos_inter <- renderbs4ValueBox({

      valor <- tab_voos |>
        filter(
          flight_type == "I"
        ) |>
        contar_linhas() |>
        formatar_numero()

      bs4ValueBox(
        value = valor ,
        subtitle = "Número de voos internacionais" ,
        icon = icon("globe") ,
        color = "lightblue"
      )


    })

      output$vg_serie_voos <- renderPlot({

        tab_voos |>
          count(flight_type,planned_departure_date_ym) |>
          filter(
            !is.na(planned_departure_date_ym)
          ) |>
          collect() |>
          mutate(
            data = as.Date(planned_departure_date_ym)
          ) |>
          ggplot(
            aes(x = data, y = n, color = flight_type)
          )+
          geom_line()+
          geom_point(size = 3, shape = 21)+
          theme_classic()
      })

  # output$vg_serie_historica_partidas <- renderPlot({
  #
  #   tab_voos |>
  #     filter(
  #       # quando usa db para acessar dados, precisa do operador !! antes dos inputs
  #
  #       actual_departure_date >= !!input$vg_periodo[1],
  #       actual_departure_date <= !!input$vg_periodo[2]
  #     ) |>
  #     count(planned_departure_date_ym) |>
  #     collect() |>
  #     mutate(
  #       planned_departure_date_ym = as.Date(planned_departure_date_ym)
  #     ) |>
  #     ggplot(aes(x = planned_departure_date_ym, y = n)) +
  #     geom_line(color="royalblue") +
  #     geom_point(color="royalblue")+
  #     theme_minimal()
  # })
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
