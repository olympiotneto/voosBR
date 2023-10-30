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
        fluidRow(
          #Caixinha para por o gráfico
          bs4Card(
            width = 12,
            height = 642,
            title = "Número de voos domésticos e internacionais",
            plotOutput("vg_serie_voos") |>
              #indicador de carregamento
              shinycssloaders::withSpinner() ,
            collapsible = FALSE
          )
        ),
        fluidRow(
          #cria abas dentro de um box
          bs4TabCard(
            width = 6,
            height = 642,
            title = "Aeroportos com mais voos",
            side = "right",
            collapsible = FALSE,
            tabPanel(
              title = "Partidas",
              tableOutput("vg_tab_aero_partidas")
            ),
            tabPanel(
              title = "Chegadas",
              tableOutput("vg_tab_aero_chegadas")
            )
          ),
          #cria abas dentro de um box
          bs4Card(
            width = 6,
            height = 662,
            title = "Empresas com mais voos",
            side = "right",
            collapsible = FALSE,
            tableOutput("vg_tab_emp")

          )
        )

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
  tab_empresas <- tbl(con,"tab_empresas")

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

      output$vg_tab_aero_partidas <- renderTable({
        tab_voos |>
          count(origin_airport, sort = TRUE) |>
          head(10) |>
          left_join(
            tab_aeroportos,
            by = c("origin_airport" = "airport_cod")
          ) |>
          collect() |>
          mutate(
            n = formatar_numero(n)
            ) |>
          select(
            Aeroporto = airport_name,
            Cidade = city,
            `Número de voos` = n
          )


      })

      output$vg_tab_aero_chegadas <- renderTable({
        tab_voos |>
          count(destination_airport, sort = TRUE) |>
          head(10) |>
          left_join(
            tab_aeroportos,
            by = c("destination_airport" = "airport_cod")
          ) |>
          collect() |>
          mutate(
            n = formatar_numero(n)
          ) |>
          select(
            Aeroporto = airport_name,
            Cidade = city,
            `Número de voos` = n
          )


      })

      output$vg_tab_emp <- renderTable({
        tab_voos |>
          count(airline, sort = TRUE) |>
          head(10) |>
          left_join(
            tab_empresas,
            by = c("airline" = "airline_cod")
          ) |>
          collect() |>
          mutate(
            n = formatar_numero(n)
          ) |>
          select(
            `Empresa aérea` = airline_name,
            `Número de voos` = n
          )


      })

 }

shinyApp(ui, server)
