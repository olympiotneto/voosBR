library(shiny)
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table" , tabname = "my_table", icon = icon("table"),
               startExpanded = TRUE,
               menuSubItem("sub menu",
                           tabName = "subMenu"),
               menuSubItem("sub menu2",
                          tabName = "subMenu2")
               ),
      menuItem("Next Widget", tabName = "Other"))),

  dashboardBody(
    tabItems(
      tabItem(tabName = "subMenu", #my_table",
              h2("First tab")
      ),
      tabItem(tabName = "subMenu2", #my_table",
              h2("2nd tab")
      ),
      tabItem(tabName = "Other",
              h2("Other tab")
      )
    )))
server <- function(session, input, output) {

}
shinyApp(ui, server)
