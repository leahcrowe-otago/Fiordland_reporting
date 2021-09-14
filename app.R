########################
## App file for Fiordland data processing and reporting
########################

## GLOBAL

source('./scripts/global_libraries.R', local = TRUE)$value

## USER INTERFACE

ui <- dashboardPage(
  dashboardHeader(title = "FBD"),
  #sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem(icon = icon("camera"),"PA merge", tabName = "PAmerge")
    )                                     
  ),
  dashboardBody(
  tabItem(tabName = "PAmerge",
          source('./scripts/PAmerge_app.R', local = TRUE)$value
          )
  
  ))

server = function(input, output, session){}

shinyApp(ui, server)