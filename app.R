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
      #menuItem(icon = icon("calendar"),"Trigger Analysis", tabName = "DMA"),
      #menuItem(icon = icon("camera"),"Photo Position Finder", tabName = "Photo"),
      #menuItem(icon = icon("ship"),"SLOW zone viewer", tabName = "szone"),
      #menuItem(icon = icon("question-circle"),text = "Instructions Wiki", href = "https://github.com/leahcrowe/narwss_rwsas_apps/wiki")
    )                                     
  ),
  dashboardBody(
  tabItem(tabName = "PAmerge",
          source('./scripts/PAmerge_app.R', local = TRUE)$value
          )
  
  ))

server = function(input, output, session){}

shinyApp(ui, server)