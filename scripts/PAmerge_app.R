#####################################################
## R Shiny Application for merging PA spreadsheets ##
##    for photo processing of FBD data             ##  
##         Written By: Leah Crowe 2021             ##
#####################################################

####################
## User interface ##
####################

ui = source('./scripts/PAmerge_ui.R', local = TRUE)$value
############
## Server ##
############

## Define server logic 
server = function(input, output, session) {
  
  source('./scripts/PAmerge_server.R', local = TRUE)$value
  
}

#########################
## Create Shiny object ##
#########################

shinyApp(ui = ui, server = server, options = list(height = 800))