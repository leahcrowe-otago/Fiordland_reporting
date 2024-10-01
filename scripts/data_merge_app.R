#####################################################
## R Shiny Application for merging FBD trip data   ##  
##         Written By: Leah Crowe 2021             ##
#####################################################

####################
## User interface ##
####################

ui = source('./scripts/data_merge_ui.R', local = TRUE)$value
############
## Server ##
############

## Define server logic 
server = function(input, output, session) {
  
  source('./scripts/data_merge_server.R', local = TRUE)$value
  
}

#########################
## Create Shiny object ##
#########################

shinyApp(ui = ui, server = server, options = list(height = 800))