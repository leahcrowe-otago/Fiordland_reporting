fluidPage(  
  useShinyjs(),
  titlePanel("Merge all photo data"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("filepathway", "File Pathway", choices = c("Network", "Local"), selected = "Network", inline = FALSE), 
      textInput("filepathinput", (HTML(paste("Local pathway for photo analysis spreadsheet", '<br/>', "Example: C:/Users/leahm/Documents/Otago/FBD data management/Fieldwork/")))),
      textInput("photoyear","Year (YYYY)"),
      textInput("photomonth","Month (MM)"),
      radioButtons("areainput", "Area monitored", choices = c("Doubtful","Dusky","Milford","Other"), selected = "Doubtful", inline = FALSE), 
      selectInput("otherfiord","Other fiords surveyed:", c("Taiporoporo-Charles" = "Taiporoporo-Charles Sound",
                                                           "Hinenui-Nancy" = "Hinenui-Nancy Sound",
                                                           "Te R\u101Dagg" = "Te R\u101-Dagg Sound",
                                                           "Taiari-Chalky" = "Taiari-Chalky Inlet",
                                                           "Rakituma-Preservation" = "Rakituma-Preservation Inlet"), multiple = T),
      radioButtons("EXIF", "collect EXIF data, or load file?", choices = c("collect","load"), selected = "load", inline = FALSE),
      actionButton("photogo", "Go"),
      br(),
      textOutput("error")
    ),
    mainPanel(
      splitLayout(radioButtons("locbase","Base:", choices = c("Deep Cove","Anchor Island","Southern Winds"), selected = "Deep Cove", inline = FALSE),
                  radioButtons("vessel","Vessel:", choices = c("Nemo","Mark Kearney","Steve Gooding","Southern Winds"), selected = "Nemo", inline = FALSE)),
      uiOutput("crew"),
      textAreaInput("wx_comments","Weather comments:", "General comments on the weather during this trip", height = 100, width = 500), 
      textAreaInput("calf_comments","Calf comments:", "Highlight new calves observed in this trip", height = 100, width = 500),
      textAreaInput("next_comments","Next step comments:", "All individuals sighted were members of the x pod. Recommendations for the future generated from the experiences of this trip", height = 100, width = 500),
      br(),
      downloadButton("report", "Generate Report"),
      #textOutput("finalmess"),
      br()
      #leafletOutput("finalleaf")
    )
  )
)