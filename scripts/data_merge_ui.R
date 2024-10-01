fluidPage(  
  useShinyjs(),
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      radioButtons("filepathway", "File Pathway", choices = c("Network", "Local"), selected = "Network", inline = T), 
      radioButtons("missing", "Missing data to hack in?", choices = c("Y", "N"), selected = "N", inline = T), 
      textInput("filepathinput", (HTML(paste("Local pathway for photo analysis spreadsheet", '<br/>', "Example: C:/Users/leahm/Documents/Otago/FBD data management/Fieldwork/")))),
      textInput("photoyear","Year (YYYY)"),
      textInput("photomonth","Month (MM)"),
      radioButtons("areainput", "Area monitored", choices = c("Doubtful","Dusky","Milford","Other"), selected = "Doubtful", inline = FALSE), 
      selectInput("otherfiord","Fiords surveyed:", c("Piopiotahi-Milford" = "Piopiotahi-Milford Sound",
                                                           "Te Houhou-George" = "Te Houhou-George Sound",
                                                           "Taiporoporo-Charles" = "Taiporoporo-Charles Sound",
                                                           "Hinenui-Nancy" = "Hinenui-Nancy Sound",
                                                           "Patea-Doubtful" = "Patea-Doubtful complex",
                                                           "Te R\u101-Dagg" = "Te R\u101-Dagg Sound",
                                                           "Tamatea-Dusky" = "Tamatea-Dusky complex",
                                                           "Taiari-Chalky" = "Taiari-Chalky Inlet",
                                                           "Rakituma-Preservation" = "Rakituma-Preservation Inlet"), multiple = T),
      radioButtons("surveydata_file", "Compile survey data or load file?", choices = c("compile","load"), selected = "load", inline = T),
      radioButtons("EXIF", "Collect EXIF data or load file?", choices = c("collect","load"), selected = "load", inline = T),
      actionButton("photogo", "Go"),
      br(),
      textOutput("error")
    ),
    mainPanel(
      splitLayout(radioButtons("locbase","Base:", choices = c("Deep Cove","Anchor Island","Southern Winds","Polaris II","Pembroke"), selected = "Deep Cove", inline = FALSE),
                  radioButtons("vessel","Vessel:", choices = c("Nemo","Mark Kearney","Steve Gooding","Southern Winds","Pembroke"), selected = "Nemo", inline = FALSE)),
      uiOutput("crew"),
      textAreaInput("wx_comments","Weather comments (handy macron \u101):", "General comments on the weather during this trip", height = 100, width = 700), 
      textAreaInput("calf_comments","Calf comments:", "Highlight new calves observed in this trip", height = 100, width = 700),
      textAreaInput("next_comments","Next step comments:", "All individuals sighted were members of the x pod. Recommendations for the future generated from the experiences of this trip", height = 100, width = 700),
      br(),
      downloadButton("report", "Generate Report"),
      #textOutput("finalmess"),
      br(),
      br()
      #leafletOutput("finalleaf")
    )
  )
)