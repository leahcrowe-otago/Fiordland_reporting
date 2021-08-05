fluidPage(  
  useShinyjs(),
  titlePanel("Merge all photo data"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("filepathway", "File Pathway", choices = c("Network", "Local"), selected = "Network", inline = FALSE), 
      textInput("filepathinput", (HTML(paste("Local pathway for photo analysis spreadsheet", '<br/>', "Example: C:/Users/leah.crowe/Desktop/Canada Data Processing/")))),
      radioButtons("areainput", "Area monitored", choices = c("Doubtful", "Dusky","Milford","Other"), selected = "Doubtful", inline = FALSE), 
      #radioButtons("tzone", "Camera Time Zone", choices = c("Atlantic Time","Eastern Time"), selected = "Eastern Time", inline = FALSE),
      textInput("photoyear","Year (YYYY)"),
      textInput("photomonth","Month (MM)"),
      textInput("photofile", "Filename (must be a csv, but do not enter the extension)"),
      #textInput("permit", "Permit Number", placeholder = "MMPA #####"),
      actionButton("photogo", "Get positions")
    ),
    mainPanel(
      (HTML(paste('<br/>',
                  "CSV must include these columns:",'<br/>',
                  '<br/>',
                  "Filename, Date, ID_Name, Part, Notes, Photographer, Catalogue_update"))),
      br(),
      textOutput("finalmess"),
      br(),
      leafletOutput("finalleaf")
    )
  )
)