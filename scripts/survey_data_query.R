library(odbc);library(dplyr);library(DBI);library(lubridate);library(mapview);library(leaflet);library(leaflet.esri)

source('~/Documents/git_otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value
survey_data<-dbReadTable(con, "survey_data")

survey_data_q<-survey_data%>%
  filter(DATE >= "2022-07-04" & DATE <= "2022-07-14")

survey_data_q

write.csv(survey_data_q,'./data/2022_07-Dusky_surveydata.csv', row.names = F)

leaflet(data = survey_data_q) %>% 
  addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels=TRUE) %>%
  addWMSTiles(
    "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
    layers = c("1-degree grid", "5-degree grid"),
    options = WMSTileOptions(format = "image/png8", transparent = TRUE),
    attribution = NULL)%>%
  addCircleMarkers(lng= ~ LONGITUDE,
                   lat= ~ LATITUDE,
                   stroke = F,
                   radius = 2,
                   fillOpacity = 1)
