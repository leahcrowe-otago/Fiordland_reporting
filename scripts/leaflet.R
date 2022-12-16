library(leaflet);library(leaflet.esri);library(leaflet.extras)
library(lubridate); library(dplyr)

#pharea = "Dusky"
phyear = 2022
phmonth = '07'
#phareafile = paste0(pharea,' Sound Dolphin Monitoring/')
phareafile = paste0('Other fiords/Survey data/')
pathway<-paste0('//storage.hcs-p01.otago.ac.nz/sci-marine-mammal-backup/Fiordland bottlenose dolphin/Long Term Monitoring/')
pathimage<-paste0(pathway,phareafile,phyear,'/',phyear,'_',phmonth)

f_data<-read.csv(paste0(pathimage,"/f_data_",phyear,"_",phmonth,".csv"), header = T, stringsAsFactors = T)

datepal<-colorFactor(palette = "Spectral", domain = unique(f_data$DATE), ordered = TRUE)

base<-leaflet(data = f_data) %>% 
  addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels=TRUE) %>%
  addWMSTiles(
    "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
    layers = c("1-degree grid", "5-degree grid"),
    options = WMSTileOptions(format = "image/png8", transparent = TRUE),
    attribution = NULL)%>%
  addLayersControl(
    overlayGroups = ~Date,
   options = layersControlOptions(collapsed = FALSE))

for (i in unique(f_data$Date)){

  date_tracks<-f_data%>%filter(Date == i)
  print(date_tracks)
  
  base = addCircleMarkers(base,
                          lng= ~ date_tracks$Longitude,
                          lat= ~ date_tracks$Latitude,
                          group= date_tracks$Date,
                          color= datepal(date_tracks$Date),
                          stroke = F,
                          radius = 2,
                          fillOpacity = 1,
                          popup = ~paste0(date_tracks$Datetime, "<br> ",date_tracks$Latitude, " ",date_tracks$Longitude))
}

base

for (i in unique(f_data$Date)){
  
    date_tracks<-f_data%>%filter(Date == i)%>%filter(Event_Type == "Encounter START")
    print(date_tracks)
    
    base = addCircleMarkers(base,
                        lng= ~ date_tracks$Longitude,
                        lat= ~ date_tracks$Latitude,
                        group= date_tracks$Date,
                        color= datepal(date_tracks$Date),
                        weight = 5,
                        stroke = T,
                        fillOpacity = 0.8,
                        popup = ~paste0(date_tracks$Datetime, "<br> ",date_tracks$Latitude, " ",date_tracks$Longitude)
                        )
    }


base
