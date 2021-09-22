library(leaflet);library(leaflet.esri);library(leaflet.extras)
library(lubridate); library(dplyr)

pharea = "Dusky"
phyear = 2021
phmonth = '07'
phareafile = paste0(pharea,' Sound Dolphin Monitoring/')
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
    overlayGroups = ~DATE,
   options = layersControlOptions(collapsed = FALSE))

for (i in unique(f_data$DATE)){

  date_tracks<-f_data%>%filter(DATE == i)
  print(date_tracks)
  
  base = addCircleMarkers(base,
                          lng= ~ date_tracks$LON,
                          lat= ~ date_tracks$LAT,
                          group= date_tracks$DATE,
                          color= datepal(date_tracks$DATE),
                          stroke = F,
                          radius = 2,
                          fillOpacity = 1)
}

base

for (i in unique(f_data$DATE)){
  
    date_tracks<-f_data%>%filter(DATE == i)%>%filter(Home.screen == "Encounter START")
    print(date_tracks)
    
    base = addCircleMarkers(base,
                        lng= ~ date_tracks$LON,
                        lat= ~ date_tracks$LAT,
                        group= date_tracks$DATE,
                        color= datepal(date_tracks$DATE),
                        weight = 7,
                        stroke = T,
                        fillOpacity = 0.8
                        )
    }


base
