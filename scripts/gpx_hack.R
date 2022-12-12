library(sf)
library(gpx)
library(leaflet)
library(leaflet.esri)
library(dplyr)
library(lubridate)

## pathway ----
pharea = "Dusky"
phyear = 2022
phmonth = '07'
phareafile = paste0(pharea,' Sound Dolphin Monitoring/')

pathway<-paste0('//storage.hcs-p01.otago.ac.nz/sci-marine-mammal-backup/Fiordland bottlenose dolphin/Long Term Monitoring/')
pathimage<-paste0(pathway,phareafile,phyear,'/',phyear,'_',phmonth)

## read GPX ----
gpx_14Jul2022<-gpx::read_gpx(paste0(pathimage,"/Tracks/14_07_22transit.gpx"))

tracks_14Jul2022<-gpx_14Jul2022$tracks$`14_07_22transit`
head(tracks_14Jul2022)

## filter for specific time period that is missing from Cybertracker ----
charles_Nemo<-tracks_14Jul2022%>%
  mutate(gmt_datetime = ymd_hms(Time, tz = "GMT")) %>%
  mutate(nz_datetime = with_tz(gmt_datetime, tz = "Pacific/Auckland"))%>%
  filter(nz_datetime >= "2022-07-14 10:12:30" & nz_datetime <= "2022-07-14 11:44:14")%>%
  mutate(DATE = as.Date(nz_datetime, tz = "Pacific/Auckland"), TIME = format(nz_datetime, tz = "Pacific/Auckland", format = "%H:%M:%S %p"))%>%
  dplyr::select(nz_datetime, DATE, TIME, Latitude, Longitude)%>%
  dplyr::rename(Datetime = nz_datetime, LATITUDE = Latitude, LONGITUDE = Longitude)

charles_Nemo

## plot ----
leaflet(data = charles_Nemo) %>% 
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

## write csv ----
write.csv(charles_Nemo, paste0(pathimage,"/Tracks/charles_Nemo_14Jul2022.csv"), row.names = F, na = "")
