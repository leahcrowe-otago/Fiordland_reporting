library(sf)
library(gpx)
library(leaflet)
library(leaflet.esri)
library(dplyr)
library(lubridate)

## pathway ----
#pharea = "Other Fiords/Nancy"
pharea = "Dusky"
phyear = 2010
phmonth = '08'

phareafile = paste0(pharea,' Sound Dolphin Monitoring/')
phareafile = paste0(pharea,' Sound/')

pathway<-paste0('//storage.hcs-p01.otago.ac.nz/sci-marine-mammal-backup/Fiordland bottlenose dolphin/Long Term Monitoring/')
pathimage<-paste0(pathway,phareafile,phyear,'/',phyear,'_',phmonth)

## read GPX ----

gpx_list<-list.files(paste0(pathimage,"/Tracks/"), pattern = "*.gpx", full.names = T, all.files = F)

gpx<-lapply(gpx_list, function(x) gpx::read_gpx(x))
typeof(gpx)
gpx_tracks<-lapply(gpx, function(x) x$tracks)

gpx_tracks<-bind_rows(unname(gpx[[1]]$tracks))
gpx_tracks<-bind_rows(gpx_tracks)

unique(as.Date(gpx_tracks$Time))
m_nm<-1/1852 # 1 m is 1/1852 nautical mile
## filter for specific time period that is missing from Cybertracker ----
missing_tracks<-gpx_tracks%>%
  mutate(gmt_datetime = ymd_hms(Time, tz = "GMT")) %>%
  mutate(nz_datetime = with_tz(gmt_datetime, tz = "Pacific/Auckland"))%>%
  #filter(nz_datetime >= "2022-07-14 10:12:30" & nz_datetime <= "2022-07-14 11:44:14")%>%
  mutate(DATE = as.Date(nz_datetime, tz = "Pacific/Auckland"), TIME = format(nz_datetime, tz = "Pacific/Auckland", format = "%H:%M:%S %p"))%>%
  dplyr::select(nz_datetime, DATE, TIME, Latitude, Longitude)%>%
  dplyr::rename(Datetime = nz_datetime, LATITUDE = Latitude, LONGITUDE = Longitude)%>%
  # great circle distance in nm, a is the equatorial axis, f is the inverse flattening of the ellipsoid, choices below correspond to 'GRS80' ellipsoid
  mutate(dist_nm = geosphere::distVincentyEllipsoid(matrix(c(LONGITUDE,LATITUDE), ncol = 2),matrix(c(lead(LONGITUDE), lead(LATITUDE)), ncol =2),a=6378137, f=1/298.257222101)*m_nm)%>%
  mutate(speed_kts = (dist_nm/(as.numeric(lead(Datetime) - Datetime))) * 3600)%>%
  arrange(Datetime)

missing_tracks<-missing_tracks%>%filter(ymd_hms(Datetime) > ymd_hms("2011-08-02 14:00:00") & ymd_hms(Datetime) < ymd_hms("2011-08-02 15:30:00"))

## plot ----

leaflet(data = missing_tracks) %>%
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
write.csv(missing_tracks, paste0(pathimage,"/Tracks/",phyear,"_",phmonth,"_missing.csv"), row.names = F, na = "")
