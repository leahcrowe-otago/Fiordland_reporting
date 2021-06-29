
library(dplyr); library(exifr); library(stringr); library(lubridate)

path<-"//storage.hcs-p01.otago.ac.nz/sci-marine-mammal-backup/Fiordland Bottlenose dolphin/Long Term Monitoring/Doubtful Sound Dolphin Monitoring/"
year<-2018
YYYY_MM<-'2018_10'

fullpath<-paste0(path,year,"/",YYYY_MM,"/")
folder.list<-list.files(fullpath, pattern = paste0("^",year), full.names = F)
folder.list<-folder.list[4]
filenames<-sapply(folder.list, function (x) list.files(paste0(fullpath, x), pattern = c("*NEF"), full.names = T, recursive = T))
filenames_unlist<-unlist(filenames, use.names = F)

metadata<-exifr::read_exif(filenames_unlist, tags = c("filename", "DateTimeOriginal"))
metadata$SourceFile[1]

metadata.jpg<-metadata%>%
  mutate(jpg_filename = str_replace(SourceFile, "\\.NEF","\\.jpg"))%>%
  mutate(jpg_filename = str_replace(jpg_filename, "Original NEF files/",""),
         datetimetz = ymd_hms(DateTimeOriginal, tz = "Pacific/Auckland"))

for (i in 1:nrow(metadata.jpg)){
  
  Sys.setFileTime(metadata.jpg$jpg_filename[i], metadata.jpg$datetimetz[i])
}


