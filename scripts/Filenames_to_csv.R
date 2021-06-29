library(dplyr); library(exifr); library(stringr); library(lubridate)

path<-"//storage.hcs-p01.otago.ac.nz/sci-marine-mammal-backup/Fiordland Bottlenose dolphin/Long Term Monitoring/Doubtful Sound Dolphin Monitoring/"
year<-2021
YYYY_MM<-'2021_01'

fullpath<-paste0(path,year,"/",YYYY_MM,"/")
folder.list<-list.files(fullpath, pattern = paste0("^",year), full.names = F)
filenames<-sapply(folder.list, function (x) list.files(paste0(fullpath, x), pattern = c("*.jpg","*.JPG"), full.names = T, recursive = T))
filenames_unlist<-unlist(filenames, use.names = F)
length(filenames_unlist)

metadata<-exifr::read_exif(filenames_unlist, tags = c("filename", "DateTimeOriginal"))
############

and<-metadata%>%
  mutate(FileName = str_replace(FileName, "with","and"))%>%
  filter(grepl(' and ',FileName))%>%
  mutate(FileName = str_replace(FileName, " '",""),
         Name = stringr::word(FileName, 3),
         Comments = "")
  
photoperind<-metadata%>%
  mutate(FileName = str_replace(FileName, " '",""),
         Name = sub(" .*","",FileName),
         Comments = case_when(
           grepl("'", FileName) ~ "Conditional match",
           TRUE ~ ''))%>%
  bind_rows(and)%>%
  mutate(Part = str_sub(FileName,-14,-14),
         Filename = str_sub(FileName,-12),
         DateTime = ymd_hms(DateTimeOriginal),
         Date = as.Date(ymd_hms(DateTimeOriginal)),
         Part = case_when(
           Part == 'l' ~ 'LD',
           Part == 'r' ~ 'RD',
           TRUE ~ Part
         ))%>%
  #dplyr::select(Filename, Date, DateTime, Name, Part, Comments)%>%
  arrange(Filename)

write.csv(photoperind, paste0('./data/Ind_per_photo_',YYYY_MM,'_',Sys.Date(),'.csv'), row.names = F)

