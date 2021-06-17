library(dplyr); library(exifr); library(stringr); library(lubridate)

path<-"//storage.hcs-p01.otago.ac.nz/sci-marine-mammal-backup/Fiordland Bottlenose dolphin/Long Term Monitoring/Doubtful Sound Dolphin Monitoring/"
year<-2021
YYYY_MM<-'2021_05'

fullpath<-paste0(path,year,"/",YYYY_MM,"/")
folder.list<-list.files(fullpath, pattern = paste0("^",year), full.names = F)
folder.list<-folder.list[1:3]
filenames<-sapply(folder.list, function (x) list.files(paste0(fullpath, x,'/Lab camera'), pattern = "*.JPG", full.names = T))
filenames_unlist<-unlist(filenames, use.names = F)
length(filenames_unlist)

metadata<-exifr::read_exif(filenames_unlist, tags = c("filename", "DateTimeOriginal"))
############

and<-metadata%>%
  filter(grepl(' and ',FileName))%>%
  mutate(FileName = str_replace(FileName, " '",""),
         Name = stringr::word(FileName, 3),
         Comments = "")
  
Doubtful2021_05<-metadata%>%
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

write.csv(Doubtful2021_05, paste0('./data/Doubtful2021_05_',Sys.Date(),'.csv'), row.names = F)

