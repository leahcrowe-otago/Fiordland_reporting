library(dplyr); library(exifr); library(stringr); library(lubridate)

#survey_area<-'Doubtful'
survey_area<-'Dusky'
path<-paste0("//storage.hcs-p01.otago.ac.nz/sci-marine-mammal-backup/Fiordland Bottlenose dolphin/Long Term Monitoring/",survey_area," Sound Dolphin Monitoring/")
year<-2021
month<-'02'

fullpath<-paste0(path,year,"/",year,"_",month,"/")
folder.list<-list.files(fullpath, pattern = paste0("^",year), full.names = F)
filenames<-sapply(folder.list, function (x) list.files(paste0(fullpath, x), pattern = c("*.jpg$|*.JPG$|*.NEF"), full.names = T, recursive = T))
filenames_unlist<-unlist(filenames, use.names = F)
length(filenames_unlist)

metadata<-exifr::read_exif(filenames_unlist, tags = c("filename", "DateTimeOriginal"))
############
##this finds ands
and<-metadata%>%
  mutate(FileName = str_replace(FileName, "with","and"))%>%
  filter(grepl(' and ',FileName))%>%
  mutate(FileName = str_replace(FileName, " '",""),
         Name = toupper(stringr::word(FileName, 3)),
         Comments = "")
  
photoperind<-metadata%>%
  mutate(FileName = str_replace(FileName, " '",""),
         Name = toupper(sub(" .*","",FileName)),
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

write.csv(photoperind, paste0('./data/Ind_per_photo_',year,"_",month,'_',Sys.Date(),'.csv'), row.names = F)

