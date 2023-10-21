library(dplyr); library(exifr); library(stringr); library(lubridate)

#attempt to fix premature eof error
#configure_exiftool(command = character(0), install_url = TRUE)

#survey_area<-'Doubtful'
survey_area<-'Dusky'
#survey_area<-'Dagg Sound'
if (survey_area == 'Doubtful' | survey_area == 'Dusky'){
  fiord_path<-paste0(survey_area," Sound Dolphin Monitoring/")
} else {
  fiord_path<-paste0('Other Fiords/',survey_area,'/')
}
path<-paste0("//storage.hcs-p01.otago.ac.nz/sci-marine-mammal-backup/Fiordland Bottlenose dolphin/Long Term Monitoring/",fiord_path)
year<-2015
list.files(paste0(path,year,"/"))
month<-'03'

fullpath<-paste0(path,year,"/",year,"_",month,"/")
folder.list<-list.files(fullpath, pattern = paste0("^",year), full.names = F)
#folder.list<-folder.list[7:8]
filenames<-sapply(folder.list, function (x) list.files(paste0(fullpath, x), pattern = c("*.jpg$|*.JPG$|*.NEF"), full.names = T, recursive = T))
filenames_unlist<-unlist(filenames, use.names = F)
length(filenames_unlist)

print(Sys.time())
metadata<-exifr::read_exif(filenames_unlist, tags = c("filename", "DateTimeOriginal"))
print(Sys.time())
nrow(metadata)
############
##this finds ands
and<-metadata%>%
  mutate(FileName = str_replace(toupper(FileName), "WITH","and"))%>%
  mutate(FileName = str_replace(FileName, "&","and"))%>%
  filter(grepl(' and ',tolower(FileName)))%>%
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
         Filename = str_sub(FileName,-12, -5),
         DateTime = ymd_hms(DateTimeOriginal),
         Date = as.Date(ymd_hms(DateTimeOriginal)),
         Part = case_when(
           Part == 'l' ~ 'LD',
           Part == 'r' ~ 'RD',
           TRUE ~ Part
         ))%>%
  #dplyr::select(Filename, Date, DateTime, Name, Part, Comments)%>%
  mutate(SURVEY_AREA = toupper(survey_area),
         TRIP = paste0(year,"_",month), 
         Group = '',
         Photographer = '')%>%
  dplyr::select(SURVEY_AREA, TRIP, Date, Filename, DateTime, Name, Part, Group, Photographer, Comments, SourceFile, FileName, DateTimeOriginal)%>%
  arrange(DateTime, Filename)

write.csv(photoperind, paste0('./data/Ind_per_photo_',year,"_",month,'_',Sys.Date(),'.csv'), row.names = F)


#############

csv_list<-list.files("./data", pattern = "Ind_per_photo_2016_12_2023-10-*", full.names = T, recursive = T)

csv_data<-lapply(csv_list, function(x)
  read.csv(x)
)

nrow(csv_data[[2]])
new_csv<-bind_rows(csv_data)
nrow(new_csv)
unique(new_csv$Date)
write.csv(new_csv, paste0('./data/Ind_per_photo_',year,"_",month,'_',Sys.Date(),'_merge.csv'), row.names = F)
