disable("report")
crew.csv<-read.csv('./data/FBD_crew.csv')%>%mutate(creworg = paste0(FIRST,' ',LAST,', ',ORGANIZATION))
crew_list<-crew.csv$creworg
output$crew<-renderUI({
  pickerInput("crew","Crew", choices = c(crew_list), multiple = T, inline = F)
})

observeEvent(input$photogo,{
  
  withProgress(message = 'Processing data and compiling report...', value = 0, {
  
  output$finalmess<-renderText({""})
  
# user input values ----
  phserv<-input$filepathway
  phyear<-input$photoyear
  phmonth<-input$photomonth
  pharea<-input$areainput
  ofiords<-input$otherfiord
  print(ofiords)
 
# pharea = "Other"
# phyear = 2023
# phmonth = '04'
# phserv = "Network"

  if(pharea == "Other"){
    phareafile = 'Other Fiords/Survey data/'
  } else {
    phareafile = paste0(pharea,' Sound Dolphin Monitoring/')
  }
  
    if(phserv == 'Network'){
      pathway<-paste0('//storage.hcs-p01.otago.ac.nz/sci-marine-mammal-backup/Fiordland bottlenose dolphin/Long Term Monitoring/')
      pathimage<-paste0(pathway,phareafile,phyear,'/',phyear,'_',phmonth)
      } else if (phserv == 'Local'){
      pathway<-input$filepathinput
      #pathway<-"D:/FBD/Doubtful Sound Dolphin Monitoring/"
      #pathway<-"C:/Users/leahm/OneDrive - University of Otago/Documents/Otago/FBD data management/Fieldwork/"
      ###pathimage<-paste0(pathway,'Other Fiords/',phyear,'/',phyear,'_',phmonth)
      pathimage<-paste0(pathway,phyear,'/',phyear,'_',phmonth)
      }
  
  print(pathimage)
  print(input$surveydata_file)
  
# Survey sightings and tracks ----
  
  print("sigs and trackline")
  
## compile ----
  if (input$surveydata_file == "compile"){
### read in final sighting file ----  
  sigs<-read.csv(paste0(pathimage,"/Sightings/f_",phyear,"_",phmonth," CyberTracker.csv"), header = T, stringsAsFactors = F)
  sigs<-sigs%>%mutate(Datetime = dmy_hms(paste(Date, Time)))%>%
    dplyr::select(Datetime, everything())
  sigs$Date<-dmy(sigs$Date)
  
  daterange<-range(sigs$Datetime)
  
  print("raw tracks")
  
### option if data came from more than one device, but needs to be manually adjusted below ----
  if(pharea == "Dusky" & phyear == 2023 & phmonth == '06'){
  
    raw_tracks1<-sf::st_read(paste0(pathimage,"/Tracks"), layer = paste0(phyear,"_",phmonth), quiet = T)
    raw_tracks2<-sf::st_read(paste0(pathimage,"/Tracks"), layer = paste0(phyear,"_",phmonth,"_Leah phone"), quiet = T)

    raw_tracks<-as.data.frame(raw_tracks1)%>%
      mutate(SST = as.character(SST),
             DEPTH = as.numeric(DEPTH))%>%
      bind_rows(as.data.frame(raw_tracks2))
    
  } else {
### read trackline shapefile ----  
    raw_tracks<-sf::st_read(paste0(pathimage,"/Tracks"), layer = paste0(phyear,"_",phmonth), quiet = T)
  
    }
  #LATITUDE VS LAT, AND LONGITUDE VS LON in earlier versions of the app
  if (phyear < 2022){
    raw_tracks<-raw_tracks%>%
      dplyr::rename(LATITUDE = LAT, LONGITUDE = LON)
  }

  print("tracks")
  print(head(raw_tracks))
  
  #tracks grouped by device to account for multiple platforms on same day
  tracks<-as.data.frame(raw_tracks)%>%
    #rename("LATITUDE" = "LAT", "LONGITUDE" = "LON")%>%
    dplyr::select(DATE, TIME, LATITUDE, LONGITUDE, DEVICEID)%>% #at 2022_07 & 2023_10 LATITUDE and LONGITUDE became LAT and LON, idk
    mutate(Datetime = ymd_hms(paste(DATE, TIME)))%>%
    dplyr::select(Datetime, everything())%>%
    arrange(DEVICEID, Datetime)%>%
    distinct()%>%
    group_by(DEVICEID, Datetime)%>%
    mutate(rank = rank(Datetime, ties.method = "first"))%>% # removes duplicated time/position records
    filter(rank == 1)%>%
    dplyr::select(-rank)
  
  tracks$Datetime<-ymd_hms(tracks$Datetime)
  
  #find nearest position for time adjusted in editing
  #considers multiple tablets/devices
  
  device<-unique(sigs$DEVICEID)
  sig_near<-NULL
  tracksrank<-NULL
  nrow(sigs)
  nrow(tracks)
#### find nearest location to times in sightings file, filter to 10 secs in the tracklines ----  
  for (i in 1:length(device)){
  
  x = device[i]
    
  sig_fil<-sigs%>%
      filter(DEVICEID == x)

  tracks_fil<-tracks%>%
      filter(DEVICEID == x)%>%
      filter(!is.na(Datetime))
  
  print(i)
  print(x)
  
  data.table::setDT(sig_fil)[,  Latitude := data.table::setDT(tracks_fil)[sig_fil, LATITUDE, on = c("Datetime"), roll = "nearest"]]
  data.table::setDT(sig_fil)[,  Longitude := data.table::setDT(tracks_fil)[sig_fil, LONGITUDE, on = c("Datetime"), roll = "nearest"]]
  
  print(sig_fil)
  print(nrow(sig_near))
  
  sig_near<-sig_near%>%
    bind_rows(sig_fil)
  
  print(nrow(sig_near))
  
  print(head(tracks_fil))
  print(nrow(tracks_fil))
  ##get bin list using seq for every 10 seconds
  ##order by DateTime and rank
  tracksrank_fil<- tracks_fil %>%
    arrange(Datetime)%>%
    ungroup()%>%
    mutate(tracksbin = cut(Datetime, breaks = c(seq(from = tracks_fil$Datetime[1], to = tracks_fil$Datetime[nrow(tracks_fil)], by = 10)))) %>%
    arrange(Datetime, tracksbin) %>% 
    group_by(tracksbin) %>% 
    mutate(rank=rank(Datetime, ties.method = "first"))%>% 
    filter(rank == 1)%>%
    ungroup()%>%
    dplyr::select(-tracksbin, -rank)
  
  
  tracksrank<-tracksrank%>%
    bind_rows(tracksrank_fil)
  
  print(nrow(tracks_fil))
  print(nrow(tracksrank))
  
  }
  
  sigs<-sig_near
  print(head(sigs))

#### final survey data ----
  f_data<-tracksrank%>%
    full_join(sigs, by = c("Datetime","LATITUDE"="Latitude","LONGITUDE"="Longitude","DATE"="Date","TIME"="Time","DEVICEID"))%>%
    mutate(DATE = as.factor(DATE),
           across(where(is.character), ~na_if(., "")))%>%
    arrange(DEVICEID, Datetime)

##### add missing tracks if hacking in ----
  
  if(input$missing == "Y"){
    print("missing")
    # add gpx files made in "gpx_hack.R"
    missing_tracks<-read.csv(paste0(pathimage,"/Tracks/",phyear,"_",phmonth,"_missing.csv"), header = T, stringsAsFactors = F)
    missing_tracks$Datetime<-ymd_hms(missing_tracks$Datetime)
    missing_tracks$DATE<-ymd(missing_tracks$DATE)
    unique(missing_tracks$DATE)
    missing_sigs<-read.csv(paste0(pathimage,"/Sightings/f_",phyear,"_",phmonth," CyberTracker_missing.csv"), header = T, stringsAsFactors = F)
    missing_sigs<-missing_sigs%>%mutate(Datetime = dmy_hms(paste(Date, Time)))%>%
      dplyr::select(Datetime, everything())
    
    missing_sigs$Date<-dmy(missing_sigs$Date)
    #find nearest position for time changes in editing
    
    data.table::setDT(missing_sigs)[,  Latitude := data.table::setDT(missing_tracks)[missing_sigs, LATITUDE, on = "Datetime", roll = "nearest"]]
    data.table::setDT(missing_sigs)[,  Longitude := data.table::setDT(missing_tracks)[missing_sigs, LONGITUDE, on = "Datetime", roll = "nearest"]]
    
    ##get bin list using seq for every 10 seconds
    ##order by DateTime and rank
    missing_tracks<-missing_tracks %>% 
      mutate(tracksbin = cut(missing_tracks$Datetime, breaks = c(seq(from = missing_tracks$Datetime[1], to = missing_tracks$Datetime[nrow(missing_tracks)], by = 10)))) %>%
      arrange(Datetime, tracksbin) %>% 
      group_by(tracksbin) %>% 
      mutate(rank=rank(Datetime, ties.method = "first"))%>% 
      filter(rank == 1)%>%
      ungroup()%>%
      dplyr::select(-tracksbin, -rank)
    
    f_data_missing<-missing_tracks%>%
      full_join(missing_sigs, by = c("Datetime","LATITUDE"="Latitude","LONGITUDE"="Longitude","DATE"="Date","TIME"="Time"))%>%
      mutate(DATE = as.factor(DATE),
             across(where(is.character), ~na_if(., "")))%>%
      arrange(Datetime)
      
    f_data<-f_data%>%
      bind_rows(f_data_missing)
  }
  
##### event numbers to each effort period ----
  Event<-f_data%>%filter(Effort == "Effort ON")%>%ungroup()%>%mutate(Event = 1:n())%>%as.data.frame()
  print(Event)
  print(head(f_data))
  
  onoffeffort<-f_data%>%
    filter(grepl('Effort',Effort))%>%
    distinct(DATE, Datetime, DEVICEID)%>%
    filter(!is.na(Datetime))%>%
    mutate(Event = as.numeric(rep(1:(n()/2),each = 2)))%>%
    group_by(Event)%>%
    mutate(min = min(Datetime),
      max = max(Datetime))%>%
    dplyr::select(-Datetime)%>%
    distinct()%>%  
    as.data.frame()

  f_data<-f_data%>%
    left_join(Event, by = c("Datetime", "DATE", "TIME", "LATITUDE", "LONGITUDE", "Event_Type", "Effort",
"Deployment", "Platform", "Crew", "Beaufort", "Swell", "Glare", "Visibility", "Overall", "Species", "Group_Size", "Calves",
"Behaviours", "Encounter_Type", "Permit", "SST", "Depth", "Note", "Tawaki", "DEVICEID"))
  
  f_data$Event<-zoo::na.locf(f_data$Event, na.rm = FALSE)

  print(unique(f_data$Event))
  print(onoffeffort)
  
  f_data<-f_data%>%
    left_join(onoffeffort, by = c('Event','DATE',"DEVICEID"))%>% 
    filter(Datetime >= min & Datetime <= max)%>%
    as.data.frame()
  nrow(f_data)
  
  print(f_data%>%
          filter(grepl("Encounter", Event_Type))%>%
          arrange(DEVICEID, Datetime))

##### sighting numbers for encounter events ----    
  sig_num<-f_data%>%
    filter(grepl("Encounter", Event_Type))%>%
    arrange(DEVICEID, Datetime)%>%
    mutate(signum = rep(1:(n()/2), each = 2))%>%
    dplyr::select(Datetime, DATE, signum, Species, Effort, Event_Type, Encounter_Type, Permit, DEVICEID)%>%
    arrange(desc(Datetime))
  
  sig_num$Species<-zoo::na.locf(sig_num$Species, na.rm = FALSE)
  
  f_data$Effort[f_data$Effort==""] <- NA
  f_data$Encounter_Type[f_data$Encounter_Type==""] <- NA
  print(nrow(f_data))
  
  f_data<-f_data%>%dplyr::select(-Encounter_Type,-Species)%>%
    left_join(sig_num, by = c("Datetime", "DATE","Event_Type", "Effort","Permit"))%>% 
    group_by(grp = cumsum(!is.na(signum))) %>% #group by Encounter_Type downfill from START to END
    mutate(Encounter_Type = replace(Encounter_Type, first(Encounter_Type) == 'Initial', 'Initial')) %>% 
    mutate(Encounter_Type = replace(Encounter_Type, first(Encounter_Type) == 'Repeat', 'Repeat')) %>% 
    mutate(Encounter_Type = replace(Encounter_Type, first(Encounter_Type) == 'Follow', 'Follow')) %>% 
    ungroup() %>% 
    dplyr::select(Datetime, DATE, TIME, LATITUDE, LONGITUDE, Effort, Platform, Event_Type, Encounter_Type, Deployment, Crew, signum,
                  Species, Group_Size, Calves, Behaviours, Permit,
                  Beaufort, Swell, Glare, Visibility, Overall, SST, Depth, Note, Event, Tawaki, -grp)%>%
    dplyr::rename(Date = DATE, Time = TIME, Latitude = LATITUDE, Longitude = LONGITUDE, Sighting_Number = signum)
     
    nrow(f_data)
  
  f_data$Platform<-zoo::na.locf(f_data$Platform, na.rm = FALSE)
  
#### write final survey data file ----  
  write.csv(f_data, paste0(pathimage,"/f_data_",phyear,"_",phmonth,".csv"), row.names = F, na = "")
  write.csv(f_data%>%filter(!is.na(Tawaki))%>%dplyr::select(Datetime, Date, Time, Latitude, Longitude,Tawaki,Note), paste0(pathimage,"/Tawaki_",phyear,"_",phmonth,"_",".csv"), row.names = F, na = "")
  
  } else {
## load survey file ----
    
  print("load")
  f_data<-read.csv(paste0(pathimage,"/f_data_",phyear,"_",phmonth,".csv"), header = T, stringsAsFactors = F)
  head(f_data)
  f_data[f_data == ''] <- NA
  
##### sighting numbers for encounter events ----   
  sig_num<-f_data%>%
    filter(grepl("Encounter", Event_Type))%>%
    arrange(desc(Datetime))%>%
    dplyr::rename("signum" = "Sighting_Number", "DATE" = "Date")
  
  print(sig_num%>%as.data.frame())
  
##### event numbers to each effort period ----
  onoffeffort<-f_data%>%
    filter(grepl('Effort',Effort))%>%
    dplyr::rename("DATE" = "Date")%>%
    distinct(DATE, Datetime)%>%
    filter(!is.na(Datetime))%>%
    ungroup()%>%
    mutate(Event = as.numeric(rep(1:(n()/2),each = 2)))%>%
    group_by(Event)%>%
    mutate(min = min(Datetime),
           max = max(Datetime))%>%
    dplyr::select(-Datetime)%>%
    distinct()%>%  
    as.data.frame()
  
  }
  
## report data ----  
  sig_count<-max(f_data$Sighting_Number, na.rm = T)
  
  sig_days<-sig_num%>%
    distinct(DATE)
  
  print(nrow(f_data))

  incProgress(3/5)
  #convert meters to nautical miles
  m_nm<-1/1852
  
### nautical miles on effort calculation
  #finding only trackline effort within the complexes (predominantly)
  if (pharea == "Dusky"){
    latmin = -45.82
    latmax = -45.48

  } else if (pharea == "Doubtful"){
    latmin = -45.5
    latmax = -45.15

  } else {
    latmin = min(f_data$Latitude)
    latmax = max(f_data$Latitude)
    print(head(f_data))
  }
  
  f_data_dist<-f_data%>%
    filter(Latitude >= latmin & Latitude <= latmax)%>%
    arrange(Datetime)%>%
    group_by(Date,Event)%>%
    mutate(LAT2 = dplyr::lag(Latitude),
           LON2 = dplyr::lag(Longitude),
           dist_km = geosphere::distVincentyEllipsoid(matrix(c(Longitude,Latitude), ncol = 2),matrix(c(LON2, LAT2), ncol =2),a=6378137, f=1/298.257222101)*m_nm)
  #finding only trackline effort within specified areas, needs manually adjusted
  if (pharea == "Other"){
    
    latmin = -45.82
    latmax = -45.48
    
    f_data_dist<-f_data_dist%>%
      filter(!(Latitude >= latmin & Latitude <= latmax))
    
    f_data<-f_data%>%
      filter(!(Latitude >= latmin & Latitude <= latmax))
  }
  
  print(f_data_dist%>%filter(dist_km > 0.1)%>%as.data.frame())
  track_dist<-round(sum(f_data_dist$dist_km, na.rm=TRUE),0)
  
  ## order events in ascending order for Last Observation Carried Forward ----
  sig_num$Permit[sig_num$Permit==""] <- NA
  sig_num$Encounter_Type[sig_num$Encounter_Type==""] <- NA
  
  sig_num<-sig_num%>%arrange(signum,Datetime)
  
  sig_num$Permit<-zoo::na.locf(sig_num$Permit, na.rm = FALSE)
  sig_num$Encounter_Type<-zoo::na.locf(sig_num$Encounter_Type, na.rm = FALSE)
  head(sig_num)
  
  onoffsigs<-sig_num%>%
    dplyr::select(signum, Datetime, Encounter_Type, Event_Type, Permit, Species)%>%
    group_by(signum, Encounter_Type)%>%
    tidyr::pivot_wider(names_from = Event_Type, values_from = Datetime)%>%
    mutate(time_w = as.numeric(difftime(`Encounter END & DATA`,`Encounter START`), units = "mins"))%>%
    as.data.frame()
  
  unique(onoffsigs$Permit)
  ### hours with dolphins by permit and distance type ----
  hours_wTt<-onoffsigs%>%
    filter(Species == "Bottlenose")%>%
    group_by(Encounter_Type, Permit)%>%
    dplyr::summarise(Total_time = sum(time_w, na.rm=TRUE))%>%
    mutate(distance = case_when(
      Encounter_Type == 'Follow' ~ '100–400',
      TRUE ~ '<100'))%>%
    ungroup()%>%
    group_by(Permit, distance)%>%
    dplyr::summarise(Total_time_dist = round(sum(Total_time, na.rm = TRUE)/60,1))%>%
    ungroup()%>%
    tidyr::pivot_wider(id_cols = distance, names_from = Permit, values_from = Total_time_dist)%>%
    replace(is.na(.), 0)%>%
    mutate(`DOC permit` = if (exists('DOC permit', where=.)) `DOC permit` else 0)%>%
    mutate(`Otago permit` = if (exists('Otago permit', where=.)) `Otago permit` else 0)%>%
    mutate(total_wTt = `DOC permit` + `Otago permit`)%>%
    dplyr::rename("DOC" = `DOC permit`, "Otago" = `Otago permit`)
  
  print(hours_wTt)
  
  
  ## Photo analysis ----
  
  if (dir.exists(pathimage) == FALSE){
    output$error<-renderText({"Double check 'Year', 'Month', and 'Area monitored' -- files cannot be found."})
  } else {
    output$error<-renderText({""})

# include photo ID or not if it doesn't exist    
    
if (identical(list.files(paste0(pathimage,"/Photo analysis"), pattern = "*.xlsx", full.names = T, all.files = F), character(0)) == FALSE){    
   
  print("identical")
  map_species<-"no"
  Tt_ID<-"yes"
  
  if (input$EXIF == "collect"){  
## if collecting timestamps, merging all PA files, gathering list of photographs, and gathering timestamps ----  
    print("merging PA")
  ### merge all PA excel spreadsheets ----
  PA_xlsx<-list.files(paste0(pathimage,"/Photo analysis"), pattern = "*.xlsx", full.names = T, all.files = F)
  #exclude weird hidden excel files
  PA_xlsx<-grep(PA_xlsx, pattern = "[~]", invert = T, value = T)
  
  head(PA_xlsx[1])
  
  PA_merge<-lapply(PA_xlsx, function (x) readxl::read_excel(x, sheet = 1, col_names = T, guess_max = 1000, range = cellranger::cell_cols("A:G")))
 
  allmerge<-do.call(rbind, PA_merge)
  head(allmerge)
  nrow(allmerge)

  allmerge$Date<-ymd(allmerge$Date)
  allmerge<-allmerge%>%
    mutate(ID_Name = case_when(
      is.na(ID_Name) ~ 'UNMA',
      TRUE ~ ID_Name))%>%
    filter(ID_Name != "CULL")
  nrow(allmerge)
  
  #### list folders that start with the year ----
  folder.list<-list.files(pathimage, pattern = paste0("^",phyear), full.names = T)
  filenames<-sapply(folder.list, function (x) list.files(x, pattern = "*.JPG$|*.jpg$|*.CR2$", full.names = T, recursive = T))
  filenames_unlist<-unlist(filenames, use.names = F)
  head(filenames_unlist)
  allphotod_df<-data.frame(fullfilename = as.vector(filenames_unlist),
             filename = basename(filenames_unlist),
             date = ymd(stringr::str_extract(filenames_unlist,'\\b\\d{8}\\b')))
  head(allphotod_df)
  nrow(allphotod_df)
  
  nrow(allmerge)
  print(nrow(allphotod_df))
  files_for_exif<-allphotod_df%>%
    mutate(filename = stringr::str_sub(filename, end = -5))%>%
    right_join(allmerge, by = c("filename" = "Filename", "date" = "Date"))%>%
    #distinct(fullfilename)%>%
    filter(!is.na(fullfilename))
  print(nrow(files_for_exif))
  incProgress(1/5)
  print("getting exif data")
  ### get exif data ----
  #http://web.mit.edu/graphics/src/Image-ExifTool-6.99/html/install.html
  #https://strawberryperl.com/
  metadata<-exifr::read_exif(files_for_exif$fullfilename, tags = c("filename", "DateTimeOriginal"))
  print(metadata)
  ##
  metadata2<-metadata%>%
    mutate(Filename = stringr::str_sub(basename(FileName), end = -5),
           Datetime = ymd_hms(DateTimeOriginal),
           Date = ymd(as.Date(Datetime)))%>%
    dplyr::select(Filename, Datetime, Date)
  nrow(metadata2)
  
  ### final file ----
  allmerge_dt<-allmerge%>%
    left_join(metadata2, by = c("Filename", "Date"))%>%
    mutate(ID_Name = case_when(grepl("EEK", ID_Name) ~ "EEK-THE-CAT",
                               grepl("JOLLY", ID_Name) ~ "JOLLY-GOOD",
                               TRUE ~ ID_Name))%>%
    filter(ID_Name != "CULL")
  
  if(pharea != "Other"){
    allmerge_dt<-allmerge_dt%>%
      mutate(Survey_Area = toupper(pharea))
  } else {
    allmerge_dt<-allmerge_dt%>%
      mutate(Survey_Area = "")
  }
  
  incProgress(2/5)
  
  } else if (input$EXIF == "load"){
### if loading a final PA file ----
    incProgress(2/5)
    print("load exif")
    allmerge_dt<-read.csv(paste0(pathimage,"/Photo analysis/f_PA_",phyear,"_",phmonth,".csv"), header = T, stringsAsFactors = T)
    allmerge_dt$Datetime<-ymd_hms(allmerge_dt$Datetime)
    allmerge_dt$Date<-ymd(allmerge_dt$Date)
    print(nrow(allmerge_dt))

    }
  
  #this is the number of unique images after culling to allow for a count when the PA is loaded
  photo_n<-nrow(allmerge_dt%>%distinct(Filename))
  
    ##add group # to match sighting number in final data
    sigminmax<-f_data%>%#filter(!is.na(Sighting_Number))%>%
      group_by(Sighting_Number)%>%
      mutate(max = ymd_hms(max(Datetime)),
             min = ymd_hms(min(Datetime)))%>%
      distinct(Date, Sighting_Number, min, max)%>%
      filter(!is.na(Sighting_Number))
    
    sigminmax$Date<-ymd(sigminmax$Date)
    
    print("Group")
    if('Group' %in% colnames(allmerge_dt)){
      print('group yes')
    allmerge_dt<-allmerge_dt%>%dplyr::select(-Group)
    }
    
#add group based on time of photo compared with sighting time ----
    allmerge_dt$Date<-ymd(allmerge_dt$Date)
    
    allmerge_dt_group<-allmerge_dt%>%
      left_join(sigminmax, by = "Date")%>%
      mutate(Group = case_when(
        Datetime >= min & Datetime <= max ~ Sighting_Number
      ))
    
    #filter out photos that fall neatly within the sighting time
    allmerge_wgroup<-allmerge_dt_group%>%filter(!is.na(Group))%>%
      dplyr::select(-Sighting_Number, -max, -min)%>%
      distinct()%>%
      arrange(Datetime)
    #filter out photos that DON'T fall neatly within the sighting time
    allmerge_wogroup<-allmerge_dt_group%>%filter(is.na(Group))%>%
      dplyr::select(-Sighting_Number, -max, -min)%>%
      distinct()%>%
      arrange(Datetime)%>%
      anti_join(allmerge_wgroup, by = c("Datetime","ID_Name","Part"))
    
    allmerge_group<-allmerge_wgroup%>%
      bind_rows(allmerge_wogroup)%>%
      arrange(Datetime)%>%
      mutate(TRIP = paste0(phyear,"_",phmonth))
    
    print(nrow(allmerge_group))
    
# write final file ----
    write.csv(allmerge_group, paste0(pathimage,"/Photo analysis/f_PA_",phyear,"_",phmonth,".csv"), row.names = F, na = "")  

    recent<-format(max(sigminmax$Date) - lubridate::years(1), "%d %b %Y")
    older<-format(max(sigminmax$Date) - lubridate::years(2), "%d %b %Y")
    print(recent)
    print(older)
    
  ## life history ----
  
  #source('./scripts/connect to MySQL.R', local = TRUE)$value
  source('./scripts/life_history_ageclass update.R', local = TRUE)$value
  # made in "life_history_ageclass.R" script
  head(lifehist)
  #print(lifehist)
  #phyear gets updated to calf year in the "life_history_ageclass.R" run
  print(phyear)
  lhyear = phyear
  
  #complicated life history related to when the survey occurs
  #could be better defined by month of last sig to determine what "dolphin" year we are dealing with
  lifehist<-lifehist%>%
    dplyr::select(POD, NAME, SEX, FIRST_YEAR, BIRTH_YEAR, LAST_YEAR, LAST_DATE, ends_with(as.character(lhyear)))%>%
    filter(across(last_col()) != 'NA' & across(last_col()) != 'D')
  
  print(lifehist)
  
  lifehist%>%filter(NAME == "BOWTIE")
  
  if (pharea != "Other"){
    lifehist<-lifehist%>%filter(POD == toupper(pharea))
  }
  
  names(lifehist)[length(names(lifehist))]<-"AgeClass" 
  # number of photos for report
  photo_counts<-allmerge_dt%>%
    filter(!grepl("\\?",ID_Name))%>%
    filter(ID_Name != "CALF")%>%
    group_by(Date, ID_Name)%>%
    tally()
  
  print('599')
  # demographic summaries of sighted dolphins for report
  daily_cap<-photo_counts%>%
    ungroup()%>%
    mutate_if(is.numeric, ~1 * (. > 0))%>%
    left_join(lifehist, by = c("ID_Name" = "NAME"))%>%
    dplyr::rename("NAME" = "ID_Name")%>%
    filter(NAME != "" & !grepl('_',NAME) & !grepl('CULL',NAME) & NAME != "UNMA" & !str_detect(NAME, "^UK") & !str_detect(NAME, "\\?"))%>%
    tidyr::pivot_wider(id_cols = c("NAME","SEX","BIRTH_YEAR","FIRST_YEAR","AgeClass"), names_from = "Date", values_from = "n")%>%
    arrange(NAME)
  
  trip_cap<-daily_cap%>%
    distinct(NAME,SEX,AgeClass)
  
  print(trip_cap%>%filter(is.na(AgeClass))%>%as.data.frame())
  
  age_sex_table<-trip_cap%>%
    dplyr::select(-NAME)%>%
    group_by(SEX, AgeClass)%>%
    tally()%>%
    tidyr::pivot_wider(names_from = SEX, values_from = n)%>%
    arrange(factor(AgeClass, levels = c("A","S-A","J","C","U")))%>%
    replace(is.na(.), 0)
 
  age_class_abbr<-function(x) {
    x %>%
    mutate(AgeClass = case_when(
          AgeClass == 'A' ~ "Adult",
          AgeClass == 'S-A' ~ "Sub-Adult",
          AgeClass == 'J' ~ "Juvenile",
          AgeClass == 'C' ~ "Calf*",
          AgeClass == 'U' ~ "Unknown"
        ))}
    
  age_sex_table<-age_class_abbr(age_sex_table)

  if (!"M" %in% colnames(age_sex_table)){
    age_sex_table<-age_sex_table%>%
      mutate(M = 0)}
  
  if (!"F" %in% colnames(age_sex_table)){
    age_sex_table<-age_sex_table%>%
      mutate(F = 0)}
  
  if (!"X" %in% colnames(age_sex_table)){
    age_sex_table<-age_sex_table%>%
      mutate(X = 0)}
  
  print('647')
  allsampledays<-f_data%>%
    distinct(Date)%>%
    mutate(value = 0)%>%
    tidyr::pivot_wider(names_from = "Date", values_from = value)
  
  disco<-function(x){
    x<-daily_cap
    freqcap<-x%>%
      dplyr::select(-NAME, -SEX, -BIRTH_YEAR,-FIRST_YEAR,-AgeClass)%>%
      as.data.frame()
    
    ##add all sample days where dolphins were not sighted
    freqcap<-tibble::add_column(freqcap, !!!allsampledays[setdiff(names(allsampledays), names(freqcap))])
    freqcap<-freqcap[,order(colnames(freqcap))]
    
    freqcap[is.na(freqcap)] <- 0 
    
    desc<-Rcapture::descriptive(freqcap)
    desc.df<-as.data.frame(desc$base.freq, row.names = FALSE)
    desc.df<-desc.df%>%
      mutate(date = lubridate::ymd(names(freqcap)))%>%
      mutate(survey = lubridate::yday(date),
             disc = cumsum(ui),
             order = 1:nrow(desc.df),
             year = lubridate::year(date))
    desc.df$survey<-as.factor(desc.df$survey)
    desc.df$disc<-as.numeric(desc.df$disc)
    
    desc.df
  }
  
  disco_data<-disco(daily_cap)
  print('680')

  disco_data$date<-ymd(disco_data$date)

  # discovery curve
    disco_curve<-ggplot(disco_data, aes(x=date, y = disc))+
    geom_line()+
    geom_col(mapping = aes(x = date, y = ni), alpha = 0.5)+
    geom_point(alpha = 0.8, size = 3)+
    theme_bw()+
    scale_colour_viridis_d(option = 'plasma', end = 0.7)+
    xlab("")+
    ylab("Individuals per day & cumulative IDs")+
    ylim(c(0,max(disco_data$disc)))+
    theme(legend.position="bottom",
          text = element_text(size = 10))
  
  ggsave(filename = 'disco_curve.png',disco_curve,device = 'png', './figures', dpi = 320, width = 120, height = 80, units = 'mm')
  
# Sightings history ----
  
  thistrip<-daily_cap%>%
    dplyr::select(-SEX, -BIRTH_YEAR, -FIRST_YEAR, -AgeClass)%>%
    tidyr::pivot_longer(!(c(NAME)),names_to = "Date")%>%
    mutate(Date = lubridate::ymd(Date))%>%
    filter(value != 0)
  
  thistrip_names<-thistrip%>%distinct(NAME)%>%as.data.frame()
  
  print('709')
  
  unseen_two_years<-lifehist%>%
    anti_join(thistrip_names)%>%
    filter(LAST_DATE < max(as.Date(onoffeffort$DATE)) - lubridate::years(1) & LAST_DATE >= max(as.Date(onoffeffort$DATE)) - lubridate::years(2))%>%
    mutate(LAST_YEAR = lubridate::year(LAST_DATE))%>%
    mutate(NameSex = case_when(
      nchar(NAME) > 2 ~ paste0(str_to_title(NAME)," (",SEX,")"),
      nchar(NAME) <= 2 ~ paste0(NAME," (",SEX,")")))
  
  unseen_names<-unseen_two_years$NAME
  
  unseen_table<-unseen_two_years%>%
    group_by(LAST_YEAR, SEX, AgeClass)%>%
    tally()%>%
    tidyr::pivot_wider(names_from = SEX, values_from = n)%>%
    arrange(factor(AgeClass, levels = c("A","S-A","J","C","U")))%>%
    replace(is.na(.), 0)
  
  unseen_table<-age_class_abbr(unseen_table)
  
  if (!"M" %in% colnames(unseen_table)){
    unseen_table<-unseen_table%>%
      mutate(M = 0)}
  
  if (!"F" %in% colnames(unseen_table)){
    unseen_table<-unseen_table%>%
      mutate(F = 0)}
  
  if (!"X" %in% colnames(unseen_table)){
    unseen_table<-unseen_table%>%
      mutate(X = 0)}
  
  print(unseen_table)
  
} else {
  
  map_species<-"yes"
  Tt_ID<-"no"
}
    
# maps ----

NZ_coast<-sf::read_sf(dsn = "./shapefiles", layer = "nz-coastlines-and-islands-polygons-topo-1500k")
  
incProgress(4/5)

  date_color<-viridis::viridis(length(unique(f_data$Date)), alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
  names(date_color) <- unique(f_data$Date)
  date_color_scale <- scale_colour_manual(name = "Date", values = date_color)

if (map_species == "no"){  
  species_shape<-c(23,21)
  names(species_shape)<-c("Bottlenose","Other")
  species_shape_scale<-scale_shape_manual(name = "Species", values = species_shape)
} else if (map_species == "yes"){
  species_shape<-c(23,24,21,22)
  names(species_shape)<-c("Bottlenose","Common","Humpback","Other")
  species_shape_scale<-scale_shape_manual(name = "Species", values = species_shape)
}
  
effmap<-ggplot()+
  #geom_polygon(NZ_coast, mapping = aes(X,Y,group = L2), alpha = 0.8)+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "grey")+
  geom_point(f_data%>%arrange(Datetime), mapping = aes(Longitude, Latitude, group = Date, color = Date), size = 0.1)+
  #geom_path(f_data%>%arrange(Datetime), mapping = aes(LON, LAT, group = DATE, color = DATE))+
  theme_bw()+
  date_color_scale+
  xlab("Longitude")+
  ylab("Latitude")+
  guides(color = guide_legend(override.aes = list(size = 2)))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom",
        plot.background = element_blank())

legend<-cowplot::get_legend(effmap)

effmap<-effmap+theme(legend.position = "none")

f_data%>%filter(!is.na(Encounter_Type))

sigmap<-ggplot()+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "grey")+
  geom_point(f_data%>%filter(!is.na(Encounter_Type) | Event_Type == 'Encounter END & DATA'), mapping = aes(Longitude, Latitude, color = Date), size = 0.1)+
  #start point
  geom_point(f_data%>%filter(Event_Type == 'Encounter START' & Encounter_Type == 'Initial'), mapping = aes(Longitude, Latitude, color = Date, shape = Species), fill = "red", size = 1.5, stroke = 1.5)+
  theme_bw()+
  date_color_scale+
  species_shape_scale+
  xlab("Longitude")+
  ylab("")+
  theme(axis.text.y=element_blank())+
  theme(legend.position = "bottom",
        legend.title = element_text( size=5), 
        legend.text=element_text(size=5),
        legend.key.height = unit(0.3, "cm"),
        legend.background=element_blank(),
        axis.text.x = element_text(angle = 90),
        plot.background = element_blank())+
  guides(colour = "none")
  
legend_sig<-cowplot::get_legend(sigmap)

sigmap<-sigmap+theme(legend.position = "none")

  print("mapping")
  
  if (pharea == "Dusky"){

    coords_dusky<-coord_sf(xlim = c(166.45,167.0), ylim = c(-45.81,-45.49), crs = 4269)

    effmap<-effmap+
      coords_dusky+
      scale_y_continuous(breaks = seq(-45.8,-45.5, by = 0.1)) +
      scale_x_continuous(breaks = seq(166.5, 167.0, by = 0.1))

    sigmap<-sigmap+
      coords_dusky+
      scale_y_continuous(breaks = seq(-45.8,-45.5, by = 0.1)) +
      scale_x_continuous(breaks = seq(166.5, 167.0, by = 0.1))

    h = 100
    w1 = 1.15
  } else if (pharea == "Doubtful"){
    effmap<-effmap+
      coord_sf(xlim = c(166.8,167.2), ylim = c(-45.5,-45.15), crs = 4269)
    sigmap<-sigmap+
      coord_sf(xlim = c(166.8,167.2), ylim = c(-45.5,-45.15), crs = 4269)
    h = 100
    w1 = 1.15
  } else {
    effmap<-effmap+
      coord_sf(xlim = c(min(f_data$Longitude)-0.2,max(f_data$Longitude)), ylim = c(min(f_data$Latitude),max(f_data$Latitude)), crs = 4269)
    sigmap<-sigmap+
      coord_sf(xlim = c(min(f_data$Longitude)-0.2,max(f_data$Longitude)), ylim = c(min(f_data$Latitude),max(f_data$Latitude)), crs = 4269)
    h = 100
    w1 = 2
  
  }
  
  map<-cowplot::plot_grid(effmap, sigmap, labels = "auto", rel_widths = c(1.13,1))
  legend<-cowplot::plot_grid(legend, ncol = 1, rel_heights = c(.1))
  legend_sig<-cowplot::plot_grid(legend_sig, ncol = 1, rel_heights = c(.1))
  
  ggsave(filename = 'map.png',map,device = 'png', './figures', dpi = 320, width = 169, height = 115, units = 'mm')
  ggsave(filename = 'legend.png',legend,device = 'png', './figures', dpi = 320, width = 169, height = 10, units = 'mm')
  ggsave(filename = 'legend_sig.png',legend_sig,device = 'png', './figures', dpi = 320, width = 75, height = 30, units = 'mm')
 
 incProgress(5/5)
 print("Done")
 enable("report")

 # download report ----
 output$report<-downloadHandler(
   
   filename = paste0(pharea,"_FBD_monitoring_report_",phyear,"_",phmonth,".pdf"),
   
   content = function(file) {
     
     if(pharea == "Other" & Tt_ID == "yes"){
      tempReport<-file.path("./scripts/FBD summary template_other.Rmd")
      file.copy("FBD summary template_other.Rmd", tempReport, overwrite = FALSE) 
     } else if (pharea == "Other" & Tt_ID == "no"){
         tempReport<-file.path("./scripts/FBD summary template_other_no dolphin.Rmd")
         file.copy("FBD summary template_other_no dolphin.Rmd", tempReport, overwrite = FALSE)
     } else {
      tempReport<-file.path("./scripts/FBD summary template.Rmd")
      file.copy("FBD summary template.Rmd", tempReport, overwrite = FALSE)
     }
     
      tripdate_s<-format(min(ymd(onoffeffort$DATE)), "%d %b %Y")
      tripdate_e<-format(max(ymd(onoffeffort$DATE)), "%d %b %Y")
      loc_base<-paste0(pharea,"/",input$locbase)
      if(grepl("Doubtful",loc_base) == TRUE){
        loc_base<-paste0("Patea-",pharea," complex/",input$locbase)
      } else if (grepl("Dusky", loc_base) == TRUE){
        loc_base<-paste0("Tamatea-",pharea," complex/",input$locbase)
      } else {
        print(ofiords)
        print(as.list(ofiords))
        loc_base<-stringi::stri_replace_last(do.call("paste", c(as.list(ofiords), sep = ", ")), fixed = ",", ", and")
        loc_base<-paste0(loc_base,"/",input$locbase)
      }
      nsurveydays<-nrow(f_data%>%distinct(Date))
      
      if(Tt_ID == "no"){
        recent = NA
        older = NA 
        photo_n = NA
        trip_cap = NA
        hours_wTt = NA
        age_sex_table = NA
        unseen_table = NA
        unseen_names = NA
      }
      
      vessel<-input$vessel
      if(grepl('Southern Winds',loc_base) == TRUE & vessel != "Southern Winds"){
        vessel<-paste0(vessel,"/Southern Winds")
      }
      crew<-stringr::str_c(input$crew, "\\linebreak", collapse = " ")
      crew<-substr(crew,1,nchar(crew)-10)
      wx_comments<-input$wx_comments
      calf_comments<-input$calf_comments
      next_comments<-input$next_comments
      if (Tt_ID == "yes") {
        pop_est <- read.csv('./data/FBD_popest.csv', header = T) %>%
          filter(Year == max(Year)) %>%
          filter(Pod == pharea) %>%
          mutate(popsent = paste0(Year, ": ", Est, " (95% CI = ", lcl, "--", ucl, ")"))
      } else {
        pop_est = NA
      }
      params<-list( tripdate_s = tripdate_s, tripdate_e = tripdate_e, loc_base = loc_base, 
                    nsurveydays = nsurveydays, recent = recent, older = older, photo_n = photo_n,
                    vessel = vessel, crew = crew, pop_est = pop_est, trip_cap = trip_cap,
                    track_dist = track_dist, sig_days = sig_days, sig_count = sig_count, hours_wTt = hours_wTt, 
                    age_sex_table = age_sex_table, unseen_table = unseen_table, unseen_names = unseen_names,
                    wx_comments = wx_comments, calf_comments = calf_comments, next_comments = next_comments,
                    pharea = pharea)
      print(params)
      rmarkdown::render(tempReport, output_file = file,
                   params = params,
                   envir = new.env(parent = globalenv()))
   })
  }
  
    
    })
})
  
  