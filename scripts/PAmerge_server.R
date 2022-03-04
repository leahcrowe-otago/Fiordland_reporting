disable("report")
crew.csv<-read.csv('./data/FBD_crew.csv')%>%mutate(creworg = paste0(FIRST,' ',LAST,', ',ORGANIZATION))
crew_list<-crew.csv$creworg
output$crew<-renderUI({
  pickerInput("crew","Crew", choices = c(crew_list), multiple = T, inline = F)
})

observeEvent(input$photogo,{
  
  withProgress(message = 'Processing data and compiling report...', value = 0, {
  
  output$finalmess<-renderText({""})
  
  phserv<-input$filepathway
  phyear<-input$photoyear
  phmonth<-input$photomonth
  pharea<-input$areainput
  
  #override<-NULL
  #print(phserv)
  #print(phyear)
  #print(phfile)

  if(pharea == "Other"){
    phareafile = 'Other Fiords/'
  } else {
    # pharea = "Doubtful"
    # phyear = 2022
    # phmonth = '01'
    phareafile = paste0(pharea,' Sound Dolphin Monitoring/')
  }
  
    if(phserv == 'Network'){
      pathway<-paste0('//storage.hcs-p01.otago.ac.nz/sci-marine-mammal-backup/Fiordland bottlenose dolphin/Long Term Monitoring/')
      } else if (phserv == 'Local'){
      pathway<-input$filepathinput
      }
  
  pathimage<-paste0(pathway,phareafile,phyear,'/',phyear,'_',phmonth)
  print(pathimage)
  
  ##########################
  ## Sightings and tracks ##
  ##########################
  print("sigs and trackline")
  
  sigs<-read.csv(paste0(pathimage,"/Sightings/f_",phyear,"_",phmonth," CyberTracker.csv"), header = T, stringsAsFactors = F)
  sigs<-sigs%>%mutate(Datetime = dmy_hms(paste(Date, Time)))%>%
    dplyr::select(Datetime, everything())
  sigs$Date<-dmy(sigs$Date)
  
  daterange<-range(sigs$Datetime)
  
  raw_tracks<-sf::st_read(paste0(pathimage,"/Tracks"), layer = paste0(phyear,"_",phmonth), quiet = T)

  tracks<-as.data.frame(raw_tracks)%>%
    dplyr::select(DATE, TIME, LATITUDE, LONGITUDE)%>%
    mutate(Datetime = ymd_hms(paste(DATE, TIME)))%>%
    #filter(Datetime >= ymd_hms('2021-09-30 10:59:00') & Datetime <= ymd_hms('2021-09-30 11:20:00'))%>%
    dplyr::select(Datetime, everything())%>%
    arrange(Datetime)%>%
    distinct()%>%
    group_by(Datetime)%>%
    mutate(rank = rank(Datetime, ties.method = "first"))%>%
    filter(rank == 1)%>%
    dplyr::select(-rank)
  print(nrow(tracks))
  #find nearest position for time changes

  data.table::setDT(sigs)[,  Latitude := data.table::setDT(tracks)[sigs, LATITUDE, on = "Datetime", roll = "nearest"]]
  data.table::setDT(sigs)[,  Longitude := data.table::setDT(tracks)[sigs, LONGITUDE, on = "Datetime", roll = "nearest"]]
  
  sigs%>%as.data.frame()
  #filter tracks to only between on/off effort      
  f_data<-tracks%>%
    full_join(sigs, by = c("Datetime","LATITUDE"="Latitude","LONGITUDE"="Longitude","DATE"="Date","TIME"="Time"))%>%
    mutate(DATE = as.factor(DATE),
           across(where(is.character), ~na_if(., "")))#%>% # add in NAs where there are blanks
    #group_by(DATE)%>%
    #mutate(Event = case_when(Effort == 'Effort ON' ~ 1, TRUE ~ 0))%>%
    #ungroup()%>%
    # arrange(Datetime)%>%
    # mutate(Effort = case_when(
    #   Event_type == "Encounter START" & grepl('^FOLLOW', toupper(Note)) ~ 'Follow ON',
    #   Event_type == "Encounter END & DATA" & grepl('^FOLLOW', toupper(Note)) ~ 'Follow OFF',
    #   Event_type == "Encounter START" & grepl('^REPEAT', toupper(Note)) ~ 'Repeat Encounter ON',
    #   Event_type == "Encounter END & DATA" & grepl('^REPEAT', toupper(Note)) ~ 'Repeat Encounter OFF',
    #   Event_type == "Encounter START" ~ 'Encounter ON',
    #   Event_type == "Encounter END & DATA" ~ 'Encounter OFF',
    #   TRUE ~ Effort
    # ),
    # Permit = case_when(
    #   grepl('DOC', Note) ~ 'DOC',
    #   grepl('UO', Note) ~ 'UO',
    #   TRUE ~ ''
    # ))

  #f_data%>%filter(DATE == '2021-10-21' & grepl('13:34:05',TIME))
  Event<-f_data%>%filter(Effort == "Effort ON")%>%ungroup()%>%mutate(Event = 1:n())%>%as.data.frame()
  
  onoffeffort<-f_data%>%
    filter(grepl('Effort',Effort))%>%
    group_by(DATE)%>%
    mutate(min = min(Datetime),
      max = max(Datetime))%>%
    distinct(DATE, min, max)%>%
    as.data.frame()
  
  f_data<-f_data%>%
    left_join(onoffeffort, by = 'DATE')%>%
    filter(Datetime >= min & Datetime <= max)%>%
    left_join(Event)
  
  f_data$Event<-zoo::na.locf(f_data$Event, na.rm = FALSE)
  
  sig_num<-f_data%>%
    filter(grepl("Encounter", Event_Type))%>%
    mutate(signum = rep(1:(n()/2), each = 2))%>%
    dplyr::select(Datetime, DATE, signum, Effort, Event_Type, Encounter_Type, Permit)
  
  f_data$Effort[f_data$Effort==""] <- NA
  f_data$Encounter_Type[f_data$Encounter_Type==""] <- NA
  
  f_data<-f_data%>%dplyr::select(-Encounter_Type)%>%
    left_join(sig_num, by = c("Datetime", "DATE","Event_Type", "Effort","Permit"))%>% 
    group_by(grp = cumsum(!is.na(signum))) %>% #group by Encounter_Type downfill from START to END
    mutate(Encounter_Type = replace(Encounter_Type, first(Encounter_Type) == 'Initial', 'Initial')) %>% 
    mutate(Encounter_Type = replace(Encounter_Type, first(Encounter_Type) == 'Repeat', 'Repeat')) %>% 
    mutate(Encounter_Type = replace(Encounter_Type, first(Encounter_Type) == 'Follow', 'Follow')) %>% 
    ungroup() %>%
    dplyr::select(Datetime, DATE, TIME, LATITUDE, LONGITUDE, Effort, Event_Type, Encounter_Type, Crew, signum,
                  Species, Group_Size, Calves, Behaviours, Permit,
                  Beaufort, Swell, Glare, Visibility, Overall, SST, Depth, Note, Event, Tawaki, -grp)%>%
    dplyr::rename(Date = DATE, Time = TIME, Latitude = LATITUDE, Longitude = LONGITUDE, Sighting_Number = signum)
     
  sig_count<-max(f_data$Sighting_Number)
  
  sig_days<-sig_num%>%
    distinct(DATE)
  
  write.csv(f_data, paste0(pathimage,"/f_data_",phyear,"_",phmonth,".csv"), row.names = F, na = "")
  write.csv(f_data%>%filter(!is.na(Tawaki))%>%dplyr::select(Datetime, Date, Time, Latitude, Longitude,Tawaki,Note), paste0(pathimage,"/Tawaki_",phyear,"_",phmonth,".csv"), row.names = F, na = "")
  print(nrow(f_data))
  
  incProgress(3/5)
  #convert meters to nm
  m_nm<-1/1852
  
  f_data_dist<-f_data%>%
    arrange(Datetime)%>%
    group_by(Date,Event)%>%
    mutate(LAT2 = dplyr::lag(Latitude),
           LON2 = dplyr::lag(Longitude),
           dist_km = geosphere::distVincentyEllipsoid(matrix(c(Longitude,Latitude), ncol = 2),matrix(c(LON2, LAT2), ncol =2),a=6378137, f=1/298.257222101)*m_nm)
  
  print(f_data_dist%>%filter(dist_km > 0.1)%>%as.data.frame())
  track_dist<-round(sum(f_data_dist$dist_km, na.rm=TRUE),0)
  
  sig_num$Permit[sig_num$Permit==""] <- NA
  sig_num$Permit<-zoo::na.locf(sig_num$Permit, na.rm = FALSE)
  sig_num$Encounter_Type<-zoo::na.locf(sig_num$Encounter_Type, na.rm = FALSE)
  
  onoffsigs<-sig_num%>%
    dplyr::select(signum, Datetime, Encounter_Type, Event_Type, Permit)%>%
    group_by(signum, Encounter_Type)%>%
    tidyr::pivot_wider(names_from = Event_Type, values_from = Datetime)%>%
    mutate(time_w = as.numeric(difftime(`Encounter END & DATA`,`Encounter START`), units = "mins"))
  
  unique(onoffsigs$Permit)
  
  hours_wTt<-onoffsigs%>%
    group_by(Encounter_Type, Permit)%>%
    dplyr::summarise(Total_time = sum(time_w, na.rm=TRUE))%>%
    mutate(distance = case_when(
      Encounter_Type == 'Follow' ~ '100–400',
      TRUE ~ '<100'))%>%
    ungroup()%>%
    group_by(Permit, distance)%>%
    dplyr::summarise(Total_time_dist = round(sum(Total_time, na.rm = TRUE)/60,1))%>%
    ungroup()%>%
    tidyr::pivot_wider(distance, names_from = Permit, values_from = Total_time_dist)%>%
    mutate(total_wTt = DOC + Otago)
  
  ####################
  ## Photo analysis ##
  ####################
  
  if (dir.exists(pathimage) == FALSE){
    output$error<-renderText({"Double check 'Year', 'Month', and 'Area monitored' -- files cannot be found."})
  } else {
    output$error<-renderText({""})
    
   if (input$EXIF == "collect"){  
    print("merging PA")
  ##merge all PA excel spreadsheets
  PA_xlsx<-list.files(paste0(pathimage,"/Photo analysis"), pattern = "*.xlsx", full.names = T, all.files = F)
  #exclude weird hidden excel files
  PA_xlsx<-grep(PA_xlsx, pattern = "[~]", invert = T, value = T)
  
  PA_merge<-lapply(PA_xlsx, function (x) readxl::read_excel(x, sheet = 1, col_names = T, guess_max = 1000))
  nrow(PA_merge[[1]])
  #nrow(PA_merge[[2]])
  allmerge<-do.call(rbind, PA_merge)
  nrow(allmerge)
  allmerge$Date<-ymd(allmerge$Date)
  allmerge<-allmerge%>%
    mutate(ID_Name = case_when(
      is.na(ID_Name) ~ 'UNMA',
      TRUE ~ ID_Name))%>%
    filter(ID_Name != "CULL")
  nrow(allmerge)
  
  #list folders that start with the year
  folder.list<-list.files(pathimage, pattern = paste0("^",phyear), full.names = T)
  filenames<-sapply(folder.list, function (x) list.files(x, pattern = "*.JPG$|*.jpg$", full.names = T, recursive = T))
  filenames_unlist<-unlist(filenames, use.names = F)
  
  allphotod_df<-data.frame(fullfilename = filenames_unlist,
             filename = basename(filenames_unlist),
             date = ymd(str_extract(filenames_unlist,'\\b\\d{8}\\b')))

  #allphotod_df%>%filter(grepl('DSC',filename))
  
  PA_fn_error<-allphotod_df%>%
    right_join(allmerge, by = c("filename" = "Filename", "date" = "Date"))%>%
    filter(is.na(fullfilename))

  if (nrow(PA_fn_error) > 0){
    #return an error message before proceeding
    #and print the table
  }
  
#allmerge%>%filter(Date == '2021-10-02')%>%filter(Filename == 'DSC_3171')

  files_for_exif<-allphotod_df%>%
    mutate(filename = stringr::str_sub(filename, end = -5))%>%
    right_join(allmerge, by = c("filename" = "Filename", "date" = "Date"))%>%
    distinct(fullfilename)%>%
    filter(!is.na(fullfilename))
  
  incProgress(1/5)
  print("getting exif data")
  #get exif data
  metadata<-exifr::read_exif(files_for_exif$fullfilename, tags = c("filename", "DateTimeOriginal"))
  print(metadata)
  ##
  metadata2<-metadata%>%
    mutate(Filename = stringr::str_sub(basename(FileName), end = -5),
           Datetime = ymd_hms(DateTimeOriginal),
           Date = as.Date(Datetime))%>%
    dplyr::select(Filename, Datetime, Date)
  nrow(metadata2)
  
  allmerge_dt<-allmerge%>%
    left_join(metadata2, by = c("Filename", "Date"))%>%
    mutate(ID_Name = case_when(grepl("EEK", ID_Name) ~ "EEK-THE-CAT",
                               grepl("JOLLY", ID_Name) ~ "JOLLY-GOOD",
                               TRUE ~ ID_Name),
           Survey_Area = toupper(pharea))
  
  incProgress(2/5)
  
  } else if (input$EXIF == "load"){
    incProgress(2/5)
    print("load exif")
    allmerge_dt<-read.csv(paste0(pathimage,"/Photo analysis/f_PA_",phyear,"_",phmonth,".csv"), header = T, stringsAsFactors = T)
    allmerge_dt$Datetime<-ymd_hms(allmerge_dt$Datetime)
    allmerge_dt$Date<-ymd(allmerge_dt$Date)
    print(nrow(allmerge_dt))
    }
  
    ##add group # to match sighting number in final data
    sigminmax<-f_data%>%filter(!is.na(Sighting_Number))%>%
      group_by(Sighting_Number)%>%
      mutate(max = max(Datetime),
             min = min(Datetime))%>%
      distinct(Date, Sighting_Number, min, max)
    
    sigminmax$Date<-ymd(sigminmax$Date)
    
    if('Group' %in% allmerge_dt){
      print('group yes')
    allmerge_dt<-allmerge_dt%>%dplyr::select(-Group)
    }
    
    allmerge_dt_group<-allmerge_dt%>%
      left_join(sigminmax, by = "Date")%>%
      mutate(Group = case_when(
        Datetime >= min & Datetime <= max ~ Sighting_Number
      ))%>%
      filter(!is.na(Group))%>%
      dplyr::select(-Sighting_Number, -max, -min)%>%
      distinct()
    
    nogroup<-allmerge_dt%>%
      anti_join(allmerge_dt_group)
    
    allmerge_dt_group<-allmerge_dt_group%>%
      bind_rows(nogroup)%>%
      arrange(Datetime)
    
    write.csv(allmerge_dt_group, paste0(pathimage,"/Photo analysis/f_PA_",phyear,"_",phmonth,".csv"), row.names = F, na = "")  
    
  recent<-format(min(as.Date(allmerge_dt$Date)) - lubridate::years(1), "%d %b %Y")
  older<-format(min(as.Date(allmerge_dt$Date)) - lubridate::years(2), "%d %b %Y")

  ##################
  ## life history ##
  ##################
  
  #fiordland_bottlenose.age_sex view
  lifehist<-read.csv('./data/FBD_lifehistory.csv', header = T, stringsAsFactors = F)
  
  photo_counts<-allmerge_dt%>%
    filter(!grepl("\\?",ID_Name))%>%
    filter(ID_Name != "CALF")%>%
    group_by(Date, ID_Name)%>%
    tally()
  
  print('259')
  daily_cap<-photo_counts%>%
    ungroup()%>%
    mutate_if(is.numeric, ~1 * (. > 0))%>%
    left_join(lifehist, by = c("ID_Name" = "NAME"))%>%
    dplyr::rename("NAME" = "ID_Name")%>%
    filter(NAME != "" & !grepl('_',NAME) & !grepl('CULL',NAME) & NAME != "UNMA" & !str_detect(NAME, "^UK") & !str_detect(NAME, "\\?"))%>%
    tidyr::pivot_wider(id_cols = c("NAME","SEX","BIRTH_YEAR","FIRST_YEAR"), names_from = "Date", values_from = "n")%>%
    arrange(NAME)%>%
    mutate(year = as.numeric(phyear))%>%
    mutate(AgeClass = case_when(
      (year - FIRST_YEAR) > 3 ~ 'A',
      (year - BIRTH_YEAR) < 1 ~ 'C',
      BIRTH_YEAR != 0 & (year - BIRTH_YEAR) <= 3 & (year - BIRTH_YEAR) > 0 ~ 'J',
      TRUE ~ 'U'
    ))%>%
    mutate(SEX=replace(SEX, is.na(SEX), 'X'))
  
  trip_cap<-daily_cap%>%
    distinct(NAME,SEX,AgeClass)
  
  print('277')
  age_sex_table<-trip_cap%>%
    dplyr::select(-NAME)%>%
    group_by(SEX, AgeClass)%>%
    tally()%>%
    tidyr::pivot_wider(names_from = SEX, values_from = n)%>%
    arrange(factor(AgeClass, levels = c("A","J","C","U")))%>%
    replace(is.na(.), 0)%>%
    mutate(AgeClass = case_when(
      AgeClass == 'A' ~ "Adult",
      AgeClass == 'J' ~ "Juvenile",
      AgeClass == 'C' ~ "Calf*",
      AgeClass == 'U' ~ "Unknown"
    ))
  
  if (!"M" %in% colnames(age_sex_table)){
    age_sex_table<-age_sex_table%>%
      mutate(M = 0)}
  
  if (!"F" %in% colnames(age_sex_table)){
    age_sex_table<-age_sex_table%>%
      mutate(F = 0)}
  
  if (!"X" %in% colnames(age_sex_table)){
    age_sex_table<-age_sex_table%>%
      mutate(X = 0)}
  
  allsampledays<-f_data%>%
    distinct(Date)%>%
    mutate(value = 0)%>%
    tidyr::pivot_wider(names_from = "Date", values_from = value)
  
  disco<-function(x){
    x<-daily_cap
    freqcap<-x%>%
      dplyr::select(-NAME, -SEX, -BIRTH_YEAR,-FIRST_YEAR,-AgeClass, -year)%>%
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
  
  onoffeffort$DATE<-ymd(onoffeffort$DATE)
  
  disco_data<-disco_data%>%
    left_join(onoffeffort, by = c('date' = 'DATE'))%>%
    dplyr::select(-min, -max)
  
  disco_curve<-ggplot(disco_data, aes(x=date, y = disc))+
    geom_line()+
    #geom_col(mapping = aes(x = date, y = ni, color = Permit), alpha = 0.5)+
    #geom_point(alpha = 0.8, size = 3, mapping = aes(color = Permit))+
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
  
  #######################
  ## Sightings history ##
  #######################
  
  #this section is a place holder until the database is complete
  ##Capture history from Excel spreadsheet##
  
  if (pharea == 'Dusky'){
  caphist<-read.csv('./data/Dusky date capture history 2007-2021.csv', header = T, stringsAsFactors = F)
  } else if (pharea == 'Doubtful'){
  caphist<-read.csv('./data/Doubtful date capture history 1990-Jan2021.csv', header = T, stringsAsFactors = F)
  }
  
  thistrip<-daily_cap%>%
    dplyr::select(-SEX, -BIRTH_YEAR, -FIRST_YEAR, -AgeClass, -year)%>%
    tidyr::pivot_longer(!(c(NAME)),names_to = "Date")%>%
    mutate(Date = lubridate::ymd(Date))%>%
    filter(value != 0)
  
  thistrip_names<-thistrip%>%distinct(NAME)%>%as.data.frame()
  
  print('365')
  all_first_last<-caphist%>%
    tidyr::pivot_longer(!(c(Entry,NAME)),names_to = "Date")%>%
    filter(value != 0)%>%
    mutate(Date = lubridate::dmy(substr(str_replace_all(Date,"\\.","-"),2,9)))%>%
    dplyr::select(NAME, Date, value)%>%
    bind_rows(thistrip)%>%
    group_by(NAME)%>%
    mutate(first_date = min(Date),
           last_date = max(Date),
           NAME = toupper(NAME))%>%
    distinct(NAME,first_date,last_date)%>%
    filter(!is.na(NAME))%>%
    arrange(NAME)%>%
    left_join(lifehist, by = "NAME")%>%
    mutate(year = as.numeric(phyear))%>%
    mutate(AgeClass = case_when(
      year - FIRST_YEAR > 3 ~ "A",
      year - BIRTH_YEAR < 1 ~ "C",
      !is.na(BIRTH_YEAR) & year - BIRTH_YEAR <= 3 & year - BIRTH_YEAR > 0 ~ "J",
      TRUE ~ "U"
    ))
  
  all_first_last$DEATH_YEAR<-as.numeric(all_first_last$DEATH_YEAR)
  
  unseen_two_years<-all_first_last%>%
    anti_join(thistrip_names)%>%
    filter(is.na(DEATH_YEAR))%>%
    filter(last_date < max(as.Date(f_data$DATE)) - lubridate::years(1) & last_date >= max(as.Date(f_data$DATE)) - lubridate::years(2))%>%
    mutate(LAST_YEAR = lubridate::year(last_date))%>%
    mutate(NameSex = case_when(
      nchar(NAME) > 2 ~ paste0(str_to_title(NAME)," (",SEX,")"),
      nchar(NAME) <= 2 ~ paste0(NAME," (",SEX,")")))
  
  unseen_names<-unseen_two_years$NAME
  print("392")
  
  unseen_table<-unseen_two_years%>%
    group_by(LAST_YEAR, SEX, AgeClass)%>%
    tally()%>%
    tidyr::pivot_wider(names_from = SEX, values_from = n)%>%
    arrange(factor(AgeClass, levels = c("A","J","C")))%>%
    replace(is.na(.), 0)%>%
    mutate(AgeClass = case_when(
      AgeClass == 'A' ~ "Adult",
      AgeClass == 'J' ~ "Juvenile",
      AgeClass == 'C' ~ "Calf*",
      AgeClass == 'U' ~ "Unknown*"
    ))
  
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
  
  #########
  ## MAP ##
  #########
  
  NZ_coast<-readOGR("./shapefiles", layer = "nz-coastlines-and-islands-polygons-topo-1500k")
  
incProgress(4/5)

effmap<-ggplot()+
  geom_polygon(NZ_coast, mapping = aes(long,lat,group = group), alpha = 0.8)+
  geom_point(f_data%>%arrange(Datetime), mapping = aes(Longitude, Latitude, group = Date, color = Date), size = 0.1)+
  #geom_path(f_data%>%arrange(Datetime), mapping = aes(LON, LAT, group = DATE, color = DATE))+
  theme_bw()+
  scale_color_viridis_d(name = "Date")+
  #xlab("Longitude")+
  ylab("")+
  theme(legend.position = "none",
        axis.text.y=element_blank())

sigmap<-ggplot()+
  geom_polygon(NZ_coast, mapping = aes(long,lat,group = group), alpha = 0.8)+
  #path
  geom_point(f_data%>%filter(!is.na(Encounter_Type)), mapping = aes(Longitude, Latitude, color = Date), size = 0.1)+
  #start point
  geom_point(f_data%>%filter(Event_Type == 'Encounter START'), mapping = aes(Longitude, Latitude, color = Date), shape = 23, fill = "red", size = 1.5, stroke = 1.5)+
  theme_bw()+
  scale_color_viridis_d(name = "Date")+
  xlab("Longitude")+
  ylab("Latitude")
  
  print("mapping")
  
  if (pharea == "Dusky"){
    effmap<-effmap+
      coord_sf(xlim = c(166.45,167.0), ylim = c(-45.81,-45.49), crs = 4269)
    sigmap<-sigmap+
      coord_sf(xlim = c(166.45,167.0), ylim = c(-45.81,-45.49), crs = 4269)
    h = 100
  } else if (pharea == "Doubtful"){
    effmap<-effmap+
      coord_sf(xlim = c(166.8,167.2), ylim = c(-45.5,-45.15), crs = 4269)
    sigmap<-sigmap+
      coord_sf(xlim = c(166.8,167.2), ylim = c(-45.5,-45.15), crs = 4269)
    h = 100
  } else {
    effmap<-effmap+
      coord_sf(xlim = c(min(f_data$LON),max(f_data$LON)), ylim = c(min(f_data$LAT),max(f_data$LAT)), crs = 4269)
    sigmap<-sigmap+
      coord_sf(xlim = c(min(f_data$LON),max(f_data$LON)), ylim = c(min(f_data$LAT),max(f_data$LAT)), crs = 4269)
  }
  
  map<-ggpubr::ggarrange(sigmap, effmap, common.legend = T, legend = "bottom", widths = c(1.15,1), labels = 'auto')
  
  ggsave(filename = 'map.png',map,device = 'png', './figures', dpi = 320, width = 169, height = h, units = 'mm')
  incProgress(5/5)
 print("Done")
 enable("report")

 output$report<-downloadHandler(
   
   filename = paste0("FBD_monitoring_report_",phyear,"_",phmonth,".pdf"),
   
   content = function(file) {
     
      tempReport<-file.path("./scripts/FBD summary template.Rmd")
      file.copy("FBD summary template.Rmd", tempReport, overwrite = FALSE)
 
      tripdate_s<-format(min(onoffeffort$DATE), "%d %b %Y")
      tripdate_e<-format(max(onoffeffort$DATE), "%d %b %Y")
      loc_base<-paste0(pharea,"/",input$locbase)
      nsurveydays<-nrow(f_data%>%distinct(DATE))
      vessel<-input$vessel
      crew<-stringr::str_c(input$crew, "\\linebreak", collapse = " ")
      crew<-substr(crew,1,nchar(crew)-10)
      wx_comments<-input$wx_comments
      calf_comments<-input$calf_comments
      next_comments<-input$next_comments
      
      pop_est<-read.csv('./data/FBD_popest.csv', header = T)%>%
        filter(Year == max(Year))%>%
        filter(Sound == pharea)%>%
        mutate(popsent = paste0(Year,": ",Est, " (95% CI = ",lcl,"--",ucl,")"))
      
      
      params<-list( tripdate_s = tripdate_s, tripdate_e = tripdate_e, loc_base = loc_base, 
                    nsurveydays = nsurveydays, recent = recent, older = older, 
                    vessel = vessel, crew = crew, pop_est = pop_est, trip_cap = trip_cap,
                    track_dist = track_dist, sig_days = sig_days, sig_count = sig_count, hours_wTt = hours_wTt, 
                    age_sex_table = age_sex_table, unseen_table = unseen_table, unseen_names = unseen_names,
                    wx_comments = wx_comments, calf_comments = calf_comments, next_comments = next_comments)
      print(params)
      rmarkdown::render(tempReport, output_file = file,
                   params = params,
                   envir = new.env(parent = globalenv()))
   })
  }
  })
})
  
  