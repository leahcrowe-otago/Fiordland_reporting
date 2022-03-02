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
           across(where(is.character), ~na_if(., "")))%>% # add in NAs where there are blanks
    #group_by(DATE)%>%
    #mutate(Event = case_when(Effort == 'Effort ON' ~ 1, TRUE ~ 0))%>%
    #ungroup()%>%
    arrange(Datetime)%>%
    mutate(Effort = case_when(
      Home.screen == "Encounter START" & grepl('^FOLLOW', toupper(Note)) ~ 'Follow ON',
      Home.screen == "Encounter END & DATA" & grepl('^FOLLOW', toupper(Note)) ~ 'Follow OFF',
      Home.screen == "Encounter START" & grepl('^REPEAT', toupper(Note)) ~ 'Repeat Encounter ON',
      Home.screen == "Encounter END & DATA" & grepl('^REPEAT', toupper(Note)) ~ 'Repeat Encounter OFF',
      Home.screen == "Encounter START" ~ 'Encounter ON',
      Home.screen == "Encounter END & DATA" ~ 'Encounter OFF',
      TRUE ~ Effort
    ),
    Permit = case_when(
      grepl('DOC', Note) ~ 'DOC',
      grepl('UO', Note) ~ 'UO',
      TRUE ~ ''
    ))

  f_data%>%filter(DATE == '2021-10-21' & grepl('13:34:05',TIME))
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
    filter(grepl("Encounter", Effort) | grepl("Follow", Effort))%>%
    mutate(signum = rep(1:(n()/2), each = 2))%>%
    dplyr::select(Datetime, DATE, signum, Home.screen, Effort, Permit)
  
  f_data$Effort[f_data$Effort==""] <- NA
  
  f_data<-f_data%>%
    left_join(sig_num, by = c("Datetime", "DATE","Home.screen","Effort","Permit"))%>% 
    group_by(grp = cumsum(!is.na(Effort))) %>% 
    mutate(Effort = replace(Effort, first(Effort) == 'Encounter ON', 'Encounter ON')) %>% 
    mutate(Effort = replace(Effort, first(Effort) == 'Repeat Encounter ON', 'Repeat Encounter ON')) %>% 
    mutate(Effort = replace(Effort, first(Effort) == 'Follow ON', 'Follow ON')) %>% 
    ungroup() %>%
    dplyr::select(Datetime, DATE, TIME, LAT, LON, Effort, Crew, signum,
                  Species.encountered, Group.size, No..of.calves, Behaviour.state, Permit,
                  Beaufort, Swell, Sighting.conditions, SST, Depth, Note, Event, -grp)
     
  sig_count<-nrow(f_data%>%filter(Effort == 'Encounter OFF' & !is.na(signum)))
  
  sig_days<-sig_num%>%
    distinct(DATE)
  
  write.csv(f_data, paste0(pathimage,"/f_data_",phyear,"_",phmonth,".csv"), row.names = F, na = "")
  print(nrow(f_data))
  
  incProgress(3/5)
  #convert meters to nm
  m_nm<-1/1852
  
  f_data_dist<-f_data%>%
    arrange(Datetime)%>%
    group_by(DATE,Event)%>%
    mutate(LAT2 = dplyr::lag(LAT),
           LON2 = dplyr::lag(LON),
           dist_km = geosphere::distVincentyEllipsoid(matrix(c(LON,LAT), ncol = 2),matrix(c(LON2, LAT2), ncol =2),a=6378137, f=1/298.257222101)*m_nm)
  
  print(f_data_dist%>%filter(dist_km > 0.1)%>%as.data.frame())
  track_dist<-round(sum(f_data_dist$dist_km, na.rm=TRUE),0)
  
  sig_num$Permit[sig_num$Permit==""] <- NA
  sig_num$Permit<-zoo::na.locf(sig_num$Permit, na.rm = FALSE)
  
  onoffsigs<-sig_num%>%
    dplyr::select(signum, Datetime, Effort, Permit)%>%
    group_by(signum)%>%
    tidyr::pivot_wider(names_from = Effort, values_from = Datetime)
  
  hours_wTt<-onoffsigs%>%
    mutate(time_wTt = as.numeric(difftime(`Encounter OFF`, `Encounter ON`, units = "mins")),
           time_wTt_r = as.numeric(difftime(`Repeat Encounter OFF`, `Repeat Encounter ON`, units = "mins")),
           time_follow = as.numeric(difftime(`Follow OFF`, `Follow ON`, units = "mins")))%>%
    ungroup()%>%
    group_by(Permit)%>%
    dplyr::summarise(total_wTt_e = round(sum(time_wTt, na.rm=TRUE)/60,1),
                     total_wTt_r = round(sum(time_wTt_r, na.rm=TRUE)/60,1),
                     total_follow = round(sum(time_follow, na.rm=TRUE)/60,1))%>%
    mutate(total_wTt = total_wTt_e + total_wTt_r)%>%
    dplyr::select(-total_wTt_e, -total_wTt_r)
  
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
  nrow(PA_merge[[2]])
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
    sigminmax<-f_data%>%filter(!is.na(signum))%>%
      group_by(signum)%>%
      mutate(max = max(Datetime),
             min = min(Datetime))%>%
      distinct(DATE, signum, min, max)
    
    sigminmax$DATE<-ymd(sigminmax$DATE)
    
    if('Group' %in% allmerge_dt){
      print('group yes')
    allmerge_dt<-allmerge_dt%>%dplyr::select(-Group)
    }
    
    allmerge_dt_group<-allmerge_dt%>%
      left_join(sigminmax, by = c('Date' = 'DATE'))%>%
      mutate(Group = case_when(
        Datetime >= min & Datetime <= max ~ signum
      ))%>%
      filter(!is.na(Group))%>%
      dplyr::select(-signum, -max, -min)%>%
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
    distinct(DATE)%>%
    mutate(value = 0)%>%
    tidyr::pivot_wider(names_from = "DATE", values_from = value)
  
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
    #geom_col(mapping = aes(x = date, y = ni, color = permit), alpha = 0.5)+
    #geom_point(alpha = 0.8, size = 3, mapping = aes(color = permit))+
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
  caphist<-read.csv('./data/Dusky date capture history 2007-2021.csv', header = T, stringsAsFactors = F)}
  else if (pharea == 'Doubtful'){
  caphist<-read.csv('./data/Doubtful date capture history 1990-Jan2021.csv', header = T, stringsAsFactors = F)}
  
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
  geom_point(f_data%>%arrange(Datetime), mapping = aes(LON, LAT, group = DATE, color = DATE), size = 0.1)+
  #geom_path(f_data%>%arrange(Datetime), mapping = aes(LON, LAT, group = DATE, color = DATE))+
  theme_bw()+
  scale_color_viridis_d(name = "Date")+
  xlab("Longitude")+
  ylab("")+
  theme(legend.position = "none",
        axis.text.y=element_blank())

sigmap<-ggplot()+
  geom_polygon(NZ_coast, mapping = aes(long,lat,group = group), alpha = 0.8)+
  geom_point(f_data%>%filter(grepl("Encounter", Effort) | grepl("Follow", Effort)), mapping = aes(LON, LAT, color = DATE), size = 0.1)+
  geom_point(f_data%>%filter(Effort == "Encounter ON" & !is.na(signum)), mapping = aes(LON, LAT, color = DATE), shape = 23, fill = "red", size = 1.5, stroke = 1.5)+
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
  
  