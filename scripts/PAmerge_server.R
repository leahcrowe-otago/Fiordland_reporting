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
    #pharea = "Dusky"
    #phyear = 2021
    #phmonth = '07'
    phareafile = paste0(pharea,' Sound Dolphin Monitoring/')
  }
  
    if(phserv == 'Network'){
      pathway<-paste0('//storage.hcs-p01.otago.ac.nz/sci-marine-mammal-backup/Fiordland bottlenose dolphin/Long Term Monitoring/')
      } else if (phserv == 'Local'){
      pathway<-input$filepathinput
      }
  
  pathimage<-paste0(pathway,phareafile,phyear,'/',phyear,'_',phmonth)
  print(pathimage)
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
  PA_xlsx<-list.files(paste0(pathimage,"/Photo analysis"), pattern = "*.xlsx", full.names = T)
  PA_merge<-lapply(PA_xlsx, function (x) readxl::read_excel(x, sheet = 1, col_names = T, guess_max = 1000))

  allmerge<-do.call(rbind, PA_merge)
  allmerge$Date<-ymd(allmerge$Date)
  allmerge<-allmerge%>%
    mutate(ID_Name = case_when(
      is.na(ID_Name) ~ 'UNMA',
      TRUE ~ ID_Name))%>%
    filter(ID_Name != "CULL")

  #list folders that start with the year
  folder.list<-list.files(pathimage, pattern = paste0("^",phyear), full.names = T)
  filenames<-sapply(folder.list, function (x) list.files(x, pattern = "*.JPG$|*.jpg$", full.names = T, recursive = T))
  filenames_unlist<-unlist(filenames, use.names = F)

  allphotod_df<-data.frame(fullfilename = filenames_unlist,
             filename = stringr::str_sub(filenames_unlist,-12,-5),
             date = ymd(substr(filenames_unlist,150, 157)))
  
  PA_fn_error<-allphotod_df%>%
    right_join(allmerge, by = c("filename" = "Filename", "date" = "Date"))%>%
    filter(is.na(fullfilename))
  
  if (nrow(PA_fn_error) > 0){
    #return an error message before proceeding
    #and print the table
  }
  
  files_for_exif<-allphotod_df%>%
    right_join(allmerge, by = c("filename" = "Filename", "date" = "Date"))%>%
    distinct(fullfilename)%>%
    filter(!is.na(fullfilename))
  incProgress(1/5)
  print("getting exif data")
  #get exif data
  metadata<-exifr::read_exif(files_for_exif$fullfilename, tags = c("filename", "DateTimeOriginal"))
  ##
  metadata2<-metadata%>%
    mutate(Filename = substr(FileName, 1, 8),
           Datetime = ymd_hms(DateTimeOriginal),
           Date = as.Date(Datetime))%>%
    dplyr::select(Filename, Datetime, Date)
  
  allmerge_dt<-allmerge%>%
    left_join(metadata2, by = c("Filename", "Date"))
  
  write.csv(allmerge_dt, paste0(pathimage,"/Photo analysis/f_PA_",phyear,"_",phmonth,".csv"), row.names = F, na = "")
  incProgress(2/5)
  } else if (input$EXIF == "load"){
    incProgress(2/5)
    print("load exif")
    allmerge_dt<-read.csv(paste0(pathimage,"/Photo analysis/f_PA_",phyear,"_",phmonth,".csv"), header = T, stringsAsFactors = T)
  
    }
  
  tripdate_s<-format(min(as.Date(f_data$DATE)), "%d %b %Y")
  tripdate_e<-format(max(as.Date(f_data$DATE)), "%d %b %Y")
  recent<-format(min(as.Date(f_data$DATE)) - lubridate::years(1), "%d %b %Y")
  older<-format(min(as.Date(f_data$DATE)) - lubridate::years(2), "%d %b %Y")
  
  ##################
  ## life history ##
  ##################
  
  lifehist<-read.csv('./data/FBD_lifehistory.csv', header = T, stringsAsFactors = F)

  photo_counts<-allmerge_dt%>%
    filter(!grepl("\\?",ID_Name))%>%
    filter(ID_Name != "CALF")%>%
    mutate(ID_Name = stringr::str_replace(ID_Name, "JOLLY", "JOLLY-GOOD"),
           ID_Name = case_when(grepl("EEK", ID_Name) ~ "EEK-THE-CAT",
                               TRUE ~ ID_Name))%>%
    group_by(Date, ID_Name)%>%
    tally()
  
  daily_cap<-photo_counts%>%
    mutate_if(is.numeric, ~1 * (. > 0))%>%
    left_join(lifehist, by = c("ID_Name" = "NAME"))%>%
    dplyr::rename("NAME" = "ID_Name")%>%
    filter(NAME != "" & !grepl('_',NAME) & !grepl('CULL',NAME) & NAME != "UNMA" & !str_detect(NAME, "^UK") & !str_detect(NAME, "\\?"))%>%
    tidyr::pivot_wider(id_cols = c("NAME","SEX","BIRTH_YEAR","FIRST_YEAR"), names_from = "Date", values_from = "n")%>%
    arrange(NAME)%>%
    mutate(AgeClass = case_when(
      year - FIRST_YEAR > 3 ~ 'A',
      year - BIRTH_YEAR == 0 ~ 'C',
      !is.na(BIRTH_YEAR) & year - BIRTH_YEAR <= 3 & year - BIRTH_YEAR >= 0 ~ 'J',
      TRUE ~ 'U'
    ))
  
  trip_cap<-daily_cap%>%
    distinct(NAME,SEX,AgeClass)
  
  uniqueID<-nrow(trip_cap)
  
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
      AgeClass == 'C' ~ "Calf*"
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
  
  disco<-function(x){
    
    freqcap<-x%>%
      dplyr::select(-NAME, -SEX, -BIRTH_YEAR,-FIRST_YEAR,-AgeClass)%>%
      as.data.frame()
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
  
  disco_curve<-ggplot(disco_data, aes(x=date, y = disc))+
    geom_col(mapping = aes(x = date, y = ni), alpha = 0.5)+
    geom_point(alpha = 0.5, size = 3)+
    geom_line()+
    theme_bw()+
    scale_colour_viridis_d(option = "plasma", name = '')+
    xlab("")+
    ylab("individuals per day and cumulative")+
    ylim(c(0,max(disco_data$disc)))
  
  ggsave(filename = 'disco_curve.png',disco_curve,device = 'png', './figures', dpi = 320, width = 120, height = 80, units = 'mm')
  
  #######################
  ## Sightings history ##
  #######################
  
  #this section is a place holder until the database is complete
  ##Capture history from Excel spreadsheet##
  caphist<-read.csv('./data/Dusky date capture history 2007-2021.csv', header = T, stringsAsFactors = F)
  
  thistrip<-daily_cap%>%
    dplyr::select(-SEX, -BIRTH_YEAR, -FIRST_YEAR, -AgeClass)%>%
    tidyr::pivot_longer(!(c(NAME)),names_to = "Date")%>%
    mutate(Date = lubridate::ymd(Date))%>%
    filter(value != 0)
  
  all_first_last<-caphist%>%
    tidyr::pivot_longer(!(c(Entry,NAME)),names_to = "Date")%>%
    filter(value != 0)%>%
    mutate(Date = lubridate::dmy(substr(str_replace_all(Date,"\\.","-"),2,9)))%>%
    dplyr::select(NAME, Date, value)%>%
    bind_rows(thistrip)%>%
    group_by(NAME)%>%
    mutate(first_date = min(Date),
           last_date = max(Date))%>%
    distinct(NAME,first_date,last_date)%>%
    filter(!is.na(NAME))%>%
    arrange(NAME)%>%
    left_join(lifehist, by = "NAME")%>%
    mutate(AgeClass = case_when(
      year - FIRST_YEAR > 3 ~ "A",
      year - BIRTH_YEAR == 0 ~ "C",
      !is.na(BIRTH_YEAR) & year - BIRTH_YEAR <= 3 & year - BIRTH_YEAR >= 0 ~ "J",
      TRUE ~ "U"
    ))
  
  all_first_last$DEATH_YEAR<-as.numeric(all_first_last$DEATH_YEAR)
  
  unseen_two_years<-all_first_last%>%
    filter(is.na(DEATH_YEAR))%>%
    filter(last_date < max(as.Date(f_data$DATE)) - lubridate::years(1) & last_date >= max(as.Date(f_data$DATE)) - lubridate::years(2))%>%
    mutate(LAST_YEAR = lubridate::year(last_date))%>%
    mutate(NameSex = case_when(
      nchar(NAME) > 2 ~ paste0(str_to_title(NAME)," (",SEX,")"),
      nchar(NAME) <= 2 ~ paste0(NAME," (",SEX,")")))
  
  unseen_table<-unseen_two_years%>%
    group_by(LAST_YEAR, SEX, AgeClass)%>%
    tally()%>%
    tidyr::pivot_wider(names_from = SEX, values_from = n)%>%
    arrange(factor(AgeClass, levels = c("A","J","C")))%>%
    replace(is.na(.), 0)%>%
    mutate(AgeClass = case_when(
      AgeClass == 'A' ~ "Adult",
      AgeClass == 'J' ~ "Juvenile",
      AgeClass == 'C' ~ "Calf*"
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
  
  ##########################
  ## Sightings and tracks ##
  ##########################
  print("sigs and trackline")
  
  sigs<-read.csv(paste0(pathimage,"/Sightings/f_",phyear,"_",phmonth," CyberTracker.csv"), header = T, stringsAsFactors = F)
  sigs<-sigs%>%mutate(Datetime = dmy_hms(paste(Date, Time)))%>%
    dplyr::select(Datetime, everything())
  sigs$Date<-dmy(sigs$Date)
  
  daterange<-range(sigs$Datetime)
  
  tracks<-sf::st_read(paste0(pathimage,"/Tracks"), layer = paste0(phyear,"_",phmonth), quiet = T)
  tracks<-as.data.frame(tracks)%>%
    dplyr::select(DATE, TIME, LAT, LON)%>%
    mutate(Datetime = ymd_hms(paste(DATE, TIME)))%>%
    dplyr::select(Datetime, everything())%>%
    distinct()

  #find nearest position for time changes
  data.table::setDT(sigs)[,  Lat := data.table::setDT(tracks)[sigs, LAT, on = "Datetime", roll = "nearest"]]
  data.table::setDT(sigs)[,  Lon := data.table::setDT(tracks)[sigs, LON, on = c("Datetime" = "Datetime"), roll = "nearest"]]
      
  #filter tracks to only between on/off effort      
  f_data<-tracks%>%
    filter(Datetime >= daterange[1] & Datetime <= daterange[2])%>%
    full_join(sigs, by = c("Datetime","LAT"="Lat","LON"="Lon","DATE"="Date","TIME"="Time"))%>%
    mutate(DATE = as.factor(DATE))%>%
    arrange(Datetime)
  
  sig_num<-f_data%>%
    filter(Home.screen == "Encounter END & DATA" | Home.screen == "Encounter START")%>%
    mutate(signum = rep(1:(n()/2), each = 2))%>%
    dplyr::select(Datetime, DATE, signum, Home.screen)
  
  f_data<-f_data%>%
    left_join(sig_num, by = c("Datetime", "DATE","Home.screen"))

  sig_count<-max(f_data$signum, na.rm = T)
  
  sig_days<-sig_num%>%
    distinct(DATE)
  
  write.csv(f_data, paste0(pathimage,"/f_data_",phyear,"_",phmonth,".csv"), row.names = F, na = "")
  incProgress(3/5)
  #convert meters to nm
  m_nm<-1/1852
  
  f_data_dist<-f_data%>%
    group_by(DATE)%>%
    mutate(LAT2 = dplyr::lag(LAT),
           LON2 = dplyr::lag(LON),
           dist_km = geosphere::distVincentyEllipsoid(matrix(c(LON,LAT), ncol = 2),matrix(c(LON2, LAT2), ncol =2),a=6378137, f=1/298.257222101)*m_nm)
  
  track_dist<-round(sum(f_data_dist$dist_km, na.rm=TRUE),0)
  
hours_wTt<-sig_num%>%
    dplyr::select(signum, Datetime, Home.screen)%>%
    tidyr::pivot_wider(names_from = Home.screen, values_from = Datetime)%>%
    mutate(time_wTt = `Encounter END & DATA` - `Encounter START`)%>%
    dplyr::summarise(total_wTt = round(as.numeric(sum(time_wTt))/60,1))

  #########
  ## MAP ##
  #########
  
  NZ_coast<-readOGR("./shapefiles", layer = "nz-coastlines-and-islands-polygons-topo-1500k")
incProgress(4/5)
  map<-ggplot()+
    geom_polygon(NZ_coast, mapping = aes(long,lat,group = group), alpha = 0.8)+
    geom_path(f_data, mapping = aes(LON, LAT, group = DATE, color = DATE))+
    geom_point(f_data%>%filter(Home.screen == "Encounter START"), mapping = aes(LON, LAT, color = DATE), shape = 23, fill = "red", size = 1.5, stroke = 1.5)+
    theme_bw()+
    scale_color_viridis_d(name = "Date")+
    xlab("Longitude")+
    ylab("Latitude")
  
  print("mapping")
  
  if (pharea == "Dusky"){
    map<-map+
      coord_sf(xlim = c(166.45,167.0), ylim = c(-45.81,-45.49), crs = 4269)
  } else if (pharea == "Doubtful"){
    map<-map+
      coord_sf(xlim = c(166.8,167.2), ylim = c(-45.5,-45.15), crs = 4269)
  } else {
    map<-map+
      coord_sf(xlim = c(min(f_data$LON),max(f_data$LON)), ylim = c(min(f_data$LAT),max(f_data$LAT)), crs = 4269)
  }
  
  ggsave(filename = 'map.png',map,device = 'png', './figures', dpi = 320, width = 169, height = 120, units = 'mm')
  incProgress(5/5)
 print("Done")
 enable("report")

 output$report<-downloadHandler(
   
   filename = paste0("FBD_monitoring_report_",phyear,"_",phmonth,".pdf"),
   
   content = function(file) {
     
      tempReport<-file.path("./scripts/FBD summary template.Rmd")
      file.copy("FBD summary template.Rmd", tempReport, overwrite = FALSE)
 
      loc_base<-paste0(pharea,"/",input$locbase)
      nsurveydays<-nrow(f_data%>%distinct(DATE))
      vessel<-input$vessel
      crew<-stringr::str_c(input$crew, "\\linebreak", collapse = " ")
      wx_comments<-input$wx_comments
      calf_comments<-input$calf_comments
      next_comments<-input$next_comments
      
      pop_est<-read.csv('./data/FBD_popest.csv', header = T)%>%
        filter(Year == max(Year))%>%
        mutate(popsent = paste0(Year,": ",Est, " (95% CI = ",lcl,"--",ucl,")"))
      
      
      params<-list( tripdate_s =  tripdate_s, tripdate_e = tripdate_e, loc_base = loc_base, 
                    nsurveydays = nsurveydays, recent = recent, older = older, 
                    vessel = vessel, crew = crew, pop_est = pop_est, uniqueID = uniqueID,
                    track_dist = track_dist, sig_days = sig_days, sig_count = sig_count, hours_wTt = hours_wTt, 
                    age_sex_table = age_sex_table, unseen_table = unseen_table,
                    wx_comments = wx_comments, calf_comments = calf_comments, next_comments = next_comments)
      print(params)
      rmarkdown::render(tempReport, output_file = file,
                   params = params,
                   envir = new.env(parent = globalenv()))
   })
  }
  })
})
  