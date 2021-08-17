crew<-read.csv('./data/FBD_crew.csv')%>%mutate(creworg = paste0(FIRST,' ',LAST,', ',ORGANIZATION))
crew_list<-crew$creworg
output$crew<-renderUI({
  pickerInput("crew","Crew", choices = c(crew_list), multiple = T)
})

observeEvent(input$photogo,{
  
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
  
  } else if (input$EXIF == "load"){
  
    allmerge_dt<-read.csv(paste0(pathimage,"/Photo analysis/f_PA_",phyear,"_",phmonth,".csv"), header = T, stringsAsFactors = T)
  }
  
  uniqueID<-allmerge_dt%>%distinct(ID_Name)%>%filter(!grepl("\\?",ID_Name))
  
  pop_est<-read.csv('./data/FBD_popest.csv', header = T)%>%
    filter(Year == max(Year))%>%
    mutate(popsent = paste0(Year,": ",Est, " (95% CI = ",lcl,"–",ucl,")"))
  
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
    dplyr::select(Datetime, DATE, signum)
  
  f_data<-f_data%>%
    left_join(sig_num, by = c("Datetime", "DATE"))%>%
    filter(Home.screen == "Encounter END & DATA" | Home.screen == "Encounter START")
  
  sig_count<-max(f_data$signum)
  
  sig_days<-sig_num%>%
    distinct(DATE)
  
  write.csv(f_data, paste0(pathimage,"/f_data_",phyear,"_",phmonth,".csv"), row.names = F, na = "")
  
  #convert meters to nm
  m_nm<-1/1852
  
  f_data_dist<-f_data%>%
    group_by(DATE)%>%
    mutate(LAT2 = dplyr::lag(LAT),
           LON2 = dplyr::lag(LON),
           dist_km = geosphere::distVincentyEllipsoid(matrix(c(LON,LAT), ncol = 2),matrix(c(LON2, LAT2), ncol =2),a=6378137, f=1/298.257222101)*m_nm)
  
  track_dist<-round(sum(f_data_dist$dist_km, na.rm=TRUE),0)
  
hours_wTt<-f_data%>%
    dplyr::select(signum, Datetime, Home.screen)%>%
    tidyr::pivot_wider(names_from = Home.screen, values_from = Datetime)%>%
    mutate(time_wTt = `Encounter END & DATA` - `Encounter START`)%>%
    dplyr::summarise(total_wTt = round(as.numeric(sum(time_wTt))/60,1))

  #########
  ## MAP ##
  #########
  
  NZ_coast<-readOGR("./shapefiles", layer = "nz-coastlines-and-islands-polygons-topo-1500k")
  
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
  
  ggsave(filename = 'map.png',map,device = 'png', './figures', dpi = 320, width = 169, height = 180, units = 'mm')
  
 print("Done")
 
 output$report<-downloadHandler(
   
   filename = paste0(pathimage,"/FBD_monitoring_report_",phyear,"_",phmonth,".pdf"),
   
   content = function(file) {
     
      tempReport<-file.path("./scripts/FBD trip summary.Rmd")
      file.copy("FBD trip summary.Rmd", tempReport, overwrite = FALSE)
 
      tripdate_s<-min(as.Date(f_data$DATE))
      tripdate_e<-max(as.Date(f_data$DATE))
      loc_base<-paste0(pharea,"/",input$locbase)
      #nsurveydays<-nrow(f_data%>%distinct(DATE))
      vessel<-input$vessel
      crew<-input$crew
      wx_comments<-input$wx_comments
      calf_comments<-input$calf_comments
      next_comments<-input$next_comments
      
      #print(nsurveydays)
      
      params<-list( tripdate_s =  tripdate_s, tripdate_e = tripdate_e, loc_base = loc_base, 
                    #nsurveydays = nsurveydays, 
                    vessel = vessel, crew = crew, pop_est = pop_est,
                    track_dist = track_dist, sig_days = sig_days, sig_count = sig_count, hours_wTt = hours_wTt, 
                    wx_comments = wx_comments, calf_comments = calf_comments, next_comments = next_comments)
      print(params)
      rmarkdown::render(tempReport, output_file = file,
                   params = params,
                   envir = new.env(parent = globalenv()))
   })
 
 
})
  