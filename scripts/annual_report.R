library(odbc);library(dplyr);library(DBI);library(lubridate);library(ggplot2);library(viridis)

max_year = 2023
min_year = 2004

source('~/git-otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value
photo_analysis_calfyear_sql<-dbReadTable(con, "photo_analysis_calfyear")%>%filter(ID_NAME != "CULL")
survey_data<-dbReadTable(con, "survey_data_calfyear")
trip_summary<-dbReadTable(con, "trip_summary")
source('~/git-otago/Fiordland_reporting/scripts/life_history_ageclass update.R', local = TRUE)$value
#lifehist<-dbReadTable(con, "life_history")
#survey_data<-dbReadTable(con, "survey_data_calfyear")

photo_analysis_calfyear_sql$CALFYEAR<-as.numeric(photo_analysis_calfyear_sql$CALFYEAR)
#photo_analysis_calfyear_sql$YEAR<-as.numeric(photo_analysis_calfyear_sql$YEAR)

#age is determined by calfyear
lifehist_long<-lifehist%>%
  tidyr::pivot_longer(cols = c(13:ncol(lifehist)), names_to = "YEAR", values_to = "AGECLASS")%>%
  mutate(YEAR = as.numeric(YEAR))%>%
  filter(YEAR > 2004 & POD != "NORTHERN")#%>%
  #filter(!(YEAR < 2016 & POD == "DUSKY"))
  # mutate(YEAR = as.numeric(substr(YEAR, 2, 5)))

myCol<-viridis(5, option = "D")
ageclass_fill = c("D" = myCol[5],"C" = myCol[4], "J" = myCol[3], "S-A" = myCol[2],"A" = myCol[1], "U" = "grey")

###############

calfyearcap<-photo_analysis_calfyear_sql%>%
  left_join(lifehist, by = c("ID_NAME" = "NAME"))%>%
  filter(CALFYEAR > 2004 & CALFYEAR <=2023 &  POD != "NORTHERN")%>%
  #filter(!(CALFYEAR < 2016 & POD == "DUSKY"))%>%
  group_by(CALFYEAR, POD)%>%
  distinct(TRIP, ID_NAME)%>%
  mutate(month_start = as.numeric(substr(TRIP,6,7)))%>%
  mutate(season = case_when(
    month_start <= 2 | month_start == 12 ~ "SUMMER",
    between(month_start, 3, 5) ~ "AUTUMN",
    between(month_start, 6, 8) ~ "WINTER",
    between(month_start, 9, 11) ~ "SPRING"
  ))

yearcap<-photo_analysis_calfyear_sql%>%
  left_join(lifehist, by = c("ID_NAME" = "NAME"))%>%
  filter(YEAR > 2004 & YEAR <=2023 &  POD != "NORTHERN")%>%
  group_by(YEAR, POD)%>%
  distinct(TRIP, ID_NAME)%>%
  mutate(month_start = as.numeric(substr(TRIP,6,7)))%>%
  mutate(season = case_when(
    month_start <= 2 | month_start == 12 ~ "SUMMER",
    between(month_start, 3, 5) ~ "AUTUMN",
    between(month_start, 6, 8) ~ "WINTER",
    between(month_start, 9, 11) ~ "SPRING"
  ))

#error checking
yearcap%>%filter(is.na(POD))
#yearcap%>%filter(TRIP == '2019_07')

IDs<-yearcap%>%ungroup%>%distinct(ID_NAME)

hist_ID<-IDs%>%left_join(lifehist, by = c("ID_NAME" = "NAME"))

yeartriptally<-yearcap%>%
  group_by(POD, YEAR, TRIP)%>%
  tally()

yeartriptally%>%filter(YEAR == 2015)

calfyeartally<-calfyearcap%>%
  distinct(POD, ID_NAME)%>%
  filter(!(ID_NAME == "SUNSHINE" & CALFYEAR == 2023))%>%
  filter(!grepl("D-2", ID_NAME))%>%
  tally()%>%
  dplyr::rename("calfyear" = "n")

yeartally<-yearcap%>%
  distinct(POD, ID_NAME)%>%
  filter(!(ID_NAME == "SUNSHINE" & YEAR == 2023))%>%
  filter(!grepl("D-2", ID_NAME))%>%
  tally()%>%
  dplyr::rename("year" = "n")

yeartally<-calfyeartally%>%
  left_join(yeartally, by = c("CALFYEAR" = "YEAR", "POD"))

yeartally

yeartally$CALFYEAR<-as.numeric(yeartally$CALFYEAR)

## last seen ----
#need this so animals aren't represented twice if there periods when we didn't know their ageclass (i.e. unknown birth years)
last_seen_ind<-lifehist_long%>%
  filter(LAST_YEAR == YEAR)%>%
  filter(LAST_YEAR >= min_year)

last_seen<-last_seen_ind%>%
  distinct(POD, LAST_YEAR, NAME, AGECLASS)%>%
  group_by(POD, LAST_YEAR, AGECLASS)%>%
  tally()%>%
  mutate(AGECLASS = case_when(
    AGECLASS == "NA" ~ "C",
    TRUE ~ AGECLASS))

last_seen$AGECLASS<-factor(last_seen$AGECLASS, levels = c("U","A","S-A","J","C","D"))

lastseen_plot<-ggplot(last_seen%>%filter(LAST_YEAR < max_year))+
  geom_col(mapping = aes(x = as.factor(LAST_YEAR), y = n, fill = AGECLASS), color = "black", alpha = 0.7)+
  theme_bw()+
  scale_fill_manual(values = ageclass_fill)+
  xlab("Year")+
  ylab("Number of individuals")+
  theme(legend.position = "bottom")+
  facet_wrap(~POD, ncol = 1)

ggsave('./figures/lastseen_plot.png', dpi = 320, width = 160, height = 100, units = 'mm')

########

year_name<-photo_analysis_calfyear_sql%>%
  distinct(ID_NAME, YEAR)%>%
  mutate(minyear = min(YEAR), maxyear = max(YEAR))

year_perdolphin<-lifehist_long%>%
  left_join(year_name, by = c("NAME" = "ID_NAME", "YEAR" = "YEAR"))%>%
  filter(!is.na(minyear))%>%
  filter(YEAR == max_year)%>%
  #filter(NAME == "NANCY")%>%
  mutate(AGECLASS = case_when(
   AGECLASS == "NA" & as.numeric(FIRST_YEAR) == max_year ~ "C",
    TRUE ~ AGECLASS))
#nrow(year_perdolphin)
year_perdolphin$AGECLASS<-factor(year_perdolphin$AGECLASS, levels = c("U","A","S-A","J","C","D"))

year_perdolphin%>%filter(AGECLASS == "NA")%>%arrange(NAME)

year_perdolphin_ageprop<-year_perdolphin%>%group_by(POD, YEAR, AGECLASS)%>%tally()%>%
  group_by(POD)%>%mutate(n_tot = sum(n))%>%
  mutate(prop = n/n_tot)

repyear_propageclass<-ggplot(year_perdolphin_ageprop)+
  geom_col(aes(x = as.factor(POD), y = prop, fill = AGECLASS),color = "black", position = "stack", alpha = 0.7)+
  theme_bw()+
  #facet_wrap(~POD)+
  xlab("")+
  ylab("Proportion")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = ageclass_fill)

year_perdolphin_sexprop<-year_perdolphin%>%group_by(POD, YEAR, SEX)%>%tally()%>%
  group_by(POD)%>%mutate(n_tot = sum(n))%>%
  mutate(prop = n/n_tot)

repyear_propsex<-ggplot(year_perdolphin_sexprop)+
  geom_col(aes(x = as.factor(POD), y = prop, fill = SEX),color = "black", position = "stack", alpha = 0.5)+
  theme_bw()+
  #facet_wrap(~POD)+
  xlab("")+
  ylab("")+
  theme(legend.position = "bottom")

prop_agesex_reportyear<-ggpubr::ggarrange(repyear_propageclass,repyear_propsex, align = "h", labels = "auto")

ggsave('./figures/prop_agesex_reportyear.png', dpi = 320, width = 200, height = 100, units = 'mm')

###########
library(ggnewscale)

calves<-lifehist%>%
  #mutate(BIRTH_YEAR = FIRST_YEAR)%>%
  filter(BIRTH_YEAR >= min_year)%>%
  #filter(AGECLASS == "C" | AGECLASS == "D")%>%
  distinct(POD, BIRTH_YEAR, NAME)%>%
  group_by(POD, BIRTH_YEAR)%>%
  tally()%>%
  ungroup()%>%
  tidyr::complete(POD, BIRTH_YEAR)%>%
  replace(is.na(.), 0)

calf_plot<-ggplot(calves)+
  scale_color_brewer(palette = 'Dark2') +
  geom_point(data = calves%>%filter(as.numeric(BIRTH_YEAR) <= max_year), mapping = aes(x = as.numeric(BIRTH_YEAR), y = n, color = POD), size = 4, alpha = 0.5)+
  geom_line(data = calves%>%filter(as.numeric(BIRTH_YEAR) <= max_year), mapping = aes(x = as.numeric(BIRTH_YEAR), y = n, color = POD, linetype = POD))+
  #new_scale_color() +
  #geom_point(aes(x = YEAR, y = Nhat, color = season))+
  #scale_color_manual(values = c("black","purple")) +
  ylab("Number of calves born")+
  xlab("Year")+
  #facet_wrap(~POD, scales = "free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_x_continuous(limits = c(2004,2023))

ggsave('./figures/calf_plot.png', dpi = 320, width = 150, height = 80, units = 'mm')

#####
yeartally%>%as.data.frame()

adults<-photo_analysis_calfyear_sql%>%
  distinct(SURVEY_AREA, ID_NAME, CALFYEAR)%>%
  left_join(lifehist, by = c("ID_NAME" = "NAME"))%>%
  tidyr::pivot_longer(cols = 16:ncol(.), names_to = "YEAR_age", values_to = "ageclass")%>%
  filter(CALFYEAR == YEAR_age)%>%
  filter(ageclass == "A" | ageclass == "U")%>%
  filter(CALFYEAR > 2004 & CALFYEAR <=2023 &  POD != "NORTHERN")%>%
  filter(!(SURVEY_AREA == "DUSKY" & CALFYEAR == 2007))%>%
  filter(!(ID_NAME == "SUNSHINE" & CALFYEAR == 2023))%>%
  filter(!grepl("D-2", ID_NAME))

adults_calfyear<-adults%>%
  distinct(POD, CALFYEAR, ID_NAME)%>%
  group_by(POD, CALFYEAR)%>%
  tally()

census_plot<-ggplot(yeartally)+
  geom_line(aes(x = CALFYEAR, y = calfyear, color = POD), linetype = "dashed")+
  geom_point(aes(x = CALFYEAR, y = calfyear, color = POD))+
  geom_line(aes(x = CALFYEAR, y = year, color = POD))+
  geom_point(aes(x = CALFYEAR, y = year, color = POD), shape = 25)+  
  geom_line(adults_calfyear, mapping = aes(x = CALFYEAR, y = n, color = POD), linetype = "dashed")+
  geom_point(adults_calfyear, mapping = aes(x = CALFYEAR, y = n, color = POD))+
  labs(shape = "", linetype = "")+
  ylab("Number of individuals")+
  xlab("Year")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(""))+
  #scale_shape_manual(values = shapes)+
  #scale_linetype_manual(values = linetype)+
  scale_color_brewer(palette = "Dark2")+
  scale_x_continuous(limits = c(2008,2023))
  
ggsave('./figures/census_plot.png', dpi = 320, width = 120, height = 100, units = 'mm')

photo_n<-photo_analysis_calfyear_sql%>%
  filter(ID_NAME != "CULL")%>%
  filter(CALFYEAR < 2024)%>%
  filter(!(SURVEY_AREA == "DUSKY" & CALFYEAR == 2007))%>%
  filter(!(SURVEY_AREA == "DOUBTFUL" & CALFYEAR == 2004))%>%
  filter(SURVEY_AREA == "DOUBTFUL" | SURVEY_AREA == "DUSKY")%>%
  distinct(SURVEY_AREA, CALFYEAR, DATE, FILENAME, PHOTOGRAPHER)%>%
  group_by(SURVEY_AREA, CALFYEAR)%>%
  tally()%>%
  dplyr::rename("POD" = "SURVEY_AREA")

photo_n%>%
  group_by(POD)%>%
  dplyr::summarise(min = min(n), max = max(n), mean = mean(n))

#ggsave('./figures/census_photos_plot.png', dpi = 600, width = 300, height = 120, units = 'mm')

###
caphist_wide<-calfyearcap%>%
  distinct(CALFYEAR, POD, ID_NAME)%>%
  arrange(ID_NAME,CALFYEAR)%>%
  group_by(ID_NAME)%>%
  tidyr::pivot_wider(names_from = CALFYEAR, values_from = POD)

caphist_wide

caphist_wide[caphist_wide == "DUSKY"]<-'1'
caphist_wide[caphist_wide == "DOUBTFUL"]<-'1'
caphist_wide[is.na(caphist_wide)]<-'0'

caphist_long<-caphist_wide%>%
  tidyr::pivot_longer(cols = 2:ncol(.), names_to = "CALFYEAR")%>%
  arrange(ID_NAME,CALFYEAR)%>% 
  group_by(ID_NAME) #%>% 
  #mutate(value = replace(value, cumsum(value != 0) %% 2 == 1, 1))

cap_lo<-caphist_long%>%
  left_join(lifehist, by = c("ID_NAME" = "NAME"))%>%
  ungroup()%>%
  mutate(FIRST_CALF_YEAR = case_when(
    month(FIRST_DATE) >= 9 ~ as.numeric(FIRST_YEAR) + 1,
    TRUE ~ as.numeric(FIRST_YEAR)
  ))%>%
  mutate(FIRST_BIRTH = case_when(
    BIRTH_YEAR == '' ~ as.numeric(FIRST_CALF_YEAR),
    TRUE ~ as.numeric(BIRTH_YEAR)
  ))%>%
  filter(CALFYEAR >= FIRST_BIRTH & CALFYEAR <= LAST_YEAR)%>%
  group_by(ID_NAME)%>%
  mutate(value2 = lead(as.numeric(value)) - as.numeric(value))%>%
  dplyr::select(ID_NAME, SEX, POD, FIRST_BIRTH, FIRST_DATE, CALFYEAR, value, value2)

cap_nogap<-cap_lo%>%
  filter(!(ID_NAME == "SUNSHINE" & CALFYEAR == 2023))%>%
  filter(!grepl("D-2", ID_NAME))%>%
  group_by(ID_NAME)%>%
  mutate(value3 = case_when(
    as.numeric(value) == 0 & as.numeric(FIRST_BIRTH) != as.numeric(CALFYEAR) ~ as.numeric(1),
    TRUE ~ as.numeric(value)
  ))%>%
  filter(value3 != 0)%>%
  filter(CALFYEAR != 2024)

cap_ng_tally<-cap_nogap%>%
  group_by(POD, CALFYEAR)%>%
  mutate(calfyear_ng = sum(value3))%>%
  mutate(CALFYEAR = as.numeric(CALFYEAR))

comp<-cap_ng_tally%>%
  left_join(yeartally, by = c("POD","CALFYEAR"))%>%
  ungroup()%>%
  distinct(POD, CALFYEAR, calfyear_ng, calfyear, year)

saveRDS(comp, paste0("./data/tally_", Sys.Date(),".rds"))

ggplot(yeartally%>%filter(!(POD == "DUSKY" & CALFYEAR == 2007)))+
  geom_col(data = photo_n, mapping = aes(x = CALFYEAR, y = n/100, fill = POD), alpha = 0.4)+
  geom_line(cap_ng_tally%>%filter(!(POD == "DUSKY" & CALFYEAR <= 2007)), mapping = aes(x = as.numeric(CALFYEAR), y = sum, color = POD))+
  geom_point(cap_ng_tally%>%filter(!(POD == "DUSKY" & CALFYEAR <= 2007)), mapping = aes(x = as.numeric(CALFYEAR), y = sum, color = POD), size = 3)+
  geom_line(aes(x = CALFYEAR, y = calfyear), color = "black", linetype = "dashed")+
  geom_point(aes(x = CALFYEAR, y = calfyear), color = "black")+
  #geom_line(adults_calfyear, mapping = aes(x = CALFYEAR, y = n, color = POD), linetype = "dashed")+
  #geom_point(adults_calfyear, mapping = aes(x = CALFYEAR, y = n, color = POD))+
  facet_wrap(~POD)+
  theme_bw()+
  theme(legend.position ="none")+
  xlab("Year")+
  scale_y_continuous(name = "# individuals identified", sec.axis = sec_axis(~.*100, name = "# photographs"))

ggsave('./figures/observed_photos.png', dpi = 600, width = 250, height = 150, units = 'mm')

photo_n%>%
  group_by(POD)%>%
  dplyr::summarise(mean = mean(n), min = min(n), max = max(n))

sex_tally<-cap_nogap%>%
  mutate(CALFYEAR = as.numeric(CALFYEAR))%>%
  left_join(lifehist_long, by = c("ID_NAME" = "NAME", "SEX", "POD", "FIRST_DATE", "CALFYEAR" = "YEAR"))%>%
  mutate(sex_assume = case_when(
    SEX == "F" ~ "F",
    SEX != "F" ~ "M_U"
  ))%>%
  group_by(POD, sex_assume, CALFYEAR, AGECLASS)%>%
  mutate(sum = sum(value3))%>%
  mutate(CALFYEAR = as.numeric(CALFYEAR))%>%
  distinct(POD, sex_assume, CALFYEAR, sum)%>%
  filter(AGECLASS == "A" | AGECLASS == "U")


# known adults
ggplot(sex_tally%>%filter(!(POD == "DUSKY" & CALFYEAR <= 2007)))+
  geom_line(cap_ng_tally%>%filter(!(POD == "DUSKY" & CALFYEAR <= 2007)), mapping = aes(x = as.numeric(CALFYEAR), y = sum))+
  geom_point(cap_ng_tally%>%filter(!(POD == "DUSKY" & CALFYEAR <= 2007)), mapping = aes(x = as.numeric(CALFYEAR), y = sum), size = 3)+
  geom_line(mapping = aes(x = as.numeric(CALFYEAR), y = sum, color = as.factor(sex_assume), linetype = AGECLASS))+
  geom_point(mapping = aes(x = as.numeric(CALFYEAR), y = sum, color = as.factor(sex_assume), shape = AGECLASS), size = 3)+
  facet_wrap(~POD)

####
# library(scales)
# lastseen_plot_calf<-lastseen_plot+
#   geom_point(data = calves%>%filter(as.numeric(BIRTH_YEAR) < reporting_year)%>%filter(POD == "DOUBTFUL"), mapping = aes(x = as.numeric(BIRTH_YEAR), y = n), fill = "black", shape = 22)+
#   scale_y_continuous(breaks= pretty_breaks())
# 
# ggsave('./figures/lastseen_plot_calf.png', dpi = 320, width = 125, height = 100, units = 'mm')
# 
# ###########
# ggplot(LPC_df_ls%>%filter(subset == "Calendar"))+
#   scale_color_brewer(palette = 'Dark2') +
#   geom_point(data = calves%>%filter(as.numeric(BIRTH_YEAR) <= reporting_year), mapping = aes(x = as.numeric(BIRTH_YEAR), y = n, color = POD))+
#   geom_line(data = calves%>%filter(as.numeric(BIRTH_YEAR) <= reporting_year), mapping = aes(x = as.numeric(BIRTH_YEAR), y = n, color = POD))+
#   ylab("Number of individuals")+
#   xlab("Year")+
#   theme_bw()+
#   theme(legend.position = "bottom")+
#   scale_x_continuous(breaks = seq(min(LPC_df_ls$YEAR),max(LPC_df_ls$YEAR),3), minor_breaks = seq(min(LPC_df_ls$YEAR),max(LPC_df_ls$YEAR),1))


###
#use below in future years
#survey_data_report<-survey_data%>%filter(YEAR == reporting_year)
#map?

#############
##timeline
datalog<-read.csv('./data/Data_Log.csv', header = T, stringsAsFactors = T)

datalog<-datalog%>%
  filter(Vessel != "PEMBROKE")%>%
  dplyr::select(Fiord, Year, Season, Folder, Start_date, End_date, Photos)%>%
  tidyr::pivot_longer(cols = ends_with("date"), names_to = "start_end", values_to = "Date")%>%
  mutate(Date = dmy(Date),
         Ordinal = yday(Date),
         MODA = format(as.Date(Date), "%d-%b"))%>%
  filter(Fiord == "DOUBTFUL" | Fiord == "DUSKY" | Fiord == "NANCY" | Fiord == "CHARLES" | Fiord == "DAGG" | Fiord == "CHALKY" | Fiord == "PRESERVATION")

#levels(fiords$Fiord)<-c("MARTINS","MILFORD","BLIGH","GEORGE","CASWELL","CHARLES","NANCY","DOUBTFUL","DAGG","DUSKY","CHALKY","PRESERVATION","STEWART","DUNEDIN")
datalog$Fiord<-ordered(datalog$Fiord, levels = c("CHARLES","NANCY","DOUBTFUL","DAGG","DUSKY","CHALKY","PRESERVATION"))

unique(datalog$Fiord)

timeline_data<-datalog%>%filter(Year >=2004)%>%filter(Fiord == "DUSKY" | Fiord == "DOUBTFUL")%>%
  mutate(tripid = paste0(Fiord,Folder),
         calfyear = case_when(
           month(Date) >= 9 ~ Year + 1,
           TRUE ~ Year
         ),
         sep_start = Ordinal - 244)

library(viridis)
col_pal<-viridis(4)

timeline<-ggplot(timeline_data)+
  geom_point(aes(x = Ordinal, y = as.factor(Year), group = tripid), shape = "square")+
  geom_path(aes(x = Ordinal, y = as.factor(Year), group = tripid), linewidth = 1.85)+
  geom_point(timeline_data%>%filter(Photos == "MISSING"), mapping = aes(x = Ordinal, y = as.factor(Year), group = tripid), color = "grey", shape = "square")+
  geom_path(timeline_data%>%filter(Photos == "MISSING"), mapping = aes(x = Ordinal, y = as.factor(Year), group = tripid), color = "grey", linewidth = 1.85)+
  scale_x_continuous(breaks = c(1,32,60,91,121,152,182,213,244,274,305,335,366),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",""), limits = c(1,366))+
  annotate("rect", xmin = 60, xmax = 151, ymin = as.factor(2004), ymax = as.factor(2023),
           alpha = .2,fill = "orange")+
  annotate("rect", xmin = 152, xmax = 243, ymin = as.factor(2004), ymax = as.factor(2023),
           alpha = .2,fill = "darkblue")+
  annotate("rect", xmin = 244, xmax = 334, ymin = as.factor(2004), ymax = as.factor(2023),
           alpha = .2,fill = "green")+
  annotate("rect", xmin = 335, xmax = 366, ymin = as.factor(2004), ymax = as.factor(2023),
           alpha = .2,fill = "red")+
  annotate("rect", xmin = 1, xmax = 59, ymin = as.factor(2004), ymax = as.factor(2023),
           alpha = .2,fill = "red")+
  #scale_y_continuous(breaks = seq(min(data$Year), max(data$Year), by = 1))+
  theme_bw()+
  xlab("")+
  ylab("Calendar year")+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom")+
  facet_wrap(~Fiord)

timeline

ggsave('./figures/timeline.png', timeline, dpi = 320, width = 200, height = 100, units = 'mm')

###
#tables
trip_summary

effort_days<-survey_data%>%
  filter(year(DATE) == max_year)%>%
  distinct(DATE)%>%
  left_join(trip_summary, by = "DATE")%>%
  group_by(SURVEY_AREA, TRIP)%>%
  mutate(`Effort (days)` = n(),
         `Date range` = paste0(format(min(ymd(DATE)),"%d %b"),"–",format(max(ymd(DATE)),"%d %b")))%>%
  distinct(SURVEY_AREA, TRIP, `Date range`,`Effort (days)`)%>%
  filter(!is.na(SURVEY_AREA))%>%
  arrange(SURVEY_AREA, TRIP)

sig_days<-photo_analysis_calfyear_sql%>%
  filter(YEAR == max_year)%>%
  distinct(SURVEY_AREA, TRIP, DATE)%>%
  group_by(SURVEY_AREA, TRIP)%>%
  tally()%>%
  dplyr::rename(`Sighting (days)` = n)

n_season<-photo_analysis_calfyear_sql%>%
  filter(YEAR == max_year)%>%
  distinct(SURVEY_AREA, TRIP, ID_NAME)%>%
  group_by(SURVEY_AREA, TRIP)%>%
  tally()%>%
  dplyr::rename("Individuals" = n)

effort_table<-effort_days%>%
  left_join(sig_days)%>%
  mutate(month_start = as.numeric(substr(TRIP,6,7)))%>%
  mutate(Season = case_when(
    month_start <= 2 | month_start == 12 ~ "SUMMER",
    between(month_start, 3, 5) ~ "AUTUMN",
    between(month_start, 6, 8) ~ "WINTER",
    between(month_start, 9, 11) ~ "SPRING"
  ))%>%
  ungroup()%>%
  arrange(SURVEY_AREA, TRIP)%>%
  left_join(n_season)%>%
  mutate(SURVEY_AREA = stringr::str_to_title(SURVEY_AREA),
         Season = stringr::str_to_title(Season))%>%
  #dplyr::rename(`Survey area` = SURVEY_AREA)%>%
  dplyr::select(Season, `Date range`,`Effort (days)`, `Sighting (days)`, Individuals)

library(kableExtra)
print(knitr::kable(effort_table, format = "latex")%>%
        kable_styling()%>%
        group_rows("Patea-Doubtful", 1, 3) %>%
        group_rows("Tamatea-Dusky", 4, 6))
# 
# LPC_table<-LPC_df_ls%>%
#   filter(n1 != "NA" & subset == "Calendar")%>%
#   mutate(Pod = stringr::str_to_title(POD))%>%
#   mutate(`95% CI` = paste0(lcl,"–",ucl))%>%
#   dplyr::select(YEAR,n1, n2, m2, Nhat, `95% CI`, n)
  
print(knitr::kable(LPC_table, format = "latex")%>%
        kable_styling()%>%
        group_rows("Doubtful Pod", 1, 13) %>%
        group_rows("Dusky pod", 14, 16))

ind_list<-photo_analysis_calfyear_sql%>%
  filter(YEAR == reporting_year)%>%
  distinct(SURVEY_AREA, ID_NAME)%>%
  arrange(SURVEY_AREA, ID_NAME)

save(LPC_df_ls, LPC_calf_plot, calves, last_seen, lastseen_plot, timeline, effort_table, file = paste0("LPC_",reporting_year,".Rdata"))

