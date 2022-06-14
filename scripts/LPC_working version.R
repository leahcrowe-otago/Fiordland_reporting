library(odbc);library(dplyr);library(DBI);library(lubridate);library(ggplot2);library(viridis)

reporting_year = 2021
min_year = 2009

source('~/Documents/git_otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value
photo_analysis_calfyear_sql<-dbReadTable(con, "photo_analysis_calfyear")
#source('~/Documents/git_otago/Fiordland_reporting/scripts/life_history_ageclass update.R', local = TRUE)$value
lifehist<-dbReadTable(con, "life_history_ageclass")
survey_data<-dbReadTable(con, "survey_data_calfyear")

photo_analysis_calfyear_sql$CALFYEAR<-as.numeric(photo_analysis_calfyear_sql$CALFYEAR)
photo_analysis_calfyear_sql$YEAR<-as.numeric(photo_analysis_calfyear_sql$YEAR)

#age is determined by calfyear
lifehist_long<-lifehist%>%
  tidyr::pivot_longer(cols = c(13:ncol(lifehist)), names_to = "YEAR", values_to = "AGECLASS")%>%
  mutate(YEAR = as.numeric(substr(YEAR, 2, 5)))

myCol<-viridis(5, option = "D")
ageclass_fill = c("D" = myCol[5],"C" = myCol[4], "W" = myCol[3], "S-A" = myCol[2],"A" = myCol[1], "U" = "grey")

###############
scenario<-c("Calendar","Calfyear")
#scenario<-c("Calendar")

for (i in scenario){
  print(i)
#i = "Calendar"
#currently defining season by the month of trip start date

pa_lh<-photo_analysis_calfyear_sql%>%
  left_join(lifehist_long, by = c("ID_NAME" = "NAME", "CALFYEAR" = "YEAR"))

yearcap<-pa_lh%>%
  mutate(YEAR = case_when(
    i == "Calendar" ~ YEAR,
    i == "Calfyear" ~ CALFYEAR))%>%
  filter(YEAR > 2005)%>%
  group_by(YEAR, POD)%>%
  distinct(TRIP, ID_NAME)%>%
  mutate(month_start = as.numeric(substr(TRIP,6,7)))%>%
  mutate(season = case_when(
    YEAR == 2019 & month_start == 12 ~ "SPRING",
    YEAR == 2015 & month_start == 12 ~ "SPRING",
    month_start <= 2 | month_start == 12 ~ "SUMMER",
    between(month_start, 3, 5) ~ "AUTUMN",
    between(month_start, 6, 8) ~ "WINTER",
    between(month_start, 9, 11) ~ "SPRING"
  ))

#error checking
#yearcap%>%filter(is.na(POD))
#yearcap%>%filter(TRIP == '2019_02')

yeartriptally<-yearcap%>%
  group_by(POD, YEAR, TRIP)%>%
  tally()

yeartriptally%>%filter(YEAR == 2015)

yeartally<-yearcap%>%
  distinct(POD, ID_NAME)%>%
  tally()

yearcap%>%
  distinct(POD, ID_NAME)%>%
  filter(YEAR == 2019)%>%arrange(ID_NAME)%>%
  as.data.frame()

yeartally$YEAR<-as.numeric(yeartally$YEAR)

#yeartally%>%filter(YEAR == 2015)
##need to do lapply by year
yearcap_ls<-split(yearcap, list(yearcap$YEAR, yearcap$POD))

season_cap<-lapply(yearcap_ls, function(x){
  #x<-yearcap_ls$`2015.DOUBTFUL`
  
  x<-x%>%
  ungroup()%>%
  #filter(TRIP != '2016_12')%>%
  dplyr::select(-month_start,-TRIP)%>% 
  distinct()%>%
  mutate(val = 1)%>%
  tidyr::pivot_wider(id_cols = c("ID_NAME"), names_from = season, values_from = val)%>%
  replace(is.na(.), 0)
  
  season_list <- c("WINTER","SPRING","AUTUMN","SUMMER")
  #ADD COLUMN WITH VALUES "NS" IF NO SURVEY WAS CONDUCTED IN THAT SEASON
  x[season_list[!(season_list %in% colnames(x))]] = 999
  
  if(i == "Calendar"){
  x%>%
    mutate(both = case_when(
    WINTER != 999 & SPRING != 999 & WINTER+SPRING == 2 ~ 1,
    WINTER == 999 & SPRING != 999 & AUTUMN != 999 & AUTUMN+SPRING == 2 ~ 1,
    WINTER != 999 & SPRING == 999 & AUTUMN != 999 & AUTUMN+WINTER == 2 ~ 1,
    TRUE ~ 0))
  } else {
    x%>%
      mutate(initial = case_when(
          #YEAR == 2017 & SPRING == 1 ~ 1,
          SUMMER == 1 ~ 1,
          SUMMER == 999 & SPRING == 1 ~ 1,
          TRUE ~ 0),
        second = case_when(
          WINTER == 1 ~ 1,
          WINTER == 999 & AUTUMN == 1 ~ 1, 
          TRUE ~ 0))%>%
      mutate(both = case_when(
          initial == 1 & second == 1 ~ 1,
          TRUE ~ 0))
  }
})

LPC_ls<-lapply(season_cap, function(x){
  #x<-season_cap$`2009.DUSKY`
  
if(i == "Calendar"){
  if(999 %in% x$WINTER == FALSE) {
      initial_n = sum(as.numeric(x$WINTER))
      season = "Winter"
  } else if (999 %in% x$AUTUMN == FALSE){
    initial_n = sum(as.numeric(x$AUTUMN))
      season = "Autumn"
  } else {
    initial_n = 999 
     season = "999"
  }

  if(999 %in% x$SPRING == FALSE) {
    second_n = sum(as.numeric(x$SPRING))
    season = paste(season, "Spring")
  } else {
    second_n = 999
    season = paste(season, "999")
  }
  
  if(initial_n != 999 & second_n != 999){
  both_n = sum(as.numeric(x$both))
  } else {
    both_n = 999
  }
  
} else {
  initial_n = sum(as.numeric(x$initial))
  second_n = sum(as.numeric(x$second))
  both_n = sum(as.numeric(x$both))
  season = "999"
} 

if(initial_n != 999 & second_n != 999 & both_n != 999){  
#first
n1 = initial_n
#n1 = 55
#second
n2 = second_n
#n2 = 52
#both
m2 = both_n
#m2 = 51

Nhat = ((n1+1)*(n2+1)/(m2+1)) - 1 
#Nhat #est
shat<-sqrt((1/m2+0.5)+(1/(n2-m2+0.5))+(1/(n1-m2+0.5))+((m2+0.5)/((n1-m2+0.5)*(n2-m2+0.5))))
CI<-n2+n1-m2-0.5+(((n2-m2+0.5)*(n1-m2+0.5))/(m2+0.5))*exp(qnorm(0.025,0.975)*shat)           
#CI #lcl
#Nhat+(Nhat-CI) #ucl

data.frame(n1, n2, m2, Nhat = trunc(Nhat), lcl = trunc(CI), ucl = trunc(Nhat+(Nhat-CI)), subset = i, season)

}
})

LPC_df<-plyr::ldply(LPC_ls, data.frame)
LPC_df<-LPC_df%>%
  mutate(YEAR = as.numeric(substr(.id,1,4)),
         POD = substr(.id,6,length(.id)))%>%
  left_join(yeartally, by = c("YEAR","POD"))

if (exists("LPC_df_ls") == FALSE){
  LPC_df_ls<-LPC_df
  print("FALSE")
} else {
  LPC_df_ls<-LPC_df_ls%>%bind_rows(LPC_df)
  print("TRUE")
}

}

dusky_temp<-read.csv("./data/DUSKY_NHAT_N.csv", header = T, stringsAsFactors = F)

LPC_df_ls<-LPC_df_ls%>%
  bind_rows(dusky_temp)%>%
  filter(!is.na(n))%>%
  distinct()

#LPC_df_ls%>%filter(POD == "DUSKY" & YEAR > 2009 & subset == "Calendar")

last_seen_ind<-lifehist_long%>%
  mutate(LAST_CALFYEAR = case_when(
    month(LAST_DATE) >= 9 ~ as.numeric(LAST_YEAR) + 1,
    TRUE ~ as.numeric(LAST_YEAR)),
          last_year_date = year(LAST_DATE))%>%
  filter(last_year_date == YEAR)%>%
  filter(last_year_date >= min_year)

last_seen_ind%>%filter(last_year_date == reporting_year-1)%>%
  mutate(age = reporting_year-as.numeric(BIRTH_YEAR),
         at_least_age = reporting_year-as.numeric(FIRST_YEAR))%>%
  dplyr::select(POD, NAME, SEX, age, at_least_age, FIRST_YEAR, LAST_DATE, AGECLASS)

last_seen<-last_seen_ind%>%
  #filter(!(LAST_CALFYEAR == 2014 & POD == "DUSKY"))%>%
  distinct(POD, last_year_date, NAME, AGECLASS)%>%
  group_by(POD, last_year_date, AGECLASS)%>%
  tally()

last_seen$AGECLASS<-factor(last_seen$AGECLASS, levels = c("U","A","S-A","W","C","D"))

lastseen_plot<-ggplot(last_seen%>%filter(last_year_date < reporting_year)%>%filter(POD != "DUSKY"))+
  geom_col(mapping = aes(x = as.numeric(last_year_date), y = n, fill = AGECLASS), color = "black", alpha = 0.7)+
  theme_bw()+
  scale_fill_manual(values = ageclass_fill)+
  xlab("Year")+
  ylab("Number of individuals")+
  theme(legend.position = "bottom")

ggsave('./figures/lastseen_plot.png', dpi = 320, width = 160, height = 100, units = 'mm')

########

year_name<-photo_analysis_calfyear_sql%>%
  distinct(ID_NAME, YEAR)%>%
  mutate(minyear = min(YEAR), maxyear = max(YEAR))

year_perdolphin<-lifehist_long%>%
  right_join(year_name, by = c("NAME" = "ID_NAME", "YEAR" = "YEAR"))%>%
  filter(!is.na(minyear))%>%
  filter(YEAR == reporting_year)%>%
  #filter(NAME == "NANCY")%>%
  mutate(AGECLASS = case_when(
   AGECLASS == "NA" & as.numeric(FIRST_YEAR) == reporting_year ~ "C",
    TRUE ~ AGECLASS))

year_perdolphin$AGECLASS<-factor(year_perdolphin$AGECLASS, levels = c("U","A","S-A","W","C","D"))

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

LPC_calf_plot<-ggplot(LPC_df_ls%>%filter(subset == "Calendar"))+
  geom_line(aes(x = YEAR, y = Nhat, color = POD))+
  geom_errorbar(aes(x = YEAR, ymin = lcl, ymax = ucl, color = POD), width = 0.4)+
  geom_line(aes(x = YEAR, y = n, color = POD), linetype = "dashed")+
  geom_point(aes(x = YEAR, y = n, color = POD), shape = 24)+
  scale_color_brewer(palette = 'Dark2') +
  geom_point(data = calves%>%filter(as.numeric(BIRTH_YEAR) <= reporting_year), mapping = aes(x = as.numeric(BIRTH_YEAR), y = n, color = POD), shape = 22)+
  geom_line(data = calves%>%filter(as.numeric(BIRTH_YEAR) <= reporting_year), mapping = aes(x = as.numeric(BIRTH_YEAR), y = n, color = POD), linetype = "dotted")+
  new_scale_color() +
  geom_point(aes(x = YEAR, y = Nhat, color = season))+
  scale_color_manual(values = c("black","purple")) +
  ylab("Number of individuals")+
  xlab("Year")+
  #facet_wrap(~POD, scales = "free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = seq(min(LPC_df_ls$YEAR),max(LPC_df_ls$YEAR),3), minor_breaks = seq(min(LPC_df_ls$YEAR),max(LPC_df_ls$YEAR),1))

ggsave('./figures/LPC_calf_plot.png', dpi = 320, width = 150, height = 120, units = 'mm')

LPC_plot<-LPC_calf_plot+
  ylim(c(50,130))+
  theme(legend.direction = "vertical")

ggsave('./figures/LPC_plot.png', dpi = 320, width = 120, height = 100, units = 'mm')

library(scales)
lastseen_plot_calf<-lastseen_plot+
  geom_point(data = calves%>%filter(as.numeric(BIRTH_YEAR) < reporting_year)%>%filter(POD == "DOUBTFUL"), mapping = aes(x = as.numeric(BIRTH_YEAR), y = n), fill = "black", shape = 22)+
  scale_y_continuous(breaks= pretty_breaks())

ggsave('./figures/lastseen_plot_calf.png', dpi = 320, width = 125, height = 100, units = 'mm')

###
#use below in future years
#survey_data_report<-survey_data%>%filter(YEAR == reporting_year)
#map?

#############
##timeline
datalog<-read.csv('./data/Data_Log.csv', header = T, stringsAsFactors = T)

datalog<-datalog%>%
  dplyr::select(Fiord, Year, Season, Folder, Start_date, End_date)%>%
  tidyr::pivot_longer(cols = ends_with("date"), names_to = "start_end", values_to = "Date")%>%
  mutate(Date = dmy(Date),
         Ordinal = yday(Date),
         MODA = format(as.Date(Date), "%d-%b"))

fiords<-datalog%>%
  filter(Fiord == "DOUBTFUL" | Fiord == "DUSKY" | Fiord == "NANCY" | Fiord == "CHARLES" | Fiord == "DAGG" | Fiord == "CHALKY" | Fiord == "PRESERVATION")
#levels(fiords$Fiord)<-c("MARTINS","MILFORD","BLIGH","GEORGE","CASWELL","CHARLES","NANCY","DOUBTFUL","DAGG","DUSKY","CHALKY","PRESERVATION","STEWART","DUNEDIN")
fiords$Fiord<-ordered(fiords$Fiord, levels = c("CHARLES","NANCY","DOUBTFUL","DAGG","DUSKY","CHALKY","PRESERVATION"))

unique(fiords$Fiord)

data<-fiords
#data<-datalog

timeline<-ggplot(data%>%filter(Year == reporting_year)%>%filter(Fiord == "DUSKY" | Fiord == "DOUBTFUL"))+
  geom_point(aes(x = Ordinal, y = Fiord, group = Folder, color = Season))+
  geom_path(aes(x = Ordinal, y = Fiord, group = Folder, color = Season), size = 2)+
  #facet_wrap(~Fiord, ncol = 2)+
  scale_x_continuous(breaks = c(1,32,60,91,121,152,182,213,244,274,305,335,366), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",""), limits = c(1,366))+
  #scale_y_continuous(breaks = seq(min(data$Year), max(data$Year), by = 1))+
  theme_bw()+
  xlab("")+
  ylab("")+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom")

ggsave('./figures/timeline.png', dpi = 320, width = 160, height = 60, units = 'mm')

###
#tables
trip_summary<-dbReadTable(con, "trip_summary")

effort_days<-trip_summary%>%
  filter(year(DATE) == reporting_year)%>%
  group_by(SURVEY_AREA, TRIP)%>%
  mutate(`Effort (days)` = n(),
         `Date range` = paste0(format(min(DATE),"%d %b"),"–",format(max(DATE),"%d %b")))%>%
  distinct(SURVEY_AREA, TRIP, `Date range`,`Effort (days)`)

sig_days<-photo_analysis_calfyear_sql%>%
  filter(YEAR == reporting_year)%>%
  distinct(SURVEY_AREA, TRIP, DATE)%>%
  group_by(SURVEY_AREA, TRIP)%>%
  tally()%>%
  dplyr::rename(`Sighting (days)` = n)

n_season<-photo_analysis_calfyear_sql%>%
  filter(YEAR == reporting_year)%>%
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

LPC_table<-LPC_df_ls%>%
  filter(n1 != "NA" & subset == "Calendar")%>%
  mutate(Pod = stringr::str_to_title(POD))%>%
  mutate(`95% CI` = paste0(lcl,"–",ucl))%>%
  dplyr::select(YEAR,n1, n2, m2, Nhat, `95% CI`, n)
  
print(knitr::kable(LPC_table, format = "latex")%>%
        kable_styling()%>%
        group_rows("Doubtful Pod", 1, 13) %>%
        group_rows("Dusky pod", 14, 16))

ind_list<-photo_analysis_calfyear_sql%>%
  filter(YEAR == reporting_year)%>%
  distinct(SURVEY_AREA, ID_NAME)%>%
  arrange(SURVEY_AREA, ID_NAME)

save(LPC_df_ls, LPC_calf_plot, calves, last_seen, lastseen_plot, timeline, effort_table, file = paste0("LPC_",reporting_year,".Rdata"))
