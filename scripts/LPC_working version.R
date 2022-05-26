library(odbc);library(dplyr);library(DBI);library(lubridate);library(ggplot2)

source('~/Documents/git_otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value
photo_analysis_calfyear_sql<-dbReadTable(con, "photo_analysis_calfyear")
#source('~/Documents/git_otago/Fiordland_reporting/scripts/life_history_ageclass update.R', local = TRUE)$value
lifehist<-dbReadTable(con, "life_history_ageclass")

scenario<-c("Calendar","Calfyear")
#scenario<-c("Calendar")

for (i in scenario){
  print(i)
i = "Calendar"
#currently defining season by the month of trip start date
yearcap<-photo_analysis_calfyear_sql%>%
  left_join(lifehist, by = c("ID_NAME" = "NAME"))%>%
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
  #x<-season_cap$`2021.DOUBTFUL`
  
if(i == "Calendar"){
  if(999 %in% x$WINTER == FALSE) {
      initial_n = sum(as.numeric(x$WINTER))
  } else if (999 %in% x$AUTUMN == FALSE){
    initial_n = sum(as.numeric(x$AUTUMN))
  } else {
    initial_n = 999 
  }

  if(999 %in% x$SPRING == FALSE) {
    second_n = sum(as.numeric(x$SPRING))
  } else {
    second_n = 999
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

data.frame(n1, n2, m2, Nhat, lcl = CI, ucl = Nhat+(Nhat-CI), subset = i)

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

plot<-ggplot(LPC_df_ls%>%filter(subset == "Calendar"))+
  geom_point(aes(x = YEAR, y = Nhat, color = POD), shape = 19)+
  geom_line(aes(x = YEAR, y = Nhat, color = POD))+
  geom_errorbar(aes(x = YEAR, ymin = lcl, ymax = ucl, color = POD))+
  geom_point(aes(x = YEAR, y = n, color = POD), shape = 24)+
  geom_line(aes(x = YEAR, y = n, color = POD), linetype = "dashed")+
  ylab("Number of individuals")+
  xlab("Year")+
  #facet_wrap(~POD, scales = "free")+
  theme_bw()#+
  # scale_shape_manual(values = c(24, 19), breaks = c("Nhat","n"))

ggsave(paste0('./figures/',i,'.png'))

LPC_df_ls%>%filter(POD == "DUSKY" & YEAR > 2009 & subset == "Calendar")
