library(odbc);library(dplyr);library(DBI);library(lubridate)

source('C:/Users/leahm/OneDrive - University of Otago/Documents/git-otago/Fiordland_reporting/scripts/connect to MYSQL.R', local = TRUE, verbose = F)$value

assign_ageclass<-function(x){
  x%>%
  mutate(
  this_year_age = case_when(
    (!is.na(BIRTH_YEAR) | BIRTH_YEAR != '') ~ as.numeric(phyear_lh) - as.numeric(BIRTH_YEAR)),
  this_year_age_est = case_when(
    (is.na(BIRTH_YEAR) | BIRTH_YEAR == '') ~ as.numeric(phyear_lh) - as.numeric(FIRST_YEAR)),
  this_year_ageclass = case_when(
    (this_year_age >= 9 | this_year_age_est >= 8 | phyear_lh >= FIRST_CALF) ~ 'A',
    (this_year_age < 9 & this_year_age >= 3) ~ 'S-A',
    (this_year_age < 3 & this_year_age > 0) ~ 'J',
    this_year_age == 0 ~ 'C',
    TRUE ~ 'U'))%>%
  mutate(this_year_ageclass = case_when(
    as.numeric(phyear_lh) < as.numeric(FIRST_YEAR) | as.numeric(phyear_lh) < as.numeric(BIRTH_YEAR) ~ 'NA',
    as.numeric(phyear_lh) > as.numeric(DEATH_YEAR) ~ 'D',
    TRUE ~ this_year_ageclass
  ))
}

#############

lifehist_sql<-dbReadTable(con, "life_history")
pa_cy<-dbReadTable(con, "photo_analysis_calfyear")

last<-pa_cy%>%
  group_by(ID_NAME)%>%
  dplyr::summarise(LAST_YEAR = as.character(max(CALFYEAR)), LAST_DATE = as.character(max(DATE)), FIRST_DATE = as.character(min(DATE)))

phyear_lh = min(lifehist_sql$FIRST_YEAR)
print(phyear_lh)

mom_first_year<-lifehist_sql%>%filter(MOM != "")%>%group_by(MOM)%>%dplyr::summarise(FIRST_CALF = min(BIRTH_YEAR))

lifehist_sql<-lifehist_sql%>%left_join(mom_first_year, by = c("NAME" = "MOM"))
lifehist<-assign_ageclass(lifehist_sql)
lifehist$DEATH_YEAR[lifehist$DEATH_YEAR==""] <- NA

lifehist<-lifehist%>%
  mutate(POD = case_when(
    ENTRYNO < 2000 ~ 'DOUBTFUL',
    ENTRYNO >= 2000 & ENTRYNO < 3000 ~ 'DUSKY',
    ENTRYNO >= 3000 & ENTRYNO < 4000 ~ 'NORTHERN'))%>%
  left_join(last, by = c("NAME" = "ID_NAME"))%>%
  mutate(LAST_YEAR = case_when(
    !is.na(DEATH_YEAR) ~ as.character(DEATH_YEAR),
    TRUE ~ LAST_YEAR
  ))%>%
  mutate(LAST_DATE = case_when(
    NAME == "D-20110328" ~ "2011-03-28",
    TRUE ~ LAST_DATE
  ))%>%
  dplyr::select(POD, NAME, CODE, SEX, MOM, FIRST_CALF, BIRTH_YEAR, FIRST_YEAR, FIRST_DATE, DEATH_YEAR, LAST_YEAR, LAST_DATE, this_year_ageclass)

inutero<-lifehist%>%
  left_join(pa_cy, by = c("MOM" = "ID_NAME"), relationship = "many-to-many")%>%
  filter(DATE < FIRST_DATE)%>%
  group_by(NAME)%>%
  dplyr::select(POD, NAME, CODE, SEX, MOM, FIRST_CALF, BIRTH_YEAR, FIRST_YEAR, FIRST_DATE, DEATH_YEAR, LAST_YEAR, LAST_DATE, DATE)%>%
  distinct()%>%
  filter(DATE == max(DATE))%>%
  mutate(INUTERO = DATE)%>%
  dplyr::select(NAME, INUTERO)%>%
  as.data.frame()

lifehist<-lifehist%>%
  left_join(inutero, by = "NAME")%>%
  dplyr::select(POD, NAME, CODE, SEX, MOM, FIRST_CALF, BIRTH_YEAR, INUTERO, FIRST_YEAR, FIRST_DATE, DEATH_YEAR, LAST_YEAR, LAST_DATE, this_year_ageclass)

names(lifehist)[length(names(lifehist))]<-phyear_lh

##after 1990

for(i in (min(as.numeric(lifehist$FIRST_YEAR))+1):year(Sys.Date())){
  phyear_lh = i
  print(phyear_lh)
  print(year(Sys.Date()))
  
  lifehist<-assign_ageclass(lifehist)
  
  lifehist<-lifehist%>%
    dplyr::select(-this_year_age, -this_year_age_est)

  names(lifehist)[length(names(lifehist))]<-phyear_lh
  lifehist
}

##once last_year is integrated, need to make some changes to the above so age class is only populated between birth/first and last  

avg_primo_age_df<-lifehist%>%
  filter(!is.na(FIRST_CALF))%>%
  mutate(primo_age = as.numeric(FIRST_CALF)-as.numeric(BIRTH_YEAR))%>%
  filter(!is.na(primo_age))%>%
  mutate(avg_primo_age = mean(primo_age),
         min_primo_age = min(primo_age),
         max_primo_age = max(primo_age))%>%
  dplyr::select(POD, NAME, FIRST_CALF, BIRTH_YEAR, INUTERO, primo_age, avg_primo_age, min_primo_age, max_primo_age)

avg_primo_age<-unique(avg_primo_age_df$avg_primo_age)
avg_primo_age

dbDisconnect(con)

