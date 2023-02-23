library(odbc);library(dplyr);library(DBI);library(lubridate)

source('~/Documents/git_otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value
#lifehist<-dbReadTable(con, "age_sex")

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

lifehist%>%filter(NAME == "D-20220115")
lifehist%>%filter(NAME == "NANCY")
lifehist%>%filter(NAME == "CORKSCREW")

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


TYPES = list(POD="varchar(20)", NAME="varchar(45)", CODE="varchar(10)", SEX="varchar(5)",MOM="varchar(45)",FIRST_CALF="varchar(4)",
             BIRTH_YEAR="varchar(4)", FIRST_YEAR="varchar(4)", FIRST_DATE="varchar(10)", DEATH_YEAR="varchar(4)", LAST_YEAR="varchar(4)", 
             LAST_DATE="varchar(10)") 

lifehist_sql<-dbWriteTable(con, name = "life_history_ageclass", value = lifehist, field.types = TYPES, row.names = FALSE, overwrite = T)

##once last_year is integrated, need to make some changes to the above so age class is only populated between birth/first and last  

