source('./scripts/connect to MySQL.R', local = TRUE)$value
#lifehist<-dbReadTable(con, "age_sex")

assign_ageclass<-function(x){
  x%>%mutate(
  this_year_age = case_when(
    (!is.na(BIRTH_YEAR) | BIRTH_YEAR != '') ~ as.numeric(phyear) - as.numeric(BIRTH_YEAR)),
  this_year_age_est = case_when(
    (is.na(BIRTH_YEAR) | BIRTH_YEAR == '') ~ as.numeric(phyear) - as.numeric(FIRST_YEAR)),
  this_year_ageclass = case_when(
    (this_year_age >= 9 | this_year_age_est >= 8) ~ 'A',
    (this_year_age < 9 & this_year_age >= 3) ~ 'S-A',
    (this_year_age < 3 & this_year_age > 0) ~ 'W',
    this_year_age == 0 ~ 'C',
    TRUE ~ 'U'))%>%
  mutate(this_year_ageclass = case_when(
    as.numeric(phyear) < as.numeric(FIRST_YEAR) ~ 'NA',
    as.numeric(phyear) > as.numeric(DEATH_YEAR) ~ 'D',
    TRUE ~ this_year_ageclass
  ))
}

#############

lifehist_sql<-dbReadTable(con, "life_history")

phyear = min(lifehist_sql$FIRST_YEAR)
print(phyear)

lifehist<-assign_ageclass(lifehist_sql)

lifehist<-lifehist%>%
  mutate(SURVEY_AREA = case_when(
    ENTRYNO < 2000 ~ 'Doubtful',
    ENTRYNO >= 2000 & ENTRYNO < 3000 ~ 'Dusky'))%>%
  dplyr::select(SURVEY_AREA, NAME, CODE, SEX, MOM, BIRTH_YEAR, FIRST_YEAR, DEATH_YEAR, this_year_ageclass)

names(lifehist)[length(names(lifehist))]<-phyear

##after 1990

for(i in (min(as.numeric(lifehist$FIRST_YEAR))+1):year(Sys.Date())){
  phyear = i
  print(phyear)
  print(year(Sys.Date()))
  
  lifehist<-assign_ageclass(lifehist)
  
  lifehist<-lifehist%>%
    dplyr::select(-this_year_age, -this_year_age_est)

  names(lifehist)[length(names(lifehist))]<-phyear
  lifehist
}

TYPES = list(SURVEY_AREA="varchar(20)", NAME="varchar(45)", CODE="varchar(10)", SEX="varchar(5)",MOM="varchar(45)",
             BIRTH_YEAR="varchar(4)", FIRST_YEAR="varchar(4)", DEATH_YEAR="varchar(4)") 

lifehist_sql<-dbWriteTable(con, name = "life_history_ageclass", value = lifehist, field.types = TYPES, row.names = FALSE, overwrite = T)