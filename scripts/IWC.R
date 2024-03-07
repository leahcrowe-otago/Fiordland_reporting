## IWC

library(odbc);library(dplyr);library(DBI);library(lubridate)

# pull data from db
source('C:/Users/leahm/OneDrive - University of Otago/Documents/git-otago/Fiordland_reporting/scripts/life_history_ageclass update.R', local = TRUE, verbose = F)$value
lifehist<-lifehist

#rename year in question column as ageclass
names(lifehist)[length(names(lifehist))]<-"ageclass" 


#yearly summary
year = 2023

# survey data ----
source('./scripts/connect to MySQL.R', local = TRUE)$value
survey<-dbReadTable(con, "survey_data")%>%
  mutate(YEAR = year(DATE))%>%
  filter(YEAR == year)

# photo data ----
source('./scripts/connect to MySQL.R', local = TRUE)$value
photo_analysis<-dbReadTable(con, "photo_analysis")%>%
  mutate(YEAR = year(DATE))%>%
  filter(YEAR == year)
head(photo_analysis)

## use for filtering fiords below
photo_analysis%>%
  distinct(SURVEY_AREA)

#northern sightings ----
# Awarua Point (northern end of Big Bay) to Anxiety Point (southern entrance of Nancy Sound)

## photo ID ----
northern<-photo_analysis%>%
  filter(YEAR == year)%>%
  filter(SURVEY_AREA == 'GEORGE' | SURVEY_AREA == "NANCY")%>%
  filter(ID_NAME != "CULL" & ID_NAME != "UNMA" & ID_NAME != "")

## number of days ----
northern%>%
  distinct(DATE)

### survey north of Anxiety Point ----
#might need to adjust the point for comprehensive query as the head of Nancy is more souther than Anxiety point
survey%>%
  filter(LATITUDE > -45.1062)%>%
  distinct(DATE)

## number of sightings ---- 
# might be the same as number of days if not systematic
northern%>%
  distinct(GROUP,DATE)

## ageclass breakdown ----
northern%>%
  left_join(lifehist, by = c('ID_NAME' = 'NAME'))%>%
  distinct(SURVEY_AREA, POD, ID_NAME, ageclass)%>%
  group_by(SURVEY_AREA, POD, ageclass)%>%
  tally()

## sex breakdown ----
northern%>%
  left_join(lifehist, by = c('ID_NAME' = 'NAME'))%>%
  distinct(SURVEY_AREA, POD, ID_NAME, SEX)%>%
  group_by(SURVEY_AREA, POD, SEX)%>%
  tally()


#middle fiord sightings ----

## photo ID ----
middle<-photo_analysis%>%
  filter(YEAR == year)%>%
  filter(SURVEY_AREA == 'DOUBTFUL')%>%
  filter(ID_NAME != "CULL" & ID_NAME != "UNMA" & ID_NAME != "")

## number of days ----
middle%>%
  distinct(DATE)

### survey south of Anxiety Point and north of Black Point----
survey%>%
  #southern latitude of the head of Nancy
  filter((LATITUDE < -45.1851 & 
  #northern point of the head of Dagg         
         LATITUDE > -45.3809) | 
  #include Crooked, Hall, and Deep Cove  
    (LATITUDE < -45.3809 & LATITUDE > -45.4835 & LONGITUDE > 166.9169)) %>%
  distinct(DATE)

## number of sightings ---- 
# might be the same as number of days if not systematic
middle%>%
  distinct(GROUP,DATE)

## ageclass breakdown ----
middle%>%
  left_join(lifehist, by = c('ID_NAME' = 'NAME'))%>%
  distinct(SURVEY_AREA, POD, ID_NAME, ageclass)%>%
  group_by(SURVEY_AREA, POD, ageclass)%>%
  tally()

## sex breakdown ----
middle%>%
  left_join(lifehist, by = c('ID_NAME' = 'NAME'))%>%
  distinct(SURVEY_AREA, POD, ID_NAME, SEX)%>%
  group_by(SURVEY_AREA, POD, SEX)%>%
  tally()

#southern fiord sightings ----

## photo ID ----
southern<-photo_analysis%>%
  filter(YEAR == year)%>%
  filter(SURVEY_AREA == 'DAGG' | SURVEY_AREA == "DUSKY" | SURVEY_AREA == "CHALKY" | SURVEY_AREA == "PRESERVATION")%>%
  filter(ID_NAME != "CULL" & ID_NAME != "UNMA" & ID_NAME != "")

## number of days ----
southern%>%
  distinct(DATE)

## number of sightings ---- 
# might be the same as number of days if not systematic
southern%>%
  distinct(GROUP, DATE)

### survey south of Black Point----
survey%>%
  filter(LATITUDE > -45.31667)%>%
  distinct(DATE)

## ageclass breakdown ----
southern%>%
  left_join(lifehist, by = c('ID_NAME' = 'NAME'))%>%
  distinct(POD, ID_NAME, ageclass)%>%
  group_by(POD, ageclass)%>%
  tally()

## sex breakdown ----
southern%>%
  left_join(lifehist, by = c('ID_NAME' = 'NAME'))%>%
  distinct(POD, ID_NAME, SEX)%>%
  group_by(POD, SEX)%>%
  tally()

# catalogue numbers ----

lifehist%>%
  group_by(POD)%>%
  tally()


