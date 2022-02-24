# load libraries
library(vtable)
library(jtools)
library(tidyverse)
library(purrr)
library(fixest)
library(readxl)
library(dplyr)
library(lubridate)

# Uploading trends data
list.files(path= "Lab3_Rawdata")
flist <-list.files(path= "Lab3_Rawdata", full.names = TRUE, pattern = "trends")
trendData <- flist %>%
  map(read_csv) %>%
  bind_rows()

#Uploading Scorecard data
scorecardData <-read_csv("Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv")
scorecardData <-rename(scorecardData, opeid = OPEID)
scorecardData <-rename(scorecardData, unitid = UNITID)

#Uploading id-name
id_name_link <- read.csv("Lab3_Rawdata/id_name_link.csv")
id_name_link <- id_name_link %>%
  group_by(schname) %>%
  mutate(N=n()) %>%
  filter(N==1)

#Joining data tables
Data1 <- trendData %>%
  left_join(id_name_link, by = "schname")

fullData <- Data1 %>%
  left_join(scorecardData, by = "opeid")

##Data Cleaning

##Standardize indexes
fullData2 <- fullData %>%
  group_by(schname) %>%
  mutate(index.s = (index - mean(index, na.rm = TRUE))/sd(index, na.rm = TRUE))

##Select bachelor data
bachelorData <- filter(fullData2, PREDDEG == 3)

#map out date
bachelorData <- bachelorData %>%
  mutate (date = str_sub(monthorweek, 1, 10)) %>%
  mutate(date = ymd(date)) %>%
  mutate(after_scorecard = date > ymd ('2015-08-31'))

#remove unitid
bachelorData <- subset(bachelorData, select = -unitid.y)

#map out earnings
bachelorData<-rename(bachelorData, median_earnings = "md_earn_wne_p10-REPORTED-EARNINGS")
bachelorData$high_income <-  ifelse(bachelorData$median_earnings > 64896, "High", "Low")

write.csv(bachelorData, "Clean_data")
  

