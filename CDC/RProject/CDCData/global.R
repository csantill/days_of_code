library(tidyverse)
library(ggplot2)
library(fiftystater)
library(leaflet)
library(geojsonio)

setwd("F:/Development/days_of_code/CDC/RProject/CDCData")
cdcMonthlyStateData <- readRDS("Data/cdcMonthlyStateData.rds")

cdcAnnualData <- readRDS("Data/cdcAnnualData.rds")
#print("CDDCGlobal")
#str(cdcAnnualData)
stategeoms <- geojsonio::geojson_read("Data/us-states.geojson", what = "sp")
state.geom <- readRDS("Data/state_geom.rds")
States<- cdcAnnualData %>% distinct(State)
Years<-  cdcAnnualData  %>% ungroup() %>% select(Year) %>% distinct(Year) %>% mutate_all(as.character)
#Years<-  cdcAnnualData %>% distinct(Year) %>% mutate_all(as.character)
Months<- cdcMonthlyStateData %>% distinct(Month.Code)

cdcMonthlyStateData <-cdcMonthlyStateData %>%  left_join(state.geom,by=c('State'='name')) %>% select(
  State,YearMonth,Year,Month.Code,Deaths,Lat=latitude,Long=longitude)

#cdcAnnualData<- sp::merge(stategeoms,cdcAnnualData,by.x='name',by.y='State')
