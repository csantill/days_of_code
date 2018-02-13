library(tidyverse)
library(ggplot2)
library(fiftystater)
library(leaflet)

setwd("F:/Development/days_of_code/CDC/RProject/CDCData")
cdcMonthlyStateData <- readRDS("Data/cdcMonthlyStateData.rds")

cdcAnnualData <- readRDS("Data/cdcAnnualData.rds")

state.geom <- readRDS("Data/state_geom.rds")
States<- cdcMonthlyStateData %>% distinct(State)
Years<-  cdcMonthlyStateData %>% distinct(Year) %>% mutate_all(as.character)
Months<- cdcMonthlyStateData %>% distinct(Month.Code)

cdcMonthlyStateData <-cdcMonthlyStateData %>%  left_join(state.geom,by=c('State'='name')) %>% select(
  State,YearMonth,Year,Month.Code,Deaths,Lat=latitude,Long=longitude)

