library(tidyverse)
library(ggplot2)
library(fiftystater)
library(leaflet)

setwd("F:/Development/days_of_code/CDC/RProject/CDCData")
cdc.state.data <- readRDS("Data/cdc.state.data.rds")
state.geom <- readRDS("Data/state_geom.rds")
States<- cdc.state.data %>% distinct(State)
Years<-  cdc.state.data %>% distinct(Year) %>% mutate_all(as.character)
Months<- cdc.state.data %>% distinct(Month.Code)

cdc.state.data <-cdc.state.data %>%  left_join(state.geom,by=c('State'='name')) %>% select(
  State,YearMonth,Year,Month.Code,Deaths,Lat=latitude,Long=longitude)
#State,YearMonth,Year,Month.Code,Deaths,Lat,Long

#data("fifty_states") # this line is optional due to lazy data loading

