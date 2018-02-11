library(tidyverse)
library(ggplot2)
library(fiftystater)
library(leaflet)
setwd("F:/Development/days_of_code")
cdc.state.data <- readRDS("CDC/RProject/CDCData/Data/cdc.state.data.rds")
states_geom <- readRDS("CDC/RProject/CDCData/Data/state_geom.rds")
States<- cdc.state.data %>% distinct(State)
Years<- cdc.state.data %>% distinct(Year) %>%mutate_all(as.character)
Months<- cdc.state.data %>% distinct(Month.Code)

data("fifty_states") # this line is optional due to lazy data loading