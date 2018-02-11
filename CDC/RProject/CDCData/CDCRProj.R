---
  title: "Days of Code"
author: "Carlos Santillan"
date: "February 10, 2018"
output: html_document
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

setwd("F:/Development/days_of_code")
```

## R Libaries


```{r libraries}

library(tidyverse)
library(ggplot2)
library(fiftystater)

data("fifty_states") # this line is optional due to lazy data loading
```

## load the data



```{r loaddata, echo=FALSE}

cdc.state.data        <- read_tsv("CDC/data/statefull.txt")
colnames(cdc.state.data) <- make.names(colnames(cdc.state.data))
cdc.state.data$Notes  <- cdc.state.data$Notes <- NULL
cdc.state.data$Deaths <- as.integer(cdc.state.data$Deaths)
cdc.state.data <- cdc.state.data %>% separate(Month.Code,c("yr","Month.Code"))

cdc.state.data <- subset(cdc.state.data,select=c("State","Year","Month.Code","Deaths"))


```



```{r explore, echo=FALSE}
str(cdc.data)
summary(cdc.state.data)
print(cdc.state.data %>% filter(Year==2016) %>% select(Deaths) %>%sum())
print(cdc.state.data %>% filter(Year==2016,State=="Texas") %>% select(Deaths) %>%sum())
print(cdc.state.data %>% filter(Year==2016,State=="Texas") %>% group_by(Month.Code)  %>% select(Month.Code,Deaths)) %>%sum()
```



```{r explore2016, echo=FALSE}
grp_state <- cdc.state.data %>% filter(Year==2016)%>% group_by(State,Month.Code)%>% select(Deaths)
summary(grp_state)

```



```{r plot, echo=FALSE}
tx <- filter(grp_state,State=="Texas")
ggplot(tx,aes(x=Month.Code, y=Deaths))+
  geom_point()

grp_state2 <-grp_state %>%group_by(State)%>% summarise(Deaths= sum(Deaths))


ggplot(grp_state2,aes(x=State),fill=State)+
  geom_bar()


```


```{r plot2, echo=FALSE}
tx <- filter(grp_state,State=="Texas")
ggplot(tx,aes(x=Month.Code, y=Deaths))+
  geom_point()

grp_state2 <-grp_state %>%group_by(State)%>% summarise(Deaths= sum(Deaths))
grp_state2$State <- tolower(grp_state2$State)

p <- ggplot(grp_state2, aes(map_id = State)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = Deaths), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())

p
p + fifty_states_inset_boxes() 

```


