summarise_if(is.numeric, sum)


cdc.state.data %>% filter(Year=="2016") %>% group_by(State) %>% 
  select(State,Lat,Long,Deaths)  %>%
  summarise(Lat=Lat,Long=Long,Deaths=sum(Deaths))


agg_data <- cdc.state.data %>% group_by(State) %>% summarise(Deaths=sum(Deaths)) 

%>%  left_join(state.geom,by=c('State'='name'))



agg_data <- cdc.state.data %>% group_by(State) %>%   summarise(Deaths=sum(Deaths))  %>%  left_join(state.geom,by=c('State'='name'))



plotdata <- cdcAnnualData %>% filter(State %in% c("Texas","California"))
ggplot(plotdata)+
  #    geom_bar(stat="identity") +
  geom_smooth(mapping=aes(x=Year, y=Deaths,group=State,color=State))
#  stat_smooth(method="loess",show.legend=TRUE) +
ggtitle("Deaths attributed to opiods")


cdc2 <- cdc %>% filter(Year=="1998")


library(geojsonio )


states <- geojsonio::geojson_read("json/us-states.geojson", what = "sp")
class(states)


df <-cdcAnnualData %>%  arrange(State,desc(Year))  %>% group_by(State) %>%mutate(PercentChange = ((Deaths - lead(Deaths)) * 100 / lead(Deaths)))

df %>%  mutate (PercentChange =  format((Deaths - lead(Deaths)) * 100 / lead(Deaths),
                                        big_mark = "," ,  digits = 2))




setwd("F:/Development/days_of_code/CDC/RProject/CDCData")
cdcMonthlyStateData <- readRDS("Data/cdcMonthlyStateData.rds")

cdcAnnualData <- readRDS("Data/cdcAnnualData.rds")
stategeoms <- geojsonio::geojson_read("Data/us-states.geojson", what = "sp")
state.geom <- readRDS("Data/state_geom.rds")
States<- cdcAnnualData %>% distinct(State)
Years<-  cdcAnnualData  %>% ungroup() %>% select(Year) %>% distinct(Year) %>% mutate_all(as.character)
Months<- cdcMonthlyStateData %>% distinct(Month.Code)

cdcMonthlyStateData <-cdcMonthlyStateData %>%  left_join(state.geom,by=c('State'='name')) %>% select(
  State,YearMonth,Year,Month.Code,Deaths,Lat=latitude,Long=longitude)


df <-cdcAnnualData  %>% filter(Year=="2016")

stategeomsJoined <- stategeoms %>% left_join(df,by=c('name','State'))


stategeomsJoined <- df %>% left_join(stategeoms,by=c('State'='name'))
