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

