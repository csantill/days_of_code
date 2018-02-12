summarise_if(is.numeric, sum)


cdc.state.data %>% filter(Year=="2016") %>% group_by(State) %>% 
  select(State,Lat,Long,Deaths)  %>%
  summarise(Lat=Lat,Long=Long,Deaths=sum(Deaths))


agg_data <- cdc.state.data %>% group_by(State) %>% summarise(Deaths=sum(Deaths)) 

%>%  left_join(state.geom,by=c('State'='name'))



agg_data <- cdc.state.data %>% group_by(State) %>%   summarise(Deaths=sum(Deaths))  %>%  left_join(state.geom,by=c('State'='name'))