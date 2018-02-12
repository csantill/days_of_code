library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
#set.seed(100)
#zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
#zipdata <- zipdata[order(zipdata$centile),]



function(input, output, session) {
  
  # Session GlobaL filtered data (Default to 2016)
 # cdc.filtered.data <- cdc.state.data %>% filter(Year=="2016")
  
  ## Interactive Map ###########################################

  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  statesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(yearFilter()[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(yearFilter(),
           Lat >= latRng[1] & Lat <= latRng[2] &
             Long >= lngRng[1] & Long <= lngRng[2])
  })
  
  yearFilter <- reactive({
    YearBy <- input$year
    cdc.filtered.data <- cdc.state.data%>% filter(Year==YearBy) 
  })
  

  
  output$scatterDeaths <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(statesInBounds()) == 0)
      return(NULL)
    str(statesInBounds())
    plotdata <- statesInBounds()%>% group_by(Month.Code)%>% select(Deaths) %>%summarise(Deaths= sum(Deaths))
    ggplot(plotdata,aes(x=Month.Code, y=Deaths,group=1))+
      geom_line(stat="identity") +
      stat_smooth() +
      ggtitle("Deaths attributed to opiods",input$year)
    })
  

  # This observer is responsible for maintaining the circles 
  observe({
    YearBy <- input$year
   
    #  cdc.filtered.data <- cdc.state.data%>% filter(Year==YearBy) 
    #  str(cdc.filtered.data)
      
      agg_data <- yearFilter() %>% group_by(State) %>% 
        summarise(Deaths=sum(Deaths))  %>%  left_join(state.geom,by=c('State'='name')) %>% select(
          State,Deaths,Lat=latitude,Long=longitude)
      
      radius <- agg_data[["Deaths"]] / sum(agg_data[["Deaths"]]) * 2300000
      leafletProxy("map", data = agg_data) %>%
      clearShapes() %>%
      addCircles(~Long, ~Lat, radius=radius, layerId=~State,
                 stroke=FALSE, fillOpacity=0.4)

  })
  
  # Show a popup at the given location
  showStatePopup <- function(state, lat, lng) {
    selectedState <- yearFilter()[yearFilter()$State == state,]
    content <- as.character(tagList(
      tags$h4("Deaths:", as.integer(sum(selectedState$Deaths))
    #  tags$strong(HTML(sprintf("%s, %s %s",
     #                          selectedState$Year, selectedState$State, selectedState$Month.Code
    #  )
    #)
    )
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = state)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showStatePopup(event$id, event$lat, event$lng)
    })
  })
  
  
  ## Data Explorer ###########################################
  

  
  observe({
    if (is.null(input$goto))
      return()
    print("the year is **********")
    print(input$goto$year)
    updateSelectInput(session,"years",selected= input$goto$year)
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      state <- input$goto$state
      lat <- input$goto$lat
      lng <- input$goto$lng

     # input$year <- input$goto$year
      showStatePopup(state, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  exploredatafilter <- reactive({
    
    cdc.state.data %>%
      filter(
        is.null(input$states) | State %in% input$states,
        is.null(input$exploreryears) | Year %in% input$exploreryears
      )
    
  })  
  output$scatterExplorerDeaths <- renderPlot({
    # If no zipcodes are in view, don't plot
    #if (nrow(exploredatafilter()) == 0)
    #  return(NULL)
    #str(statesInBounds())
    print("data filter")
    head(exploredatafilter())
    plotdata <- exploredatafilter()%>% group_by(Year)%>% select(Year,Deaths) %>%summarise(Deaths= sum(Deaths))
    ggplot(plotdata,aes(x=Year, y=Deaths,group=1))+
      geom_line(stat="identity") +
      stat_smooth(method="lm") +
      ggtitle("Deaths attributed to opiods")
  })
  

  
  output$cdctable <- DT::renderDataTable({
    df <-   exploredatafilter() %>%
    mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-state="', State,'" data-year="', Year, ',"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-state="', State, ',"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
