library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)



function(input, output, session) {
  

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
  # statesInBounds <- reactive({
  #   if (is.null(input$map_bounds))
  #     return(()[FALSE,])
  #   bounds <- input$map_bounds
  #   latRng <- ryearFilterange(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)
  #   
  #   subset(yearFilter(),
  #          Lat >= latRng[1] & Lat <= latRng[2] &
  #            Long >= lngRng[1] & Long <= lngRng[2])
  # })
  
  yearFilter <- reactive({
    YearBy <- input$year
    #cdc.filtered.data <- cdcMonthlyStateData%>% filter(Year==YearBy) 
   # cdcAnnualData <- cdcAnnualData %>% filter(Year==YearBy) 
    cdc.filtered.data <- cdcMonthlyStateData%>% filter(Year==YearBy) 
  })
  
  yearFilterAnnual <- reactive({
    YearBy <- input$year

    cdc.filtered.data <- cdcAnnualData%>% filter(Year==YearBy) 
  })
  
  output$scatterDeaths <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(yearFilter()) == 0)
      return(NULL)

    plotdata <- yearFilter()%>% group_by(Month.Code)%>% select(Month.Code,Deaths) %>%summarise(Deaths= sum(Deaths))
    ggplot(plotdata,aes(x=Month.Code, y=Deaths,group=1))+
      geom_line(stat="identity") +
      stat_smooth(method="loess") +
      ggtitle("Deaths attributed to opiods",input$year)
    })
  

  # This observer is responsible for maintaining the circles 
  observe({
    YearBy <- input$year
   
      agg_data <- yearFilter() %>% group_by(State) %>% 
                summarise(Deaths=sum(Deaths))  %>%  
                left_join(state.geom,by=c('State'='name')) %>% 
                select(State,Deaths,Lat=latitude,Long=longitude)
      
      radius <- agg_data[["Deaths"]] / sum(agg_data[["Deaths"]]) * 2300000
      leafletProxy("map", data = agg_data) %>%
      clearShapes() %>%
      addCircles(~Long, ~Lat, radius=radius, layerId=~State,
                 stroke=FALSE, fillOpacity=0.4)

  })
  
  PrettyNumbers <- function(val)
  {
    format(val,  big.mark=",",small.mark=".",  small.interval=3)
  }  
  # Show a popup at the given location
  showStatePopup <- function(state, lat, lng) {
    selectedState <- yearFilterAnnual()[yearFilterAnnual()$State == state,]
    str(yearFilterAnnual())
    content <- as.character(tagList(
      tags$h2(state),
      tags$h3("Year:", as.integer(selectedState$Year)),
      tags$h4("Deaths:", PrettyNumbers(as.integer(selectedState$Deaths))) ,
      tags$h4("Population:", PrettyNumbers(selectedState$Population)) ,
      tags$h4("Crude Rate:", selectedState$Crude.Rate) 
                                    
                                  
      )
    )
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
  
  
  ## State Date Explorer  #################
  explorestatedatafilter <- reactive({
    
    print("here")
    print(input$usstates)
    print(input$explorerstateyears)
    cdcAnnualData %>%
      filter(
        input$usstates == "" | State == input$usstates  ,
        is.null(input$explorerstateyears) | Year %in% input$explorerstateyears
      )
    
  })  
  
  output$scatterExplorerStateDeaths <- renderPlot({
    if (nrow(explorestatedatafilter()) == 0)
      return(NULL)
    
    plotdata <- explorestatedatafilter()%>% group_by(Year)%>% select(Year,Deaths) %>%summarise(Deaths= sum(Deaths))
    ggplot(plotdata,aes(x=Year, y=Deaths,group=1))+
      geom_line(stat="identity") +
      stat_smooth(method="loess",show.legend=TRUE) +
      ggtitle("Deaths attributed to opiods")
  })
  
  output$cdcStatetable <- DT::renderDataTable({
    if (nrow(explorestatedatafilter()) == 0)
      return(NULL)
    str(explorestatedatafilter())
    df <-   explorestatedatafilter() %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-state="', State,'" data-year="', Year, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(lengthMenu  = c(18, 36, 72, 144),ajax = list(url = action)), escape = FALSE) %>% formatRound(c("Deaths","Population"),interval = 3, mark = ",",digits=0)
  })
  
  ## Data Explorer ###########################################
  observe({
    if (is.null(input$goto))
      return()
    updateSelectInput(session,"year",selected= input$goto$year)
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      state <- input$goto$state
      lat <- input$goto$lat
      lng <- input$goto$lng
      showStatePopup(state, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  exploredatafilter <- reactive({

    cdcAnnualData %>%
      filter(
        is.null(input$states) | State %in% input$states
      )
    
  })  
  output$scatterExplorerDeaths <- renderPlot({
    if (nrow(exploredatafilter()) == 0)
      return(NULL)

    plotdata <- exploredatafilter()%>% group_by(Year)%>% select(Year,Deaths) %>%summarise(Deaths= sum(Deaths))
    ggplot(plotdata,aes(x=Year, y=Deaths,group=1))+
      geom_line(stat="identity") +
      stat_smooth(method="loess",show.legend=TRUE) +
      ggtitle("Deaths attributed to opiods")
  })
  

  
  output$cdctable <- DT::renderDataTable({
    str(exploredatafilter())
    df <-   exploredatafilter() %>%
    mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-state="', State,'" data-year="', Year, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(lengthMenu  = c(18, 36, 72, 144),ajax = list(url = action)), escape = FALSE) %>% formatRound(c("Deaths","Population"),interval = 3, mark = ",",digits=0)
  })
}
