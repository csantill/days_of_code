library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(sf)


function(input, output, session) {
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(stategeoms) %>% 
      addPolygons(  weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                    # ,
                    # highlight = highlightOptions(
                    #   weight = 5,
                    #   color = "#666",
                    #   dashArray = "",
                    #   fillOpacity = 0.7,
                    #   bringToFront = TRUE)
                    ) %>%
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      setView(lng = -93.85,
              lat = 37.45,
              zoom = 4)
  })
  
  
  yearFilter <- reactive({
    YearBy <- input$year
    cdc.filtered.data <-
      cdcMonthlyStateData %>% filter(Year == YearBy)
  })
  
  gottoUpdatedMap <- reactive({
    StateBy <- input$monthlyState
    Year <- input$year
    if (nrow(MonthlyDataStateFilter()) == 0)
      return(NULL)
    row <- MonthlyDataStateFilter[1,]
    state <- row$State
    lat <- row$Lat
    long <- row$Long
    
    gotoState(state, lat, long)
    
  })
  
  MonthlyDataStateFilter <- reactive({
    StateBy <- input$monthlyState
    leafletProxy("map") %>% clearPopups()
    
    newfilter <- cdcMonthlyStateData %>%
      filter(
        input$monthlyState == "" | State == input$monthlyState  ,
        input$year == "" | Year == input$year
      )
    
    if (nrow(newfilter) != 0)
    {
      row <- newfilter[1,]
      state <- row$State
      lat <- row$Lat
      long <- row$Long
      gotoState(state, lat, long)
    }
    
    
    newfilter
  })
  
  yearFilterAnnual <- reactive({
    YearBy <- input$year
    cdc.filtered.data <- cdcAnnualData %>% filter(Year == YearBy)
  })
  
  output$scatterDeaths <- renderPlot({
    shiny::validate(need(
      nrow(MonthlyDataStateFilter()) != 0,
      'No data Available for this Year/State'
    ))
    plotdata <-
      MonthlyDataStateFilter() %>% group_by(Month.Code) %>% select(Month.Code, Deaths) %>%
      summarise(Deaths = sum(Deaths))
    
    ggplot(plotdata, aes(x = Month.Code, y = Deaths, group = 1)) +
      geom_line(stat = "identity") +
      stat_smooth(method = "loess") +
      ggtitle(
        paste(
          "Deaths attributed to opiods in the state of" ,
          input$monthlyState,
          sep = " "
        ),
        input$year
      )
  })
  
  
  # This observer is responsible for maintaining the circles
  observe({
    YearBy <- input$year
    
    agg_data <- yearFilter() %>% group_by(State) %>%
      summarise(Deaths = sum(Deaths))  %>%
      left_join(state.geom, by = c('State' = 'name')) %>%
      select(State, Deaths, Lat = latitude, Long = longitude)
    
    #str(stategeoms)
    #print("next")
      stategeomsJoined<- sp::merge(stategeoms,yearFilterAnnual(),by.x='name',by.y='State')
   # sfstategeoms<-st_as_sf(stategeoms)
    #stategeomsJoined <-  yearFilterAnnual() %>%  left_join(sfstategeoms,by=c('State'='name')) %>% st_cast(to = "POLYGON")
    
    #stgeomsJOined <- sf_as_st(stategeomsJoined)
    #str(stategeomsJoined)
    bins <- c(-Inf,-10.0,-3.0,0, 1.0,  3.0,5.0,10.0,Inf)
    print(head(stategeomsJoined$PercentChange))
    pal <- colorNumeric("YlOrRd", domain = stategeomsJoined$PercentChange)
    #print(pal)
    radius <-
      agg_data[["Deaths"]] / sum(agg_data[["Deaths"]]) * 2300000
    leafletProxy("map", data = agg_data) %>% clearControls() %>%
      clearShapes() %>% addPolygons(data=stategeomsJoined, 
                                    weight = 2,
                                    fill=~pal(PercentChange),
                                    opacity = .5,
                                    color = "white",
                                    dashArray = "3",
                                    fillOpacity = .6) %>%
      addLegend(pal = pal, values = ~stategeomsJoined$PercentChange, opacity = 0.7, title = NULL,
                                                                     position = "bottomleft") %>%
      clearGroup("Circles") %>%
      addCircles(
        ~ Long,
        ~ Lat,
        group = "Circles",
        radius = radius,
        layerId =  ~ State,
        stroke = FALSE,
        fillOpacity = 0.4
      )  
    
  })
  
  PrettyNumbers <- function(val)
  {
    format(
      val,
      big.mark = ",",
      small.mark = ".",
      small.interval = 3
    )
  }
  # Show a popup at the given location
  showStatePopup <- function(state, lat, lng) {
    #str(yearFilterAnnual())
    #print(state)
    selectedState <-
      yearFilterAnnual()[yearFilterAnnual()$State == input$monthlyState,]
    if (nrow(selectedState) != 1 ) 
      return (NULL)
    content <- as.character(tagList(
      tags$h2(state),
      tags$h3("Year:", as.integer(selectedState$Year)),
      tags$h4("Deaths:", PrettyNumbers(as.integer(
        selectedState$Deaths
      ))) ,
      tags$h4("Population:", PrettyNumbers(selectedState$Population)) ,
      tags$h4("Crude Rate:", selectedState$Crude.Rate),
      tags$h4("Adjusted Crude Rate:", selectedState$Age.Adjusted.Rate)
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
  
  
  ## State Data Explorer  #################
  
  explorestatedatafilter <- reactive({
    cdcAnnualData %>%
      filter(
        is.null(input$usstates) | State %in% input$usstates)

  })
    
    output$scatterExplorerStateDeaths <- renderPlot({
      shiny::validate(need(
        nrow(explorestatedatafilter()) != 0,
        'No data Available for this Year/State'
      ))

      plotdata <-
        explorestatedatafilter() #%>% group_by(Year)%>% select(State,Year,Deaths)
      ggplot(plotdata) +
        #    geom_bar(stat="identity") +
        geom_line(mapping = aes(
          x = Year,
          y = (Deaths),
          group = State,
          color = State
        )) +
        geom_smooth(mapping = aes(
          x = Year,
          y = (Deaths),
          group = State,
          color = State
        )) +
        #  stat_smooth(method="loess",show.legend=TRUE) +
        ggtitle("Deaths attributed to opiods")
    })
    
    output$cdcStatetable <- DT::renderDataTable({
      if (NROW(explorestatedatafilter()) == 0)
        return(NULL)
      
      df <-   explorestatedatafilter() %>% arrange(desc(Year))%>%
        mutate(
          Action = paste(
            '<a class="go-map" href="" data-lat="',
            Lat,
            '" data-long="',
            Long,
            '" data-state="',
            State,
            '" data-year="',
            Year,
            '"><i class="fa fa-crosshairs"></i></a>',
            sep = ""
          )
        )
      action <- DT::dataTableAjax(session, df)
      
      DT::datatable(df,
                    options = list(
                      lengthMenu  = c(18, 36, 72, 144),
                      ajax = list(url = action)
                    ),
                    escape = FALSE) %>% formatRound(
                      c("Deaths", "Population"),
                      interval = 3,
                      mark = ",",
                      digits = 0
                    )
    })
    
    ## Data Explorer ###########################################
    observe({
      if (is.null(input$goto))
        return()
      updateSelectInput(session, "year", selected = input$goto$year)
      if (input$goto$state != "US Aggregated")
      {
        updateSelectInput(session, "monthlyState", selected = input$goto$state)
        state <- input$goto$state
        lat <- input$goto$lat
        lng <- input$goto$lng
        gotoState(state, lat, lng)
      }
      
      
    })
    
    gotoState <- function (state, lat, lng) {
      isolate({
        map <- leafletProxy("map")
        map %>% clearPopups()
        dist <- 0.8
        if (state != "US Aggregated")
        {
          showStatePopup(state, lat, lng)
          map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
        }
      })
      
    }
    
    exploredatafilter <- reactive({
      cdcAnnualData %>%
        filter(input$states == "" | State == input$states)
      
    })
    output$scatterExplorerDeaths <- renderPlot({
      if (nrow(exploredatafilter()) == 0)
        return(NULL)
      
      plotdata <-
        exploredatafilter() %>% group_by(Year) %>% select(Year, Deaths) %>% summarise(Deaths = sum(Deaths))
      ggplot(plotdata, aes(x = Year, y = Deaths, group = 1)) +
        geom_line(stat = "identity") +
        stat_smooth(method = "loess", show.legend = TRUE) +
        ggtitle("Deaths attributed to opiods")
    })
    
    
    
    output$cdctable <- DT::renderDataTable({

      df <-   exploredatafilter() %>% arrange(desc(Year)) %>% select (State,Year,Deaths,Population,Crude.Rate,Age.Adjusted.Rate,PercentChange)
      if (input$states != "")
      {
      #  df <-
      #    df %>%  mutate (PercentChange =  format((Deaths - lead(Deaths)) * 100 / lead(Deaths),
      #                                            big_mark = "," ,  digits = 2))
      }
      
      # df <- df %>%
      #   mutate(
      #     Action = paste(
      #       '<a class="go-map" href="" data-lat="',
      #       Lat,
      #       '" data-long="',
      #       Long,
      #       '" data-state="',
      #       State,
      #       '" data-year="',
      #       Year,
      #       '"><i class="fa fa-crosshairs"></i></a>',
      #       sep = ""
      #     )
      #   )
      action <- DT::dataTableAjax(session, df)
      
      DT::datatable(df,
                    options = list(
                      lengthMenu  = c(18, 36, 72, 144),
                      ajax = list(url = action)
                    ),
                    escape = FALSE) %>% formatRound(
                      c("Deaths", "Population"),
                      interval = 3,
                      mark = ",",
                      digits = 0
                    )
    })
}
