#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram


navbarPage("CDC Opiod Deaths", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 400, height = "auto",
                                      
                                      h2("CDC"),
                                      
                                      selectInput("year","Year", Years,selected="2016"),
                                    #  selectInput("size", "Size", vars, selected = "adultpop"),
                                    #  conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                    #                   # Only prompt for threshold when coloring or sizing by superzip
                                    #                   numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                    #  ),
                                      
                                      plotOutput("scatterDeaths", height = 300)
                                   #   plotOutput("scatterCollegeIncome", height = 250)
                        ),
                        
                        tags$div(id="cite",
                                 'Data are from the Multiple Cause of Death Files, 1999-2016, as ',
 'compiled from data provided by the 57 vital statistics jurisdictions through the Vital Statistics Cooperative Program.',
'Accessed at http://wonder.cdc.gov/ucd-icd10.html on Feb 10, 2018 4:36:12 PM '
                        )
                    )
           ),
           
           tabPanel("Data explorer",
                    fluidRow(
                      column(3,
                             selectInput("states", "States", c("All states"="", States), multiple=TRUE)
                      )
                    ),
                    fluidRow(
                      column(3,
                             selectInput("exploreryears", "Years", c("All Years"="",Years), multiple=TRUE)
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("cdctable"),
                    hr(),
                    
                    plotOutput("scatterExplorerDeaths", height = 300)
           ),
           
           conditionalPanel("false", icon("crosshair"))
)

