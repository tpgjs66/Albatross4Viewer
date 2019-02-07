
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(DT)
library(sp)
library(sf)
library(rgdal)
library(rgeos)
library(dplyr)
library(stplanr)
library(RcppArmadillo)
library(mapview)
library(rmapshaper)
library(doParallel)

ui <- (dashboardPage(
  dashboardHeader(title = "Albatross4 Viewer"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Data Preparation", 
        tabName = "data", 
        icon = icon("database")
        # menuSubItem("Watersheds", tabName = "m_water", icon = icon("map")),
        # menuSubItem("Population", tabName = "m_pop", icon = icon("map"))
      ),
      menuItem(
        "View Household", 
        tabName = "household", 
        icon = icon("users",lib = "font-awesome"),
        menuSubItem("Map", tabName = "map-household", icon = icon("globe")),
        # menuSubItem("Graph", tabName = "graph-household", icon = icon("signal")),
        menuSubItem("List", tabName = "list-household", icon = icon("table",lib="font-awesome"))
      ),
      menuItem(
        "View Schedule", 
        tabName = "schedule", 
        icon = icon("list-ol", lib = "font-awesome"),
        menuSubItem("Map", tabName = "map-schedule", icon = icon("globe")),
        # menuSubItem("Graph", tabName = "graph-schedule", icon = icon("signal")),
        menuSubItem("List", tabName = "list-schedule", icon = icon("table",lib="font-awesome"))
      ),
      # menuItem(
      #   "Summary Statistics", 
      #   tabName = "statistics", 
      #   icon = icon("stats",lib = "glyphicon"),
      #   menuSubItem("Map", tabName = "map-summary", icon = icon("globe")),
      #   menuSubItem("Graph", tabName = "graph-summary", icon = icon("chart-area"))
      # ),
      # menuItem(
      #   "Animate map", 
      #   tabName = "animate", 
      #   icon = icon("facetime-video",lib = "glyphicon")
      # ),
      menuItem(
        "About", 
        tabName = "about", 
        icon = icon("envelope",lib = "glyphicon")
      )
    )
  ),
  dashboardBody(
    tags$head(tags$style(type="text/css", "
                         #loadmessage {
                         border: 16px solid #f3f3f3; /* Light grey */
                         border-radius: 50%;
                         border-top: 16px solid #3c8dbc; /* Blue */
                         width: 100px;
                         height: 100px;
                         -webkit-animation: spin 2s linear infinite;

                         margin: auto;
                         margin-left: auto;
                         position: relative;
                         z-index: 100;
                         top: 350px;
                         left: 600px;
                         }
                         @keyframes spin {
     `                   0% { transform: rotate(0deg); }
                         100% { transform: rotate(360deg); }
                         }
                         ")),
    absolutePanel(
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div(id="loadmessage"))
    ),
    tabItems(
      tabItem(tabName = "data",
              tabsetPanel(type = "tabs",
                          tabPanel("Overview",
                                   img(src="Albatross.jpg", align = "left", width = "50%", height = "50%")
                          ),
                          tabPanel("Household",
                                   sidebarLayout(
                                     sidebarPanel( 
                                       
                                       # Input: Select a file ----
                                       fileInput("household", "Choose household data (less than 500MB)",
                                                 multiple = FALSE,
                                                 accept = c("text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv")),
                                       
                                       # Input: Select separator ----
                                       radioButtons("householdsep", "Separator",
                                                    choices = c(Comma = ",",
                                                                Semicolon = ";",
                                                                Tab = "\t"),
                                                    selected = ","),
                                       
                                       # Input: Select quotes ----
                                       radioButtons("householdquote", "Quote",
                                                    choices = c(None = "",
                                                                "Double Quote" = '"',
                                                                "Single Quote" = "'"),
                                                    selected = '"'),
                                       
                                       # Input: Checkbox if file has header ----
                                       checkboxInput("householdheader", "Header", TRUE),
                                       
                                       # Input: Select number of rows to display ----
                                       radioButtons("householddisp", "Display",
                                                    choices = c(Head = "head",
                                                                All = "all"),
                                                    selected = "head")
                                       
                                     ),
                                     mainPanel(
                                       tableOutput("previewHousehold"),
                                       h3(textOutput("previewHouseholdTotal")),
                                       width = 12
                                     )
                                   )
                          ),
                          tabPanel("Household coordinates",
                                   sidebarLayout(
                                     sidebarPanel( 
                                       
                                       # Input: Select a file ----
                                       fileInput("hhcoords", "Choose household coordinates data (less than 500MB)",
                                                 multiple = FALSE,
                                                 accept = c("text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv")),
                                       
                                       # Input: Select separator ----
                                       radioButtons("hhcoordssep", "Separator",
                                                    choices = c(Comma = ",",
                                                                Semicolon = ";",
                                                                Tab = "\t"),
                                                    selected = ","),
                                       
                                       # Input: Select quotes ----
                                       radioButtons("hhcoordsquote", "Quote",
                                                    choices = c(None = "",
                                                                "Double Quote" = '"',
                                                                "Single Quote" = "'"),
                                                    selected = '"'),
                                       
                                       # Input: Checkbox if file has header ----
                                       checkboxInput("hhcoordsheader", "Header", TRUE),
                                       
                                       # Input: Select number of rows to display ----
                                       radioButtons("hhcoordsdisp", "Display",
                                                    choices = c(Head = "head",
                                                                All = "all"),
                                                    selected = "head")
                                       
                                     ),
                                     mainPanel(
                                       
                                     )
                                   )
                          ),
                          tabPanel("Schedule",
                                   sidebarLayout(
                                     sidebarPanel( 
                                       
                                       # Input: Select a file ----
                                       fileInput("schedule", "Choose schedule data (less than 500MB)",
                                                 multiple = FALSE,
                                                 accept = c("text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv")),
                                       
                                       # Input: Select separator ----
                                       radioButtons("schedulesep", "Separator",
                                                    choices = c(Comma = ",",
                                                                Semicolon = ";",
                                                                Tab = "\t"),
                                                    selected = ","),
                                       
                                       # Input: Select quotes ----
                                       radioButtons("schedulequote", "Quote",
                                                    choices = c(None = "",
                                                                "Double Quote" = '"',
                                                                "Single Quote" = "'"),
                                                    selected = '"'),
                                       
                                       # Input: Checkbox if file has header ----
                                       checkboxInput("scheduleheader", "Header", TRUE),
                                       
                                       # Input: Select number of rows to display ----
                                       radioButtons("scheduledisp", "Display",
                                                    choices = c(Head = "head",
                                                                All = "all"),
                                                    selected = "head")
                                       
                                     ),
                                     mainPanel(
                                       tableOutput("previewSchedule"),
                                       h3(textOutput("previewScheduleTotal")),
                                       width = 12
                                     )
                                   )
                          ),
                          tabPanel("Schedule coordinates",
                                   sidebarLayout(
                                     sidebarPanel( 
                                       
                                       # Input: Select a file ----
                                       fileInput("scheduleCoords", "Choose schedule coordinates data (less than 500MB)",
                                                 multiple = FALSE,
                                                 accept = c("text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv")),
                                       
                                       # Input: Select separator ----
                                       radioButtons("scheduleCoordssep", "Separator",
                                                    choices = c(Comma = ",",
                                                                Semicolon = ";",
                                                                Tab = "\t"),
                                                    selected = ","),
                                       
                                       # Input: Select quotes ----
                                       radioButtons("scheduleCoordsquote", "Quote",
                                                    choices = c(None = "",
                                                                "Double Quote" = '"',
                                                                "Single Quote" = "'"),
                                                    selected = '"'),
                                       
                                       # Input: Checkbox if file has header ----
                                       checkboxInput("scheduleCoordsheader", "Header", TRUE),
                                       
                                       # Input: Select number of rows to display ----
                                       radioButtons("scheduleCoordsdisp", "Display",
                                                    choices = c(Head = "head",
                                                                All = "all"),
                                                    selected = "head")
                                       
                                     ),
                                     mainPanel(
                                       
                                     )
                                   )
                          ),
                          tabPanel("Shape files - PC4",
                                   sidebarLayout(
                                     sidebarPanel(
                                       
                                       # Input: Select a file ----
                                       fileInput("shpFilePc4", "Choose shape files (.prj, .shp, .shx, .dbf)",
                                                 multiple = TRUE,
                                                 accept = c(".prj",
                                                            ".shp",
                                                            ".shx",
                                                            ".dbf"),
                                                 placeholder = "4 files"),
                                       helpText("Note: Multiple files should be uploaded at once.")
                                     ),
                                     mainPanel(
                                       h3("")
                                     )
                                   )
                          )
              )
      ),
      tabItem(tabName = "list-household",
              fluidPage(div(DT::dataTableOutput("householdTotal"),
                            style = "font-size: 100%; width: 100%"))
      ),
      tabItem(tabName = "list-schedule",
              tabsetPanel(type = "tabs",
                          tabPanel("Schedule",
                                   fluidRow(
                                     column(width = 12,
                                            DT::dataTableOutput("scheduleTotal")
                                     )
                                   )
                          ),
                          tabPanel("O-D flow",
                                   fluidRow(
                                     column(width = 7,
                                            box(width = 12,
                                                DT::dataTableOutput("scheduleod")
                                            ),
                                            downloadButton("downloadDTsched", "Download")
                                     ),
                                     column(width = 5,
                                            verticalLayout(
                                              box(width = 12,
                                                  verticalLayout(
                                                    fluidRow(
                                                      column(width = 12,
                                                             box(width = 6, title = "Activity type",status = "primary",
                                                                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                                                                 # Copy the line below to make a set of radio buttons
                                                                 checkboxGroupInput("listschedact",label = NULL,
                                                                                    choices = list("Home" = "Home",
                                                                                                   "Work" = "Work",
                                                                                                   "Business" = "Business",
                                                                                                   "Bring/Get" = "BringGet",
                                                                                                   "Groceries" = "Groceries",
                                                                                                   "Non-Daily Shopping" = "NonGroc",
                                                                                                   "Services" = "Services",
                                                                                                   "Social" = "Social",
                                                                                                   "Leisure" = "Leisure",
                                                                                                   "Touring" = "Touring",
                                                                                                   "Other" = "Other"
                                                                                    ),
                                                                                    selected = c("Home","Work","Business","BringGet",
                                                                                                 "Groceries","NonGroc","Services",
                                                                                                 "Social","Leisure","Touring","Other"))),
                                                             box(width = 6, title = "Transport mode",status = "primary",
                                                                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                                                                 # Copy the line below to make a set of radio buttons
                                                                 checkboxGroupInput("listschedmode",label = NULL,
                                                                                    choices = list("Staying Home" = "Missing",
                                                                                                   "Car" = "Car",
                                                                                                   "Car as Passenger" = "Car as Passenger",
                                                                                                   "Public Transport" = "Public Transport",
                                                                                                   "Walking or Biking" = "Walking or Biking"
                                                                                    ),
                                                                                    selected = c("Missing","Car","Car as Passenger",
                                                                                                 "Public Transport","Walking or Biking")))
                                                             
                                                             
                                                      )
                                                    ),
                                                    box(width = 12, title = "Time of day",status = "primary",
                                                        solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                                                        sliderInput("listschedtime", label = NULL, min =180 , max = 1620, value = c(180,1620))
                                                    ),
                                                    actionButton("submitscheduleod", "Submit")
                                                    # submitButton("Submit")
                                                  )
                                              )
                                            )
                                     )
                                   )
                          )
              )
      ),
      tabItem(tabName = "map-household",
              fluidRow(
                column(width = 8,
                       box(width = 12,leafletOutput("maphh",width = "100%", height = 750))
                ),
                # box(width = 12, title = "Number of households to query",status = "primary",
                #     solidHeader = FALSE,collapsible = TRUE,collapsed = TRUE,
                #     fluidRow(width = 12,
                #              column(width = 7,
                #                     checkboxInput("maphhshow", label = "Show only specified", value = FALSE)
                #              ),
                #              column(width = 5,
                #                     numericInput("maphhnum", label = NULL, value = 10)
                #              )
                #     )
                # ),
                column(width = 4,
                       verticalLayout(
                         box(width = 12, title = "Household information",status = "primary",
                             solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                             verticalLayout(
                               fluidRow(
                                 column(width = 12,
                                        verbatimTextOutput("clickedhhId")
                                 ),
                                 column(width = 12,
                                        dataTableOutput("clickedhhTable")
                                 )
                               )
                             )
                         )
                       )
                ),
                actionButton("submitmaphh", "Submit")
                # submitButton("Submit")
              )
      ),
      tabItem(tabName = "map-schedule",
              tabsetPanel(type = "tabs",
                          tabPanel("Overview"
                          ),
                          tabPanel("Activity location",
                                   fluidRow(
                                     column(width = 8,
                                            box(width = 12,leafletOutput("mapactloc",width = "100%", height = 750))
                                     ),
                                     column(width = 4,
                                            verticalLayout(
                                              box(width = 12,title = "Data filter",status = "primary",
                                                  solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                                                  verticalLayout(
                                                    fluidRow(
                                                      column(width = 12,
                                                             box(width = 6, title = "Activity type",status = "primary",
                                                                 solidHeader = FALSE,collapsible = TRUE,collapsed = FALSE,
                                                                 # Copy the line below to make a set of radio buttons
                                                                 checkboxGroupInput("mapactlocact",label = NULL,
                                                                                    choices = list("Home" = "Home",
                                                                                                   "Work" = "Work",
                                                                                                   "Business" = "Business",
                                                                                                   "Bring/Get" = "BringGet",
                                                                                                   "Groceries" = "Groceries",
                                                                                                   "Non-Daily Shopping" = "NonGroc",
                                                                                                   "Services" = "Services",
                                                                                                   "Social" = "Social",
                                                                                                   "Leisure" = "Leisure",
                                                                                                   "Touring" = "Touring",
                                                                                                   "Other" = "Other"
                                                                                    ),
                                                                                    selected = NULL)),
                                                             box(width = 6, title = "Transport mode",status = "primary",
                                                                 solidHeader = FALSE,collapsible = TRUE,collapsed = FALSE,
                                                                 # Copy the line below to make a set of radio buttons
                                                                 checkboxGroupInput("mapactlocmode",label = NULL,
                                                                                    choices = list("Staying Home" = "Missing",
                                                                                                   "Car" = "Car",
                                                                                                   "Car as Passenger" = "Car as Passenger",
                                                                                                   "Public Transport" = "Public Transport",
                                                                                                   "Walking or Biking" = "Walking or Biking"
                                                                                    ),
                                                                                    selected = NULL))
                                                      )
                                                    ),
                                                    box(width = 12, title = "Charging",status = "primary",
                                                        solidHeader = FALSE,collapsible = TRUE,collapsed = TRUE,
                                                        checkboxGroupInput("mapactloccharging", label = NULL,
                                                                           choices = list("No charging" = "NoCharging",
                                                                                          "Private charging" = "PrivateCharging",
                                                                                          "Public charging" = "PublicCharging",
                                                                                          "Semi-public charging" = "SemiPublicCharging",
                                                                                          "Fast charging" = "FastCharging"
                                                                                          ),
                                                                           selected = c("NoCharging","PrivateCharging", "PublicCharging", "SemiPublicCharging", "FastCharging"))
                                                    ),
                                                    box(width = 12, title = "Time of day",status = "primary",
                                                        solidHeader = FALSE,collapsible = TRUE,collapsed = TRUE,
                                                        sliderInput("mapactloctime", label = NULL, min =180 , max = 1620, value = c(180,1620))
                                                    ),
                                                    actionButton("submitmapactloc", "Submit")
                                                    # submitButton("Submit")
                                                  )
                                              ),
                                              box(width = 12,title = "Activity information",status = "primary",
                                                  solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                                                  verticalLayout(
                                                    fluidRow(
                                                      column(width = 12,
                                                             verbatimTextOutput("clickedactlocId")
                                                      ),
                                                      column(width = 12,
                                                             dataTableOutput("clickedactlocTable")
                                                      )
                                                    )
                                                  )
                                              )
                                            )
                                     )
                                   )
                          ),
                          tabPanel("O-D flow(PC4)",
                                   fluidRow(
                                     column(width = 8,
                                            box(width = 12,leafletOutput("mapodflow",width = "100%", height = 750))
                                     ),
                                     column(width = 4,
                                            box(width = 12,title = "Search by O-D pair ID",status = "primary",
                                                solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                                                # Input: Specify the trip id to view ----
                                                numericInput("odid", label = NULL, value = 1),
                                                actionButton("update", "Search")
                                            ),
                                            hr(),
                                            verticalLayout(
                                              box(width = 12,title = "Data filter",status = "primary",
                                                  solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                                                  verticalLayout(
                                                    fluidRow(
                                                      column(width = 12,
                                                             box(width = 6, title = "Activity type",status = "primary",
                                                                 solidHeader = FALSE,collapsible = TRUE,collapsed = FALSE,
                                                                 # Copy the line below to make a set of radio buttons
                                                                 checkboxGroupInput("mapodflowact",label = NULL,
                                                                                    choices = list("Home" = "Home",
                                                                                                   "Work" = "Work",
                                                                                                   "Business" = "Business",
                                                                                                   "Bring/Get" = "BringGet",
                                                                                                   "Groceries" = "Groceries",
                                                                                                   "Non-Daily Shopping" = "NonGroc",
                                                                                                   "Services" = "Services",
                                                                                                   "Social" = "Social",
                                                                                                   "Leisure" = "Leisure",
                                                                                                   "Touring" = "Touring",
                                                                                                   "Other" = "Other"
                                                                                    ),
                                                                                    selected = NULL)),
                                                             box(width = 6, title = "Transport mode",status = "primary",
                                                                 solidHeader = FALSE,collapsible = TRUE,collapsed = FALSE,
                                                                 # Copy the line below to make a set of radio buttons
                                                                 checkboxGroupInput("mapodflowmode",label = NULL,
                                                                                    choices = list("Staying Home" = "Missing",
                                                                                                   "Car" = "Car",
                                                                                                   "Car as Passenger" = "Car as Passenger",
                                                                                                   "Public Transport" = "Public Transport",
                                                                                                   "Walking or Biking" = "Walking or Biking"
                                                                                    ),
                                                                                    selected = NULL))
                                                      )
                                                    ),
                                                    box(width = 12, title = "Charging",status = "primary",
                                                        solidHeader = FALSE,collapsible = TRUE,collapsed = TRUE,
                                                        checkboxGroupInput("mapodflowcharging", label = NULL,
                                                                           choices = list("No charging" = "NoCharging",
                                                                                          "Private charging" = "PrivateCharging",
                                                                                          "Public charging" = "PublicCharging",
                                                                                          "Semi-public charging" = "SemiPublicCharging",
                                                                                          "Fast charging" = "FastCharging"
                                                                           ),
                                                                           selected = c("NoCharging","PrivateCharging", "PublicCharging", "SemiPublicCharging", "FastCharging"))
                                                    ),
                                                    box(width = 12, title = "Time of day",status = "primary",
                                                        solidHeader = FALSE,collapsible = TRUE,collapsed = TRUE,
                                                        sliderInput("mapodflowtime", label = NULL, min =180 , max = 1620, value = c(180,1620))
                                                    ),
                                                    box(width = 12, title = "Number of O-D pairs to query",status = "primary",
                                                        solidHeader = FALSE,collapsible = TRUE,collapsed = TRUE,
                                                        fluidRow(width = 12,
                                                                 helpText("Note: O-D pairs are sorted largest to smallest by n.trips"),
                                                                 column(width = 7,
                                                                        checkboxInput("mapodflowshow", label = "Show only specified", value = FALSE)
                                                                 ),
                                                                 column(width = 5,
                                                                        numericInput("mapodflownum", label = NULL, value = 10)
                                                                 )
                                                        )
                                                    ),
                                                    actionButton("submitmapodflow", "Submit")
                                                    # submitButton("Submit")
                                                  )
                                              )
                                            ),
                                            box(width = 12, title = "Option",status = "primary",
                                                solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                                                downloadButton("dlmapodflow", label = "Export map")
                                            )
                                     )
                                   )
                          ),
                          tabPanel("Route-Individual(PC4)",
                                   fluidRow(
                                     column(width = 8,
                                            box(width = 12,leafletOutput("maprouteind",width = "100%", height = 750))
                                     ),
                                     column(width = 4,
                                            verticalLayout(
                                              box(width = 12,title = "Data filter",status = "primary",
                                                  solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                                                  verticalLayout(
                                                    fluidRow(
                                                      column(width = 12,
                                                             box(width = 6, title = "Activity type",status = "primary",
                                                                 solidHeader = FALSE,collapsible = TRUE,collapsed = FALSE,
                                                                 # Copy the line below to make a set of radio buttons
                                                                 checkboxGroupInput("maprouteindact",label = NULL,
                                                                                    choices = list("Home" = "Home",
                                                                                                   "Work" = "Work",
                                                                                                   "Business" = "Business",
                                                                                                   "Bring/Get" = "BringGet",
                                                                                                   "Groceries" = "Groceries",
                                                                                                   "Non-Daily Shopping" = "NonGroc",
                                                                                                   "Services" = "Services",
                                                                                                   "Social" = "Social",
                                                                                                   "Leisure" = "Leisure",
                                                                                                   "Touring" = "Touring",
                                                                                                   "Other" = "Other"
                                                                                    ),
                                                                                    selected = NULL)),
                                                             box(width = 6, title = "Transport mode",status = "primary",
                                                                 solidHeader = FALSE,collapsible = TRUE,collapsed = FALSE,
                                                                 # Copy the line below to make a set of radio buttons
                                                                 radioButtons("maprouteindmode",label = NULL,
                                                                                    choices = list("All" = "All",
                                                                                                   "Car" = "Car",
                                                                                                   "Car as Passenger" = "Car as Passenger",
                                                                                                   "Public Transport" = "Public Transport",
                                                                                                   "Walking or Biking" = "Walking or Biking"
                                                                                    ),
                                                                                    selected = "All"))
                                                      )
                                                    ),
                                                    box(width = 12, title = "Charging",status = "primary",
                                                        solidHeader = FALSE,collapsible = TRUE,collapsed = TRUE,
                                                        checkboxGroupInput("maprouteindcharging", label = NULL,
                                                                           choices = list("No charging" = "NoCharging",
                                                                                          "Private charging" = "PrivateCharging",
                                                                                          "Public charging" = "PublicCharging",
                                                                                          "Semi-public charging" = "SemiPublicCharging",
                                                                                          "Fast charging" = "FastCharging"
                                                                           ),
                                                                           selected = c("NoCharging","PrivateCharging", "PublicCharging", "SemiPublicCharging", "FastCharging"))
                                                    ),
                                                    box(width = 12, title = "Time of day",status = "primary",
                                                        solidHeader = FALSE,collapsible = TRUE,collapsed = TRUE,
                                                        sliderInput("maprouteindtime", label = NULL, min =180 , max = 1620, value = 800)
                                                    ),
                                                    box(width = 12, title = "Number of O-D pairs to route",status = "primary",
                                                        solidHeader = FALSE,collapsible = TRUE,collapsed = TRUE,
                                                        fluidRow(width = 12,
                                                                 helpText("Note: O-D pairs are sorted largest to smallest by n.trips"),
                                                                 column(width = 7,
                                                                        checkboxInput("maprouteindshow", label = "Show only specified", value = TRUE)
                                                                 ),
                                                                 column(width = 5,
                                                                        numericInput("maprouteindnum", label = NULL, value = 5)
                                                                 )
                                                        )
                                                    ),
                                                    actionButton("submitmaprouteind", "Submit")
                                                    # submitButton("Submit")
                                                  )
                                              )
                                            ),
                                            box(width = 12, title = "Route information",status = "primary",
                                                solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                                                verticalLayout(
                                                  fluidRow(
                                                    column(width = 12,
                                                           verbatimTextOutput("clickedrouteindId")
                                                    ),
                                                    column(width = 12,
                                                           dataTableOutput("clickedrouteindTable")
                                                    )
                                                  )
                                                )
                                            ),
                                            box(width = 12, title = "Option",status = "primary",
                                                solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                                                downloadButton("dlmaprouteind", label = "Export map")
                                            )
                                     )
                                   )
                          ),
                          tabPanel("Route-Individual(PC6)",
                                   fluidRow(
                                     column(width = 8,
                                            box(width = 12,leafletOutput("maprouteindpc6",width = "100%", height = 750))
                                     ),
                                     column(width = 4,
                                            verticalLayout(
                                              box(width = 12,title = "Data filter",status = "primary",
                                                  solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                                                  verticalLayout(
                                                    fluidRow(
                                                      column(width = 12,
                                                             box(width = 6, title = "Activity type",status = "primary",
                                                                 solidHeader = FALSE,collapsible = TRUE,collapsed = FALSE,
                                                                 # Copy the line below to make a set of radio buttons
                                                                 checkboxGroupInput("maprouteindpc6act",label = NULL,
                                                                                    choices = list("Home" = "Home",
                                                                                                   "Work" = "Work",
                                                                                                   "Business" = "Business",
                                                                                                   "Bring/Get" = "BringGet",
                                                                                                   "Groceries" = "Groceries",
                                                                                                   "Non-Daily Shopping" = "NonGroc",
                                                                                                   "Services" = "Services",
                                                                                                   "Social" = "Social",
                                                                                                   "Leisure" = "Leisure",
                                                                                                   "Touring" = "Touring",
                                                                                                   "Other" = "Other"
                                                                                    ),
                                                                                    selected = NULL)),
                                                             box(width = 6, title = "Transport mode",status = "primary",
                                                                 solidHeader = FALSE,collapsible = TRUE,collapsed = FALSE,
                                                                 # Copy the line below to make a set of radio buttons
                                                                 radioButtons("maprouteindpc6mode",label = NULL,
                                                                              choices = list("All" = "All",
                                                                                             "Car" = "Car",
                                                                                             "Car as Passenger" = "Car as Passenger",
                                                                                             "Public Transport" = "Public Transport",
                                                                                             "Walking or Biking" = "Walking or Biking"
                                                                              ),
                                                                              selected = "All"))
                                                      )
                                                    ),
                                                    box(width = 12, title = "Charging",status = "primary",
                                                        solidHeader = FALSE,collapsible = TRUE,collapsed = TRUE,
                                                        checkboxGroupInput("maprouteindpc6charging", label = NULL,
                                                                           choices = list("No charging" = "NoCharging",
                                                                                          "Private charging" = "PrivateCharging",
                                                                                          "Public charging" = "PublicCharging",
                                                                                          "Semi-public charging" = "SemiPublicCharging",
                                                                                          "Fast charging" = "FastCharging"
                                                                           ),
                                                                           selected = c("NoCharging","PrivateCharging", "PublicCharging", "SemiPublicCharging", "FastCharging"))
                                                    ),
                                                    box(width = 12, title = "Time of day",status = "primary",
                                                        solidHeader = FALSE,collapsible = TRUE,collapsed = TRUE,
                                                        sliderInput("maprouteindtimepc6", label = NULL, min =180 , max = 1620, value = 800)
                                                    ),
                                                    box(width = 12, title = "Number of trips to route",status = "primary",
                                                        solidHeader = FALSE,collapsible = TRUE,collapsed = TRUE,
                                                        fluidRow(width = 12,
                                                                 column(width = 7,
                                                                        checkboxInput("maprouteindpc6show", label = "Show only specified", value = TRUE)
                                                                 ),
                                                                 column(width = 5,
                                                                        numericInput("maprouteindpc6num", label = NULL, value = 5)
                                                                 )
                                                        )
                                                    ),
                                                    actionButton("submitmaprouteindpc6", "Submit")
                                                    # submitButton("Submit")
                                                  )
                                              )
                                            ),
                                            box(width = 12, title = "Route information",status = "primary",
                                                solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                                                verticalLayout(
                                                  fluidRow(
                                                    column(width = 12,
                                                           verbatimTextOutput("clickedrouteindIdpc6")
                                                    ),
                                                    column(width = 12,
                                                           dataTableOutput("clickedrouteindTablepc6")
                                                    )
                                                  )
                                                )
                                            ),
                                            box(width = 12, title = "Option",status = "primary",
                                                solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                                                downloadButton("dlmaprouteindpc6", label = "Export map")
                                            )
                                     )
                                   )
                          ),
                          tabPanel("Route-Aggregated(PC4)",
                                   fluidRow(
                                     column(width = 8,
                                            box(width = 12,leafletOutput("maprouteagg",width = "100%", height = 750))
                                     ),
                                     column(width = 4,
                                            verticalLayout(
                                              box(width = 12,title = "Data filter",status = "primary",
                                                  solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                                                  verticalLayout(
                                                    fluidRow(
                                                      column(width = 12,
                                                             box(width = 6, title = "Activity type",status = "primary",
                                                                 solidHeader = FALSE,collapsible = TRUE,collapsed = FALSE,
                                                                 # Copy the line below to make a set of radio buttons
                                                                 checkboxGroupInput("maprouteaggact",label = NULL,
                                                                                    choices = list("Home" = "Home",
                                                                                                   "Work" = "Work",
                                                                                                   "Business" = "Business",
                                                                                                   "Bring/Get" = "BringGet",
                                                                                                   "Groceries" = "Groceries",
                                                                                                   "Non-Daily Shopping" = "NonGroc",
                                                                                                   "Services" = "Services",
                                                                                                   "Social" = "Social",
                                                                                                   "Leisure" = "Leisure",
                                                                                                   "Touring" = "Touring",
                                                                                                   "Other" = "Other"
                                                                                    ),
                                                                                    selected = NULL)),
                                                             box(width = 6, title = "Transport mode",status = "primary",
                                                                 solidHeader = FALSE,collapsible = TRUE,collapsed = FALSE,
                                                                 # Copy the line below to make a set of radio buttons
                                                                 radioButtons("maprouteaggmode",label = NULL,
                                                                                    choices = list("All" = "All",
                                                                                                   "Car" = "Car",
                                                                                                   "Car as Passenger" = "Car as Passenger",
                                                                                                   "Public Transport" = "Public Transport",
                                                                                                   "Walking or Biking" = "Walking or Biking"
                                                                                    ),
                                                                                    selected = "All"))
                                                             
                                                             
                                                      )
                                                    ),
                                                    box(width = 12, title = "Charging",status = "primary",
                                                        solidHeader = FALSE,collapsible = TRUE,collapsed = TRUE,
                                                        checkboxGroupInput("maprouteaggcharging", label = NULL,
                                                                           choices = list("No charging" = "NoCharging",
                                                                                          "Private charging" = "PrivateCharging",
                                                                                          "Public charging" = "PublicCharging",
                                                                                          "Semi-public charging" = "SemiPublicCharging",
                                                                                          "Fast charging" = "FastCharging"
                                                                           ),
                                                                           selected = c("NoCharging","PrivateCharging", "PublicCharging", "SemiPublicCharging", "FastCharging"))
                                                    ),
                                                    box(width = 12, title = "Time of day",status = "primary",
                                                        solidHeader = FALSE,collapsible = TRUE,collapsed = TRUE,
                                                        sliderInput("maprouteaggtime", label = NULL, min =180 , max = 1620, value = 800)
                                                    ),
                                                    box(width = 12, title = "Number of O-D pairs to route",status = "primary",
                                                        solidHeader = FALSE,collapsible = TRUE,collapsed = TRUE,
                                                        fluidRow(width = 12,
                                                                 helpText("Note: O-D pairs are sorted largest to smallest by n.trips"),
                                                                 column(width = 7,
                                                                        checkboxInput("maprouteaggshow", label = "Show only specified", value = TRUE)
                                                                 ),
                                                                 column(width = 5,
                                                                        numericInput("maprouteaggnum", label = NULL, value = 5)
                                                                 )
                                                        )
                                                    ),
                                                    actionButton("submitmaprouteagg", "Submit")
                                                    # submitButton("Submit")
                                                  )
                                              )
                                            ),
                                            box(width = 12, title = "Route information",status = "primary",
                                                solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                                                verticalLayout(
                                                  fluidRow(
                                                    column(width = 12,
                                                           verbatimTextOutput("clickedrouteaggId")
                                                    ),
                                                    column(width = 12,
                                                           dataTableOutput("clickedrouteaggTable")
                                                    )
                                                  )
                                                )
                                            ),
                                            box(width = 12, title = "Option",status = "primary",
                                                solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                                                downloadButton("dlmaprouteagg", label = "Export map")
                                            )
                                     )
                                   )
                          )
              )
      ),
      tabItem(tabName = "map-summary",
              h2("Show map")
      ),
      tabItem(tabName = "graph-household",
              h2("Show graph")
      ),
      tabItem(tabName = "graph-schedule",
              h2("Show graph")
      ),
      tabItem(tabName = "graph-summary",
              h2("Show graph")
      ),
      tabItem(tabName = "about",
              h2("About")
      ),
      tabItem(tabName = "animate",
              tabsetPanel(type = "tabs",
                          tabPanel("Overview"
                          ),
                          tabPanel("Animate map",
                                   fluidRow(
                                     column(width = 8,
                                            box(width = 12,leafletOutput("mapanim",width = "100%", height = 750))
                                     ),
                                     column(width = 4,
                                            verticalLayout(
                                              box(width = 12,title = "Data filter",status = "primary",
                                                  solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                                                  verticalLayout(
                                                    fluidRow(
                                                      column(width = 12,
                                                             box(width = 6, title = "Activity type",status = "primary",
                                                                 solidHeader = FALSE,collapsible = TRUE,collapsed = FALSE,
                                                                 # Copy the line below to make a set of radio buttons
                                                                 checkboxGroupInput("mapanimact",label = NULL,
                                                                                    choices = list("Home" = "Home",
                                                                                                   "Work" = "Work",
                                                                                                   "Business" = "Business",
                                                                                                   "Bring/Get" = "BringGet",
                                                                                                   "Groceries" = "Groceries",
                                                                                                   "Non-Daily Shopping" = "NonGroc",
                                                                                                   "Services" = "Services",
                                                                                                   "Social" = "Social",
                                                                                                   "Leisure" = "Leisure",
                                                                                                   "Touring" = "Touring",
                                                                                                   "Other" = "Other"
                                                                                    ),
                                                                                    selected = c("Home","Work","Business","BringGet",
                                                                                                 "Groceries","NonGroc","Services",
                                                                                                 "Social","Leisure","Touring","Other"))),
                                                             box(width = 6, title = "Transport mode",status = "primary",
                                                                 solidHeader = FALSE,collapsible = TRUE,collapsed = FALSE,
                                                                 # Copy the line below to make a set of radio buttons
                                                                 checkboxGroupInput("mapanimmode",label = NULL,
                                                                                    choices = list("Staying Home" = "Missing",
                                                                                                   "Car" = "Car",
                                                                                                   "Car as Passenger" = "Car as Passenger",
                                                                                                   "Public Transport" = "Public Transport",
                                                                                                   "Walking or Biking" = "Walking or Biking"
                                                                                    ),
                                                                                    selected = c("Missing","Car","Car as Passenger",
                                                                                                 "Public Transport","Walking or Biking")))
                                                      )
                                                    )
                                                  )
                                              ),
                                              box(width = 12,title = "Time line",status = "primary",
                                                  solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                                                  sliderInput("mapanimtime", label = NULL, min = 180, max = 1620,
                                                              value = c(180,195),
                                                              step=15,
                                                              animate= animationOptions(interval = 200, loop = FALSE,
                                                                                        playButton = NULL, pauseButton = NULL))
                                              )
                                            )
                                     )
                                   )
                          )
              )
      )
    )
  )
))