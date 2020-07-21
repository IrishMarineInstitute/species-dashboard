# This is the ui of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
##### Packages #####
library(shiny)
library(leaflet)
library(dplyr)
library(shinythemes)
library(plotly)
#library(FSA)
library(htmltools)
library(shinycssloaders)
library(shinyWidgets)

library(rgdal)
library(tidyverse)

shinyUI(
    navbarPage(id="TopLevelMenu", title="Species Dashboard", theme= shinytheme("cerulean"), fluid=TRUE,
               ##### Intro #####                    
               tabPanel("Introduction",
                        
                        tags$head(includeScript("google-analytics.js")), 
                        tags$style(
                          ".irs-bar {",
                          "  border-color: transparent;",
                          "  background-color: transparent;",
                          "}",
                          ".irs-bar-edge {",
                          "  border-color: transparent;",
                          "  background-color: transparent;",
                          "}"
                        ),
                        fluidRow(
                            column(width = 7,
                                   tabsetPanel(id= "tabs",
                                               tabPanel("Fishing Grounds", value = "A", 
                                                        p(), htmlOutput("intro_tabset1"),
                                                        div(p(HTML(paste0('Funding for this project was provided by the EMFF ',br(),
                                                                          p(),
                                                                          img(src="Logos/Irelands_EU_ESIF_2014_2020_en.jpg", width = "300px", height = "100px")))))),
                                               tabPanel("Irish Ports", value = "B", 
                                                        p(), htmlOutput("intro_tabset2a"), 
                                                        p(), imageOutput("tabmap2", inline=TRUE),
                                                        p(), htmlOutput("intro_tabset2b")),
                                               tabPanel("Types of Gear", value = "C", 
                                                        p(), htmlOutput("intro_tabset3"),
                                                        p(),
                                                        hr(),
                                                        column(6,
                                                               selectInput("gearpic",label="Diagram of fishing gear type",
                                                                           choices =  list("Beam trawl"= 1, "Gillnet"= 2, 
                                                                                           "Midwater trawl"= 3,"Otter trawl"= 4,
                                                                                           "Seine Net"= 5), 
                                                                           selected = 1),
                                                               tags$h5("Illustration:"),
                                                               imageOutput("gear_pic"),
                                                               offset=2)
                                               ),
                                               tabPanel("Vessel Nationalities",value = "D",
                                                        p(), htmlOutput("intro_tabset4b")),
                                               tabPanel("Fish Ageing",value = "E",
                                                        p(), htmlOutput("intro_tabset5"),
                                                        p(),
                                                        fluidRow(column(6,
                                                                        imageOutput("tabpic5"),
                                                                        offset=2)),
                                                        fluidRow(column(12,htmlOutput("intro_tabset5b"),style = "margin-top:-8em"))),
                                               tabPanel("Data Collection",value = "F",
                                                        p(), htmlOutput("intro_tabset6"),
                                                        htmlOutput("intro_tabset6b"),
                                                        tags$a(href = "https://www.dcmap-ireland.ie/", 
                                                               "Learn More", 
                                                               target="_blank"),
                                                       imageOutput("tabpic6a"))
                                               
                                   )#close tabsetPanel     
                            ), #close column
                            column(width = 5,
                                   conditionalPanel(condition = "input.tabs == 'A'",
                                                    imageOutput("fgmap1",height ="100%")),
                                   conditionalPanel(condition = "input.tabs == 'B'",
                                                    imageOutput("fgmap2",height ="100%"),
                                                    htmlOutput("intro_tabsetmap2")),
                                   conditionalPanel(condition = "input.tabs == 'C'",
                                                    imageOutput("fgmap3"),
                                                    br(),
                                                    br(),
                                                    br(),
                                                    br(),
                                                    br(),
                                                    br(),
                                                    br(),
                                                    br(),
                                                    htmlOutput("intro_b1a")),
                                   conditionalPanel(condition = "input.tabs == 'D'",
                                                    imageOutput("fgmap4"),
                                                    br(),
                                                    br(),
                                                    br(),
                                                    br(),
                                                    br(),
                                                    br(),
                                                    htmlOutput("intro_tabset4a")),     
                                   conditionalPanel(condition = "input.tabs == 'E'",
                                                    imageOutput("fgmap5")),
                                   conditionalPanel(condition = "input.tabs == 'F'",
                                                   imageOutput("tabpic6b",height ="100%"),
                                                   htmlOutput("intro_tabset6a")
                                                   )
                            )#close column
                        ) #close fluidRow1 
               ), #close tabPanel 
               ##### Fish sp tab - option selectors ######  
               tabPanel("Fish Species",
                        fluidRow(
                            column(width = 7,
                                   fluidRow(
                                       column(width=3,
                                              # selectInput("species",label="Species",
                                              #             choices= list("Cod","Boarfish","Haddock","Herring",
                                              #                           "Hake","Horse Mackerel","Ling","Mackerel",
                                              #                           "Megrim","White-bellied Anglerfish",
                                              #                           "Black-bellied Anglerfish","Plaice", "Sole","Sprat",
                                              #                           "Blue Whiting","Whiting","Saithe","Pollack"),
                                              #             selected= "Cod" ),
                                              # We set the species list and default selection in server.R now 
                                              selectInput("species",label="Species",
                                                          choices= NULL,
                                                          selected= NULL ),
                                              conditionalPanel(condition = "input.fishtab == 'A'",
                                                               selectInput(inputId="biooptionselection", label="Select parameter", 
                                                                           choices=list("None","Age","Sex","Presentation","Gear","Sample Type"),
                                                                           selected = "None")),
                                              conditionalPanel(condition = "input.fishtab == 'B'",
                                                               selectInput(inputId="ageoptionselection", label="Select parameter", 
                                                                           choices=list("None","Sex","Presentation","Gear","Sample Type"),
                                                                           selected = "None")),
                                              conditionalPanel(condition = "input.biooptionselection =='Gear' && input.fishtab == 'A'",
                                                               uiOutput("GearFilter")),
                                              conditionalPanel(condition = "input.ageoptionselection =='Gear' && input.fishtab == 'B'",
                                                               uiOutput("GearFilter.a")                
                                              )),
                                       column(width=4,
                                              conditionalPanel(condition="input.fishtab == 'A'",
                                                               uiOutput("quarterfilter"),
                                                               uiOutput("yearfilter")),
                                              conditionalPanel(condition="input.fishtab == 'B'",
                                                               uiOutput("quarterfilter.a"),
                                                               uiOutput("yearfilter.a"))
                                       ),
                                       column(width=5,
                                              conditionalPanel("input.fishtab == 'A'",
                                                               radioGroupButtons(
                                                                 inputId = "Id",
                                                                 label = "",
                                                                 choices = c("ICES Area", 
                                                                             "ICES Division"),
                                                                 direction = "horizontal",
                                                                 checkIcon = list(
                                                                   yes = tags$i(class = "fa fa-check-square", 
                                                                                style = "color: steelblue"),
                                                                   no = tags$i(class = "fa fa-square-o", 
                                                                               style = "color: steelblue"))
                                                               ),
                                                               
                                                               
                                                               uiOutput("spatialops.w")
                                              ), #- SubArea filter
                                              
                                              conditionalPanel("input.fishtab == 'A'",
                                                               downloadButton("downloadDatalw", "Download data")#,
                                                              # br(),
                                                              # downloadLink("downloadDatalw_full", "Download full dataset")
                                              ),
                                              conditionalPanel("input.fishtab == 'B'",
                                                               radioGroupButtons(
                                                                 inputId = "Id.a",
                                                                 label = "",
                                                                 choices = c("ICES Area", 
                                                                             "ICES Division"),
                                                                 direction = "horizontal",
                                                                 checkIcon = list(
                                                                   yes = tags$i(class = "fa fa-check-square", 
                                                                                style = "color: steelblue"),
                                                                   no = tags$i(class = "fa fa-square-o", 
                                                                               style = "color: steelblue"))
                                                               ),
                                                               uiOutput("spatialops.a")), #- SubArea filter
                                              conditionalPanel("input.fishtab == 'B'",                 
                                                               downloadButton("downloadDatala", "Download data",class="btn btn-outline-primary")#,
                                                               #br(),
                                                              #downloadLink("downloadDatala_full", "Download full dataset")
                                                               ))
                                   ),
                                   ##### Fish sp tab - Maps and plots  ######                                     
                                   fluidRow(
                                       column(width=12,
                                              conditionalPanel(condition = "input.fishtab == 'A'",
                                                               plotlyOutput("bio_lw")
                                                               %>% withSpinner(color="#0dc5c1")),
                                              conditionalPanel(condition = "input.fishtab == 'B'",
                                                               plotlyOutput("bio_la")
                                                               %>% withSpinner(color="#0dc5c1"))
                                       )),
                                   
                                   fluidRow(
                                       column(width=10, 
                                              conditionalPanel(condition = "input.fishtab == 'C'",
                                                               imageOutput("fish_b1", height="100%"),
                                                               tags$style(HTML(".js-irs-0 .irs-grid-pol.small {height: 4px;}")),
                                                               tags$style(HTML(".js-irs-1 .irs-grid-pol.small {height: 0px;}")),
                                                               sliderInput("slideryear", "Choose Year:",
                                                                           min = 2007, max = 2019, #change after yearly update..For year 2020 max year is 2019
                                                                           value = 2019, step = 1,
                                                                           sep = "",
                                                                           animate = TRUE),htmlOutput("LandingsDisttext")),offset=4,style = "margin-top:-5em"))
                            ), 
                            ##### Fish sp tab - Species tabsets #####
                            column(width = 5,
                                   tabsetPanel(id = "fishtab",
                                               tabPanel("Biology",value= "A", 
                                                        p(), htmlOutput("fish_biology"),
                                                        fluidRow(column(width=7,imageOutput("fish_drawing", height='100%')),
                                                                 column(width=5,conditionalPanel(condition = "input.species =='White-bellied Anglerfish' || input.species =='Black-bellied Anglerfish'",
                                                                                                 imageOutput("monk_belly"))))),     
                                               tabPanel("Age", value = "B", 
                                                        p(),
                                                        fluidRow(column(width=5, htmlOutput("ageingtxt")),
                                                                 column(width=7, imageOutput("speciesotolith", height='100%'))),
                                                        p(),
                                                        fluidRow(column(width=5,textInput("lengthcm", label = "Enter fish length in cm:"), value = 0),
                                                                 column(width=7,tags$b("Age range observed*:"), h4(textOutput("agerange")),
                                                                        tags$b("Modal age is:"),h4(textOutput("mode")),
                                                                        tags$small("*age range based on age readings and lengths taken from fish sampled at ports and the stockbook"))),
                                                        hr(),
                                                        column(width=5,actionButton("showhist",label = "Show Histogram")), 
                                                        plotlyOutput("age_hist")
                                               ),
                                               tabPanel("Distribution",value= "C",
                                                       
                                                        p(),htmlOutput("fish_distribution"),
                                                        p(),htmlOutput("fish_b1a"),
                                                        h3("Useful links for more information:"),
                                                        a(href=paste0("https://shiny.marine.ie/stockbook/"),
                                                          "The Digital Stockbook",target="_blank"),
                                                        p(),
                                                        a(href=paste0("https://www.marine.ie"),
                                                          "The Marine Institute webpage",target="_blank"),
                                                        p(),
                                                        "For any quaries contact",
                                                       a("informatics@marine.ie",href="informatics@marine.ie")
                                                        )
                                   )#close tabsetPanel
                            )#close column
                        )#close fluidRow   
               ) #close tabPanel
    )#close navbarPage
) #close shinyUI







