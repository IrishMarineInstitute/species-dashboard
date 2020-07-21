# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Load in the data



SpeciesList<-read.csv("Data/SpeciesList.csv")


bio.data <- readRDS("Data/bio.data20200319.rds")
cc.age<- readRDS("Data/cc.age20200319.rds")


Supp_table <- read.csv('Data/Supplemental data.csv', header=TRUE, sep = ",")

Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# Define server logic 
shinyServer(function(input, output, session){
    
    ## Read parameter strings from the URL and update the "species" selectInput appropriately
    observe({
        urlParameters <- parseQueryString(session$clientData$url_search)
        ## If we have a species parameter in the URL we will try and use it to
        ## choose our default species
        if (!is.null(urlParameters[['species']])) {
            
            speciesURLParameter <- urlParameters[['species']]
            
            # Try and find the description for the parameter passed in
            speciesURLName <- SpeciesList[tolower(SpeciesList$IC_Species)==tolower(speciesURLParameter),"Species_Name"]
            
            # If we didn't get a match just use the first species in the data frame as the default species
            if (length(speciesURLName) == 0){
                speciesURLName <- SpeciesList[1,"Species_Name"]
            }
            
            updateSelectInput(session, 
                              "species",label="Species",
                              choices= SpeciesList$Species_Name,
                              selected= speciesURLName )
            
            
            #Show the Fish Species tab using SELECT - this is a bit of hack to make sure the
            # user is taken to the Fish Species page first
            showTab("TopLevelMenu","Fish Species",select= TRUE, session)
            
        } 
        ## Else we 'll just use the first species in the data frame as the default species
        else 
        {
            updateSelectInput(session, 
                              "species",label="Species",
                              choices= SpeciesList$Species_Name,
                              selected= SpeciesList[1,"Species_Name"] )
            
        }
    })
    
    
    # showModal(modalDialog(
    #     title = "Please note",
    #     "This app is best viewed using Google Chrome",
    #     footer = modalButton("OK"),
    #     easyClose = TRUE
    # ))
    
    ###### Introduction page #######
    output$intromap1 <- renderLeaflet({
        leaflet() %>% 
            addProviderTiles(providers$Esri.OceanBasemap) %>% 
            setView(lng = -8.2124, lat = 53.2734, zoom = 6)
    })
    output$fgmap1<-renderImage({ 
        filename <- normalizePath(file.path('www',paste("FishingGrounds",'.jpg', sep='')))
        list(src = filename, width = 500, height= 570)}, deleteFile = FALSE) 
    output$fgmap2<-renderImage({
        filename <- normalizePath(file.path('www',paste("PortPie",'.png', sep='')))
        list(src = filename, width =500, height= 550, align = 'center')}, deleteFile = FALSE)
    output$fgmap3<-renderImage({
        filename <- normalizePath(file.path('www',paste("GearTypes",'.png', sep='')))
        list(src = filename, width =500, height= 550)}, deleteFile = FALSE) 
    output$fgmap4<-renderImage({
        filename <- normalizePath(file.path('www',paste("NationalityAllGears",'.png', sep='')))
        list(src = filename, width =475, height= 515)}, deleteFile = FALSE)
    output$fgmap5<-renderImage({
        filename <- normalizePath(file.path('www/Ageing',paste("otoliths in head with inset copy",'.jpg', sep='')))
        list(src = filename, width =600, height= 500)}, deleteFile = FALSE) 
    output$tabmap2<-renderImage({
        filename <- normalizePath(file.path('www',paste("PortNames",'.png', sep='')))
        list(src = filename, width =350, height= 400, style="display: block; margin-left: auto; margin-right: auto;")},
        deleteFile = FALSE) 
    output$tabmap3<-renderImage({
        filename <- normalizePath(file.path('www',paste("GearTypes",'.png', sep='')))
        list(src = filename, width =550, height= 650)}, deleteFile = FALSE)
    output$gear_pic<-renderImage({
        filename <- normalizePath(file.path('www/GearPics',paste(input$gearpic,'.jpg', sep='')))
        list(src = filename, width =400, height= "auto")}, deleteFile = FALSE)
    output$tabpic5<-renderImage({
        filename <- normalizePath(file.path('www/Ageing',paste("agedexample",'.png', sep='')))
        list(src = filename, width =400, height= 250)}, deleteFile = FALSE)
    output$tabpic6a<-renderImage({
      filename <- normalizePath(file.path('www',paste("Data",'.jpg', sep='')))
      list(src = filename, width =750, height= 250)}, deleteFile = FALSE)
    output$tabpic6b<-renderImage({
      filename <- normalizePath(file.path('www',paste("Sampled",'.png', sep='')))
      list(src = filename, width =500, height= 550)}, deleteFile = FALSE)
    # output$tabpic6c<-renderImage({
    #   filename <- normalizePath(file.path('www',paste("RV2",'.jpeg', sep='')))
    #   list(src = filename, width =450, height= 250)}, deleteFile = FALSE)
    output$intro_tabset1<- renderText({
        as.character(Supp_table[which(Supp_table[,"Fish"] == "All Species"),"biology"])})
    output$intro_tabset2a<- renderText({
        as.character(Supp_table[which(Supp_table[,"Fish"] == "All Species"),"distribution"])})
    output$intro_tabset2b<- renderText({
        as.character(Supp_table[which(Supp_table[,"Fish"] == "All Species"),"tabset2b"])})
    output$intro_tabsetmap2 <- renderText({
        as.character(Supp_table[which(Supp_table[,"Fish"] == "All Species"),"map2"])})
    output$intro_tabset3<- renderText({
        as.character(Supp_table[which(Supp_table[,"Fish"] == "All Species"),"tabset3"])})
    output$intro_b1a<- renderText({
        as.character(Supp_table[which(Supp_table[,"Fish"] == "All Species"),"b1a"])})
    output$intro_tabset4a<- renderText({
        as.character(Supp_table[which(Supp_table[,"Fish"] == "All Species"),"tabset4a"])})
    output$intro_tabset4b<- renderText({
        as.character(Supp_table[which(Supp_table[,"Fish"] == "All Species"),"tabset4b"])})
    output$intro_tabset5 <- renderText({
        as.character(Supp_table[which(Supp_table[,"Fish"] == "All Species"),"Ageing"])})
    output$intro_tabset5b <- renderText({
        as.character(Supp_table[which(Supp_table[,"Fish"] == "All Species"),"Ageing2"])})
    output$intro_tabset6 <- renderText({
      as.character(Supp_table[which(Supp_table[,"Fish"] == "All Species"),"DataCol"])})
    output$intro_tabset6a <- renderText({
      as.character(Supp_table[which(Supp_table[,"Fish"] == "All Species"),"Catchmap"])})
    output$intro_tabset6b <- renderText({
      as.character(Supp_table[which(Supp_table[,"Fish"] == "All Species"),"Catch"])})
    ####### Fish Species page #######
    output$fish_b1a<- renderText({
        as.character(Supp_table[which(Supp_table[,"Fish"] %in% input$species),"b1a"])
    })
    output$fish_biology<- renderText({
        as.character(Supp_table[which(Supp_table[,"Fish"] %in% input$species),"biology"])
    })
    output$fish_distribution<-renderText({
        as.character(Supp_table[which(Supp_table[,"Fish"] %in% input$species),"distribution"])
    })
    output$fish_drawing<- renderImage({
        filename <- paste("www/FishSketches/", Supp_table[which(Supp_table[,"Fish"] %in% input$species),"Fish"], ".png", sep="")
        list(src = filename, filetype = "png",width= "auto", height = 150)}, deleteFile = FALSE)
    output$monk_belly<- renderImage({
        filename <- paste("www/FishSketches/BUD&PISC", ".jpg", sep="")
        list(src = filename, filetype = "jpg",width= "auto", height = 150)}, deleteFile = FALSE)
    output$fish_b1<-renderImage({
        filename <- paste("www/LandingsDist/Landings", input$slideryear, "/Land", 
                          Supp_table[which(Supp_table[,"Fish"] %in% input$species),"Fish_Code"],".png", sep="")
        list(src = filename, width ="auto", height= 550)}, deleteFile = FALSE)
    output$ageingtxt <- renderText({
        as.character(Supp_table[which(Supp_table[,"Fish"] %in% input$species),"Ageing2"])
    })
    
    
    ##### Age/length widget ######
    x1 <- reactive({cc.age$Age[which(cc.age$Species==paste(SpeciesList[which(SpeciesList$Species_Name==input$species),][[2]]) & 
                                         cc.age$Length== paste(input$lengthcm))]})
    output$agerange <- reactive({
        if (length(x1())<3) {
            paste("No individuals of this length recorded",sep="")
        }else{ 
            paste(min(x1()),"to", max(x1()), "years,", "n = ",length(x1()), sep=" ")
        }
    })
    output$mode <- reactive({
        paste(Mode(x1()), sep=" ")
    })
    
    
    ##### Histogram #######
    observeEvent(input$showhist, {
        y1 <- reactive({cc.age$Age[which(cc.age$Species==paste(SpeciesList[which(SpeciesList$Species_Name==input$species),][[2]]) & cc.age$Length== paste(input$lengthcm))]})
        
        output$age_hist <- renderPlotly({
          yy<-as.data.frame(y1())
          yy$age<-as.factor(yy$y1)
          p<-ggplot(yy,aes(age))+geom_bar(color="black",fill="white",width = 1)+
          labs(title ='Histogram of observered ages',x="Age" )
          ggplotly(p)
         
        })
    })
    
    ##### Distribtion slider text ####### 
    output$LandingsDisttext <- renderText({
        if(input$species=="Horse Mackerel")
        {if(input$slideryear>2014){paste(Supp_table[which(Supp_table[,"Fish"] %in% input$species),"LandingsDist"], sep="")}
            else{}}
        else{#if(input$slideryear==2018){
            #paste("The distribution of ", input$species, " landings by Irish vessels between 2014 and ", 
             # input$slideryear, ". ",  Supp_table[which(Supp_table[,"Fish"] %in% input$species),"LandingsDist"], sep="")}
            #else{
                paste("The distribution of ", input$species, " landings by Irish vessels during ", 
              input$slideryear, ". ", 
              Supp_table[which(Supp_table[,"Fish"] %in% input$species),"LandingsDist"], sep="")}
            #}
    })
    
######## Length/Weight #######
grsp <-reactive({
 filter(bio.data,Species==as.character(SpeciesList[which(SpeciesList$Species_Name==input$species),][[2]]))})
  
    # Reactive quarter filter based on quarters available by species
    output$quarterfilter<- renderUI({
        quarterlist2<-c("All",levels(na.omit(as.factor(grsp()$Quarter))))
        selectInput("quarter","Quarter", choices=quarterlist2, selected = "All")
    })
    
    # Reactive year filter based on years available by species
    output$yearfilter<- renderUI({
       
        sliderInput("year","Years", min=min(grsp()$Year, na.rm=TRUE), max=max(grsp()$Year, na.rm=TRUE), 
                    # value =c(min(grsp()$Year, na.rm=TRUE),max(grsp()$Year, na.rm=TRUE)) ,sep="", step=1)##all years
                    value =max(grsp()$Year, na.rm=TRUE) ,sep="", step=1)##by one year
        
    })
    
    # Reactive gear filter based on gears available by species
    output$GearFilter <- renderUI({
        gearlist <- unique(grsp()$Gear)
        gearlist2 = factor(append("All", as.character(gearlist)))
        selectInput(inputId="gearselect", label="Select gear type", choices=gearlist2, selected= "All")
    })
    
    #Filtering grsp() (full) data based on filters above
    # grspnew.w<- reactive({
    #     if(is.null(input$year[1]) || is.null(input$year[2])){
    #         grspyear=grsp()
    #     }else if(input$year[1]==min(grsp()$Year, na.rm=TRUE)&&input$year[2]==max(grsp()$Year, na.rm=TRUE)){
    #         grspyear=grsp()
    #     }else{
    #         grspyear<- filter(grsp(), Year %in% input$year[1]:input$year[2])
    #     }
    #     if(input$quarter == "All" || is.null(input$quarter)){
    #         grspqtr = grspyear
    #     }else{
    #         grspqtr<- filter(grspyear, Quarter %in% input$quarter )
    #     }
    #     if(input$gearselect == "All"|| is.null(input$gearselect)|| input$biooptionselection=="None"){
    #         grspgear = grspqtr  #grspmonth
    #     }else{
    #         grspgear <- filter(grspqtr, Gear %in% input$gearselect) #grspmonth
    #     }
    # })
    
    grspnew.w<- reactive({
        
        grspyear<- filter(grsp(), Year %in% input$year)
        
        if(input$quarter == "All" || is.null(input$quarter)){
            grspqtr = grspyear
        }else{
            grspqtr<- filter(grspyear, Quarter %in% input$quarter )
        }
        if(input$gearselect == "All"|| is.null(input$gearselect)|| input$biooptionselection=="None"){
            grspgear = grspqtr  #grspmonth
        }else{
            grspgear <- filter(grspqtr, Gear %in% input$gearselect) #grspmonth
        }
    })
  
   
    #Creating Sub Area filter based on the full data
    output$spatialops.w <- renderUI({
  #unqsub =  mixedsort(as.character(unique(grsp()$ICESSubArea)),numeric.type="roman")
   if(input$Id=="ICES Area"){   
       pickerInput(
           inputId = "subselect",
           label = "ICES Area",
           choices = str_sort(as.character(unique(grsp()$ICESSubArea)),numeric = TRUE),
           selected=str_sort(as.character(unique(grsp()$ICESSubArea)),numeric = TRUE),
           options = list(
             `actions-box` = TRUE
           ),
           multiple = TRUE
       )}
    else{
      pickerInput(
        inputId = "subselect2",
        label = "ICES Division",
        choices = str_sort(as.character(unique(grsp()$ICESDivFullNameN )),numeric = TRUE),
        selected=str_sort(as.character(unique(grsp()$ICESDivFullNameN )),numeric = TRUE),
        options = list(
          `actions-box` = TRUE
        ),
        multiple = TRUE
      )}
})
    
    
  
    #Update the Area filter based on the full data being filtered by year, quarter, #month and gear
      observe({
        if(input$Id=="ICES Area"){     
          if(is.null(input$year)){
              return()
         }else{# ....
  
           x <- str_sort(as.character(unique(grspnew.w()$ICESSubArea)),numeric = TRUE)
          updatePickerInput(session, "subselect",label="ICES Area", choices=x,selected = x)
          
         }}
        else{
          if(is.null(input$year)){
            return()
          }else{# ....
            
            x <- str_sort(as.character(unique(grspnew.w()$ICESDivFullNameN )),numeric = TRUE)
            updatePickerInput(session, "subselect2",label="ICES Division", choices=x,selected = x)
            
          }
        }
      })
  
    
    
    #Filter based on Sub Area
    grspnew.w1<- reactive({
      if(input$Id=="ICES Area"){
            grspSub <- filter(grspnew.w(),ICESSubArea %in% input$subselect)}
      else{
        grspSub <- filter(grspnew.w(),ICESDivFullNameN %in% input$subselect2)
      }
    })
    
    
    ###Data Downloader widget  
    output$downloadDatalw <- downloadHandler(
        filename = function() {
            paste(input$species, "-LWdata",".csv", sep = "")
        },
        content = function(file) {
            #write.csv(grspnew.w1(), file, row.names = FALSE) 
            write.csv(grsp(), file, row.names = FALSE) 
        })
    
    
    
    ###Plotly charts
    output$bio_lw<- renderPlotly({
        if(input$biooptionselection=="Sex"){
            grspnew.w1 <- filter(grspnew.w1(), !is.na(Sex))
            p <- plot_ly(grspnew.w1(), x = ~Length, y = ~Weight, type = 'scatter', 
                         text=~paste("length:",Length,"cm","<br>weight:",Weight, "grams<br>date:", Date),
                         hoverinfo='text',
                         color = ~Sex, colors="Set1",
                             #c('M'='#6699ff','O'='#cccccc','U'='#999999','F'='#ff66cc','I'='#ccff99'),
                         mode = 'markers', marker =list(opacity = 0.5)) %>% 
                layout(hovermode=TRUE, title=paste(input$species,"Length vs Weight (points coloured by sex)"),
                       xaxis = list(title = 'Length (cm)', range= c(min(grspnew.w1()$Length), max(grspnew.w1()$Length)+1)),
                       yaxis = list(title = 'Weight (g)', range = c(0, max(grspnew.w1()$Weight, na.rm = T)*1.05)),
                       margin=(list(t=70)),
                       showlegend = TRUE) 
            p$elementId <- NULL
            p 
        }else if(input$biooptionselection=="Age"){
            grspnew.w1 <- filter(grspnew.w1(), Age>-1)
            p <- plot_ly(grspnew.w1(), x = ~Length, y = ~Weight, type = 'scatter', mode = 'markers',hoverinfo='text',
                         text=~paste("length:",Length,"cm","<br>weight:",Weight, "grams<br>date:", Date, "<br>Age:", Age),
                         color= ~Age, colors = "Set1",marker =list(opacity = 0.5)) %>%  
                layout(hovermode=TRUE, title=paste(input$species,"Length vs Weight (points coloured by age)"),
                       xaxis = list(title = 'Length (cm)', range= c(min(grspnew.w1()$Length), max(grspnew.w1()$Length)+1)),
                       yaxis = list(title = 'Weight (g)', range = c(0, max(grspnew.w1()$Weight, na.rm = T)*1.05)),
                       margin=(list(t=70)),
                       showlegend = FALSE)
            p$elementId <- NULL
            p 
        }else if(input$biooptionselection=="Presentation"){
            grspnew.w1 <- filter(grspnew.w1(), !is.na(Presentation))
            p <- plot_ly(grspnew.w1(), x = ~Length, y = ~Weight, type = 'scatter', mode = 'markers',hoverinfo='text',
                         text=~paste("length:",Length,"cm","<br>weight:",Weight, "grams<br>date:", Date, "<br>presentation:", Presentation),
                         color= ~Presentation, colors = "Dark2") %>%  
                layout(hovermode=TRUE, title=paste(input$species,"Length vs Weight (points coloured by sample presentation)"),
                       xaxis = list(title = 'Length (cm)', range= c(min(grspnew.w1()$Length), max(grspnew.w1()$Length)+1)),
                       yaxis = list(title = 'Weight (g)', range = c(0, max(grspnew.w1()$Weight, na.rm = T)*1.05)),
                       margin=(list(t=70)),
                       showlegend = TRUE)
            p$elementId <- NULL
            p 
        }else if(input$biooptionselection=="Sample Type"){
            grspnew.w1 <- filter(grspnew.w1(), !is.na(Type))
            p <- plot_ly(grspnew.w1(), x = ~Length, y = ~Weight, type = 'scatter', mode = 'markers',hoverinfo='text',
                         text=~paste("length:",Length,"cm","<br>weight:",Weight, "grams<br>date:", Date, "<br>sample type:",Type), 
                         color= ~Type,colors =c('Discards'='red','Landings'='lightgreen')) %>%  
                layout(hovermode=TRUE, title=paste(input$species,"Length vs Weight (points coloured by sample type)"),
                       xaxis = list(title = 'Length (cm)', range= c(min(grspnew.w1()$Length), max(grspnew.w1()$Length)+1)),
                       yaxis = list(title = 'Weight (g)', range = c(0, max(grspnew.w1()$Weight, na.rm = T)*1.05)),
                       margin=(list(t=70)),
                       showlegend = TRUE)
            p$elementId <- NULL
            p 
        }else if(input$biooptionselection=="Gear"){
            grspnew.w1 <- filter(grspnew.w1(), !is.na(Gear))
            p <- plot_ly(grspnew.w1(), x = ~Length, y = ~Weight, type = 'scatter', mode = 'markers',hoverinfo='text',
                         text=~paste("length:",Length,"cm","<br>weight:",Weight, "grams<br>date:", Date, "<br>gear type:",Gear),
                         color= ~Gear,colors = "Set1") %>%  
                layout(hovermode=TRUE, title=paste(input$species,"Length vs Weight (points coloured by gear type)"),
                       xaxis = list(title = 'Length (cm)', range= c(min(grspnew.w1()$Length), max(grspnew.w1()$Length)+1)),
                       yaxis = list(title = 'Weight (g)', range = c(0, max(grspnew.w1()$Weight, na.rm = T)*1.05)),
                       margin=(list(t=70)),
                       showlegend = TRUE)
            p$elementId <- NULL
            p 
        }
        else{
            p <- plot_ly(grspnew.w1(), x = ~Length, y = ~Weight, type = 'scatter',color=~Weight, colors="Spectral",
                         mode = 'markers', marker =list(opacity = 0.5), hoverinfo='text',
                         text=~paste("length:",Length,"cm<br>weight:",Weight, "grams<br>Date:", Date)) %>%
                layout(hovermode=TRUE, title=paste(input$species," Length vs Weight", sep=""),
                       xaxis = list(title = 'Length (cm)', range= c(min(grspnew.w1()$Length), max(grspnew.w1()$Length)+1)),
                       yaxis = list(title = 'Weight (g)', range = c(0, max(grspnew.w1()$Weight, na.rm = T)*1.05)),
                       margin=(list(t=80)),
                       showlegend = FALSE)
            p$elementId <- NULL
            p
           
        }
    })   
    
######## Age/Weight ########
    output$speciesotolith<-renderImage({
        filename <- normalizePath(file.path('www/Ageing',paste(input$species,'.png', sep='')))
        list(src = filename, width =300)}, deleteFile = FALSE)
    
cc.a<-reactive({filter(cc.age,Species==as.character(SpeciesList[which(SpeciesList$Species_Name==input$species),][[2]]))})  
# Reactive year filter based on years available by species
output$yearfilter.a<- renderUI({
    sliderInput("year.a","Years", min=min(cc.a()$Year, na.rm=TRUE), max=max(cc.a()$Year, na.rm=TRUE), 
                # value =c(min(cc.a()$Year, na.rm=TRUE),max(cc.a()$Year, na.rm=TRUE)), sep="", step=1)
                value =max(cc.a()$Year, na.rm=TRUE), sep="", step=1)
    #
})

# Reactive quarter filter based on quarters available by species
output$quarterfilter.a<- renderUI({
    quarterlist2<-c("All",levels(na.omit(as.factor(cc.a()$Quarter))))
 selectInput("quarter.a","Quarter", choices=quarterlist2, selected = "All")
})

# Reactive gear filter based on gears available by species
output$GearFilter.a <- renderUI({
    gearlist <- unique(cc.a()$Gear)
    gearlist2 = factor(append("All", as.character(gearlist)))
    selectInput(inputId="gearselect.a", label="Select gear type", choices=gearlist2, selected= "All")
}) 



grspage <- reactive({
    grspageyear<- filter(cc.a(), Year %in% input$year.a)
    
    if(input$quarter.a == "All" || is.null(input$quarter.a)){
        grspageqtr = grspageyear
    }else{
        grspageqtr<- filter(grspageyear, Quarter %in% input$quarter.a )
    }
    if(input$gearselect.a == "All"|| is.null(input$gearselect.a)|| input$ageoptionselection=="None"){
        grspagegear = grspageqtr #grspagemonth
    }else{
        grspagegear <- filter(grspageqtr, Gear %in% input$gearselect.a)  #grspagemonth
    }
})

#Creating Sub Area filter based on the full data
 output$spatialops.a <- renderUI({
#     unqsub.a = factor(append("All", as.character(unique(cc.a()$ICESDivFullName))))
#      checkboxGroupInput(inputId = "subselect.a",label= "ICES Area", choices = as.list(unqsub.a),
#                         selected= "All", inline = TRUE)
#     
 

if(input$Id.a=="ICES Area"){   
  pickerInput(
    inputId = "subselect.a",
    label = "ICES Area",
    choices = str_sort(as.character(unique(cc.a()$ICESSubArea)),numeric = TRUE),
    selected=str_sort(as.character(unique(cc.a()$ICESSubArea)),numeric = TRUE),
    options = list(
      `actions-box` = TRUE
    ),
    multiple = TRUE
  )}
else{
  pickerInput(
    inputId = "subselect2.a",
    label = "ICES Division",
    choices = str_sort(as.character(unique(cc.a()$ICESDivFullNameN )),numeric = TRUE),
    selected=str_sort(as.character(unique(cc.a()$ICESDivFullNameN )),numeric = TRUE),
    options = list(
      `actions-box` = TRUE
    ),
    multiple = TRUE
  )}
    })

#Update the Area filter based on the full data being filtered by year, quarter, month and gear
observe({
     # if(is.null(input$year.a)){#} |is.na(input$year.a)  ){
     #    return()
     # }else{# ....
     #     x <-factor(append("All", as.character(unique(grspage()$ICESDivFullName))))
     #     updateCheckboxGroupInput(session, "subselect.a",label="ICES Areas", choices=x,
     #                              selected= "All", inline = TRUE)
     #    
     # }
  if(input$Id.a=="ICES Area"){     
    if(is.null(input$year.a)){
         return()
    }else{# ....
      
      x <- str_sort(as.character(unique(grspage()$ICESSubArea )),numeric = TRUE)
      updatePickerInput(session, "subselect.a",label="ICES Area", choices=x,selected = x)
      
    }}
  else{
    if(is.null(input$year.a)){
      return()
    }else{# ....
      
      x <- str_sort(as.character(unique(grspage()$ICESDivFullNameN )),numeric = TRUE)
      updatePickerInput(session, "subselect2.a",label="ICES Division", choices=x,selected = x)
      
    }
  }
})

#Filter based on Area
grspnew.a1<- reactive({
    if(input$Id.a=="ICES Area"){
        grspageSub = filter(grspage(), ICESSubArea %in% input$subselect.a)
    }else{
        grspageSub <- filter(grspage(), ICESDivFullNameN %in% input$subselect2.a)}
  
})

####Age Data downloader
output$downloadDatala <- downloadHandler(
    filename = function() {
        paste(input$species, "-LAdata", ".csv", sep = "")
    },
    content = function(file) {
        #write.csv(grspnew.a1(), file, row.names = FALSE)
        write.csv(cc.a(), file, row.names = FALSE)
    })

output$bio_la<- renderPlotly({
    if(input$ageoptionselection=="Sex"){
        p <- plot_ly(grspnew.a1(), x = grspnew.a1()$AgeContin , y =grspnew.a1()$Length,
                     type = 'scatter', mode = 'markers',hoverinfo='text',
                     text=~paste("length:",Length,"cm","<br>age:",AgeContin, "<br>date:", Date), 
                     color = ~Sex, colors = "Set1",
                     #colors=c('U'='#999999','M'='#6699ff','O'='#cccccc','F'='#ff66cc','I'='#ccff99'),
                     mode = 'markers') %>% 
            layout(hovermode=TRUE, title=paste(input$species,"age at length (points coloured by sex)"),
                   xaxis = list(title = 'Age', range= c(0, max(grspnew.a1()$AgeContin)+1)),
                   yaxis = list(title = 'Length (cm)', range= c(min(grspnew.a1()$Length), max(grspnew.a1()$Length)+1)),
                   margin=(list(t=50)),
                   showlegend = TRUE) 
        p$elementId <- NULL
        p 
    }else if(input$ageoptionselection=="Presentation"){
        grspnew.a1 <- filter(grspnew.a1(), !is.na(Presentation))
        p <- plot_ly(grspnew.a1(), x = grspnew.a1()$AgeContin, y = grspnew.a1()$Length, 
                     type = 'scatter', mode = 'markers',hoverinfo='text',
                     text=~paste("length:",Length,"cm","<br>age:",AgeContin, "<br>date:", Date, "<br>presentation:", Presentation),
                     color= ~Presentation,colors = "Dark2") %>%  
            layout(hovermode=TRUE, title=paste(input$species,"age at length (points coloured by presentation)"),
                   xaxis = list(title = 'Age', range= c(0, max(grspnew.a1()$AgeContin)+1)),
                   yaxis = list(title = 'Length (cm)', range= c(min(grspnew.a1()$Length), max(grspnew.a1()$Length)+1)),
                   margin=(list(t=50)),
                   showlegend = TRUE) 
        p$elementId <- NULL
        p 
    }else if(input$ageoptionselection=="Sample Type"){
        grspnew.a1 <- filter(grspnew.a1(), !is.na(Type))
        p <- plot_ly(grspnew.a1(), x = grspnew.a1()$AgeContin, y = grspnew.a1()$Length,
                     type = 'scatter', mode = 'markers',hoverinfo='text',
                     text=~paste("length:",Length,"cm","<br>age:",AgeContin, "<br>date:", Date, "<br>sample type:",Type),
                     color= ~Type,colors =c('Discards'='red','Landings'='lightgreen')) %>%  
            layout(hovermode=TRUE, title=paste(input$species,"age at length (points coloured by sample type)"),
                   xaxis = list(title = 'Age', range= c(0, max(grspnew.a1()$AgeContin)+1)),
                   yaxis = list(title = 'Length (cm)', range= c(min(grspnew.a1()$Length), max(grspnew.a1()$Length)+1)),
                   margin=(list(t=50)),
                   showlegend = TRUE) 
        p$elementId <- NULL
        p 
    }else if(input$ageoptionselection=="Gear"){
        grspnew.a1 <- filter(grspnew.a1(), !is.na(Gear))
        p <- plot_ly(grspnew.a1(), x = grspnew.a1()$AgeContin, y = grspnew.a1()$Length,
                     type = 'scatter', mode = 'markers',hoverinfo='text',
                     text=~paste("length:",Length,"cm","<br>age:",AgeContin, "<br>date:", Date, "<br>gear type:",Gear),
                     color= ~Gear,colors = "Set1") %>%  
            layout(hovermode=TRUE, title=paste(input$species,"age at length (points coloured by gear type)"),
                   xaxis = list(title = 'Age', range= c(0, max(grspnew.a1()$AgeContin)+1)),
                   yaxis = list(title = 'Length (cm)', range= c(min(grspnew.a1()$Length), max(grspnew.a1()$Length)+1)),
                   margin=(list(t=50)),
                   showlegend = TRUE) 
        p$elementId <- NULL
        p 
    }else{
        p <- plot_ly(grspnew.a1(), x = grspnew.a1()$AgeContin, y = grspnew.a1()$Length,
                     color= ~Age, colors = 'Paired',hoverinfo='text',
                     type = 'scatter', mode = 'markers', marker =list(opacity = 0.5),
                     text=~paste("length:",Length,"cm","<br>age:",AgeContin))%>% 
            layout(hovermode=TRUE, title=paste(input$species,"age at length"),
                   xaxis = list(title = 'Age', range= c(0, max(grspnew.a1()$AgeContin)+1)),
                   yaxis = list(title = 'Length (cm)', range= c(0, max(grspnew.a1()$Length)+1)),
                   margin=(list(t=50)),
                   showlegend = FALSE)
        p$elementId <- NULL
        p
    }
})    
}
)