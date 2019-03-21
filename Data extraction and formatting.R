library(RODBC)
library(dplyr)
library(lubridate)
library(plotly)
getwd()

Q<- "
SELECT *
FROM [InformaticsLoad].[dbo].[SpeciesResultSetExpanded]
"
channel <- odbcDriverConnect("Driver=SQL Server; 
                             Server=VMINFORMDEV01; Database=InformaticsLoad")
SDdata<- sqlQuery(channel,Q)
close(channel)
#saveRDS(SDdata, file = "FullData.SQL20180510.rds")
#SDdata<- readRDS(file="FullData.SQL20180510.rds")

####### Formatting #######
levels(as.factor(SDdata$Species))
SDdata$Species <- trimws(SDdata$Species)

#SDdata$ICESDivFullName <- trimws(SDdata$ICESDivFullName)

head(SDdata) #check white space removed
SDdata<-SDdata[SDdata$Species %in% c("Atlantic Herring","Atlantic Mackerel", "Blackbellied Angler",
                                     "Blue Whiting","Boarfish",
                                     "Cod Atlantic","Haddock","Hake European",
                                     "Horse Mackerel Atlantic","Lemon Sole","Ling","Megrim","Monkfish Angler",
                                     "Monkfish Angler nei","Plaice","Pollack",
                                     "Saithe","Sole Black","Sprat European",
                                     "Whiting"),]
SDdata$Species <- droplevels(as.factor(SDdata$Species)) #drop empty levels of species
SDdata <-  SDdata %>%
  mutate(Species = 
           recode(Species,
                  'Atlantic Herring' = "Herring",
                  'Cod Atlantic' = "Cod",
                  'Blackbellied Angler' = "Black-bellied Anglerfish",
                  'Monkfish Angler nei' = "Monkfish",
                  'Monkfish Angler' = "White-bellied Anglerfish",
                  'Atlantic Mackerel' = "Mackerel",
                  "Hake European" = "Hake",
                  'Lemon Sole' = "Sole",
                  'Sole Black' = "Sole",
                  'Sprat European' = "Sprat",
                  'Horse Mackerel Atlantic' = "Horse Mackerel"
           ))
SDdata$Presentation <-droplevels(SDdata$Presentation)



####### Complete Cases #######

### Length ###
bio.data.length <- SDdata
dataV2 <- bio.data.length[!is.na(bio.data.length$Length),] 
dataV3 <- dataV2[!is.na(dataV2$Weight),]
dim(dataV3)
cc.length <- dataV3
cc.length <- cc.length[cc.length$Weight>0,]
saveRDS(cc.length, file= "CompleteLengthCases20180510.rds") ##change to todays date before running
#cc.length <-  readRDS(file="CompleteLengthCases20180510.rds")
bio.data.full<- cc.length
bio.data <- sample_frac(bio.data.full, 0.1)
saveRDS(bio.data, file = "bio.data.sample20180510.rds")

### Age ###
bio.data.age <- SDdata
dataA2 <- bio.data.age[!is.na(bio.data.age$Age),]
dataA3 <- dataA2[!is.na(dataA2$Length),]
#dim(dataA3)
cc.age<-dataA3
cc.age <- cc.age[!is.na(cc.age$Date),]
#cc.age$Date <- trim (cc.age$Date 
cc.age<- cc.age %>%
  mutate(decimaldate = decimal_date(Date))
cc.age <- cc.age %>% 
  mutate(justdecimal= cc.age$decimaldate-cc.age$Year)
cc.age <- cc.age %>%
  mutate(AgeContin = cc.age$Age + cc.age$justdecimal)
cc.age <- cc.age[!cc.age$Age <0.1,]
saveRDS(cc.age, file= "CompleteAgeCases20180510.rds") ##change to todays date before running
#cc.age <-  readRDS(file="CompleteAgeCases20180510.rds")
cc.age.full<- readRDS("CompleteAgeCases20180510.rds")
cc.age <- sample_frac(cc.age.full, 0.1)
saveRDS(cc.age, file = "cc.age.sample20180510.rds")
