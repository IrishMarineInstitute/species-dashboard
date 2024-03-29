library(RODBC)
library(plyr)
library(dplyr)
library(lubridate)
library(plotly)

Q<- "
SELECT *
FROM [InformaticsLoad].[dbo].[SpeciesResultSetExpanded]
"
channel <- odbcDriverConnect("Driver=SQL Server; 
                             Server=XXXXXXXXX; Database=XXXXXXXXXX")
SDdata<- sqlQuery(channel,Q)



#saveRDS(SDdata, file = "FullData.rds")
###################################################

####Some of the species in table SpeciesResultSetExpanded changed since last updates data in 2019#####
####use Species table to compare names
Q<- "
SELECT *
FROM [InformaticsLoad].[dbo].[Species]
"
sp<-sqlQuery(channel,Q)

close(channel)
sp$CommonName <- trimws(sp$CommonName)##remove white space
com_sp<-filter(sp,CommonName%in%c("Angler","Atlantic Cod","Atlantic Herring",
"Atlantic Horse Mackerel","Atlantic Mackerel",
"Billet","Blackbellied Angler","Blue Whiting","Boarfish",
"Common Ling","Common Sole",
"European Hake","European Sprat","European Plaice",
"Green Pollack","Haddock","Megrim","Whiting"))
com_sp[c(3,22,24,28)]




sp<-SDdata

####### Formatting #######
#SDdata<-readRDS("Data/FullData.rds")
levels(as.factor(SDdata$Species))
SDdata$Species <- trimws(SDdata$Species)
head(SDdata) #check white space removed

# SDdata<-SDdata[SDdata$Species %in% c("Atlantic Herring","Atlantic Mackerel", "Blackbellied Angler",
#                                      "Blue Whiting","Boarfish",
#                                      "Cod Atlantic","Haddock","Hake European",
#                                      "Horse Mackerel Atlantic","Lemon Sole","Ling","Megrim","Monkfish Angler",
#                                      "Monkfish Angler nei","Plaice","Pollack",
#                                      "Saithe","Sole Black","Sprat European","Whiting"),]

SDdata<-SDdata[SDdata$Species %in% c("Atlantic Herring","Atlantic Mackerel","Blackbellied Angler",
                                     "Blue Whiting","Boarfish",
                                     "Atlantic Cod","Haddock","European Hake",
                                     "Atlantic Horse Mackerel", "Lemon Sole","Common Ling","Megrim","Angler",
                                     "European Plaice","Green Pollack",
                                     "Billet","Common Sole","European Sprat","Whiting"),]


levels(as.factor(SDdata$Species))
SDdata$Species <- droplevels(as.factor(SDdata$Species)) #drop empty levels of species

# SDdata <-  SDdata %>%
#   mutate(Species = 
#            recode(Species,
#                   'Atlantic Herring' = "Herring",
#                   'Cod Atlantic' = "Cod",
#                   'Blackbellied Angler' = "Black-bellied Anglerfish",
#                   'Monkfish Angler nei' = "Monkfish",
#                   'Monkfish Angler' = "White-bellied Anglerfish",
#                   'Atlantic Mackerel' = "Mackerel",
#                   "Hake European" = "Hake",
#                   'Lemon Sole' = "Sole",
#                   'Sole Black' = "Sole",
#                   'Sprat European' = "Sprat",
#                   'Horse Mackerel Atlantic' = "Horse Mackerel"
#            ))

####### Complete Cases #######
### Length ###
bio.data.length <- SDdata
dataV2 <- bio.data.length[!is.na(bio.data.length$Length),] 
dataV3 <- dataV2[!is.na(dataV2$Weight),]
dim(dataV3)
cc.length <- dataV3
cc.length <- cc.length[cc.length$Weight>0,]
#saveRDS(cc.length, file= "Data/CompleteLengthCases20200319.rds") ##change to todays date before running

bio.data<- cc.length
bio.data.sample <- sample_frac(bio.data, 0.1)

bio.data<-filter(bio.data,Year!=2020)
bio.data<-droplevels(filter(bio.data,ICESSubArea!="U    "))
bio.data$ICESSubArea<-paste0("27.",bio.data$ICESSubArea)
bio.data$ICESSubArea<-trimws(bio.data$ICESSubArea)
bio.data$ICESDiv<-trimws(bio.data$ICESDiv)
bio.data$ICESSubArea<-as.factor(bio.data$ICESSubArea)
bio.data$ICESDiv<-as.factor(bio.data$ICESDiv)
levels(bio.data$ICESDiv)[10]<-"(unclassified)" #renaming "U"
bio.data$ICESDivFullNameN<-droplevels(interaction(bio.data$ICESSubArea,bio.data$ICESDiv))

bio.data$ICESSubArea<-as.factor(bio.data$ICESSubArea)
bio.data$ICESDivFullNameN<-as.factor(bio.data$ICESDivFullNameN)

saveRDS(bio.data, file = "Data/bio.data20200319.rds")

### Age ###
bio.data.age <- SDdata
dataA2 <- bio.data.age[!is.na(bio.data.age$Age),]
dataA3 <- dataA2[!is.na(dataA2$Length),]
dim(dataA3)
cc.age<-dataA3
cc.age <- cc.age[!is.na(cc.age$Date),]
cc.age<- cc.age %>%
  mutate(decimaldate = decimal_date(Date))
cc.age <- cc.age %>% 
  mutate(justdecimal= cc.age$decimaldate-cc.age$Year)
cc.age <- cc.age %>%
  mutate(AgeContin = cc.age$Age + cc.age$justdecimal)
cc.age <- cc.age[!cc.age$Age <0.1,]
cc.age<-filter(cc.age,Year!=2020)
cc.age<-droplevels(filter(cc.age,ICESSubArea!="U    "))
cc.age$ICESSubArea<-paste0("27.",cc.age$ICESSubArea)
cc.age$ICESSubArea<-trimws(cc.age$ICESSubArea)
cc.age$ICESDiv<-trimws(cc.age$ICESDiv)
cc.age$ICESSubArea<-as.factor(cc.age$ICESSubArea)
cc.age$ICESDiv<-as.factor(cc.age$ICESDiv)
levels(cc.age$ICESDiv)[10]<-"(unclassified)" #renaming "U"
cc.age$ICESDivFullNameN<-droplevels(interaction(cc.age$ICESSubArea,cc.age$ICESDiv))

cc.age$ICESSubArea<-as.factor(cc.age$ICESSubArea)
cc.age$ICESDivFullNameN<-as.factor(cc.age$ICESDivFullNameN)

saveRDS(cc.age, file = "Data/cc.age20200319.rds")




#########SpeciesList for server.R####

SpeciesList <- data.frame(IC_Species =c(as.character(com_sp$LogbooksFAOCode)),
                          Species_Name_full = c(as.character(com_sp$CommonName)),
                          Species_Name = c("Herring" ,"Sprat","Cod","Haddock",
                                           "Whiting","Blue Whiting","Pollack","Saithe","Ling",
                                           "Hake", "Black-bellied Anglerfish",
                                           "White-bellied Anglerfish","Horse Mackerel","Mackerel","Plaice",
                                          "Megrim","Sole","Boarfish"))
write.csv(SpeciesList,"Data/SpeciesList.csv",row.names=F)
