setwd("H:/species-dashboard")
library(dplyr)
library(tibble)
bio.data20190417 <- readRDS("H:/species-dashboard/bio.data.sample20190417.rds")
all.length <- readRDS("H:/species-dashboard/CompleteLengthCases20190321.rds")
all.age <-  readRDS("H:/species-dashboard/CompleteAgeCases20190321.rds")
sample.length <- readRDS("H:/species-dashboard/bio.data.sample20190417.rds")
sample.age <-  readRDS("H:/species-dashboard/cc.age.sample20190321.rds")


sample.length <- add_column(sample.length, after=1:nrow(sample.length))
sample.length.Hake <- filter(sample.length, Species=="Hake")
View(sample.length.Hake)


sample.length.ANK <- filter(sample.length, Species=="Black-bellied Anglerfish")
View(sample.length.ANK)
sample.length <- filter(sample.length, after!=319213)
saveRDS(sample.length, file = "bio.data.sample20190423.rds")


