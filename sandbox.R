options(scipen = 999)

library(anytime)
library(data.table)
library(stringr)
library(lubridate)

if (!exists("fullData")) { fullData <- read.csv("parking.csv",sep=";",header=T) }
if (!exists("missingData")) { missingData <- read.csv("parkeergarages.csv") }
fullData <- data.table(fullData) # Data from 2016-12
missingData <- data.table(missingData) # Data from 2016-12

# Reorder data
fullData <- fullData[order(timestamp)]
missingData <- missingData[order(Einde.Periode)]

###################################################################################################
# fullData cleaning
###################################################################################################

# Replace Parking ID's with names
fullData$entity_entityid <- as.character(fullData$entity_entityid)
fullData[entity_entityid==20031]$entity_entityid <- "P01 Vrijdagmarkt"
fullData[entity_entityid==20019]$entity_entityid <- "P02 Reep"
fullData[entity_entityid==20027]$entity_entityid <- "P04 Savaanstraat"
fullData[entity_entityid==20015]$entity_entityid <- "P07 Sint-Michiels"
fullData[entity_entityid==20004]$entity_entityid <- "P08 Ramen"
fullData[entity_entityid==20023]$entity_entityid <- "P10 Sint-Pietersplein"

# Add both parking ID and name as column
fullData[,c("parkingID","parkingName") := list(substr(entity_entityid,1,3),substr(entity_entityid,5,nchar(entity_entityid)))]

# Filter unnessary rows out of dataset
fullData <- subset(fullData,variable_variableid %in% c(20009,20006))

# Convert timestamp to date
fullData$timestamp <- anytime(as.numeric(substr(fullData$timestamp,1,10)))

# Remove full capacity
fullData <- subset(fullData,variable_variableid == 20006)

# Remove unnecessary columns
fullData <- fullData[,c("variable_variableid","entity_entityid","end_point_type","entry_id"):=NULL]

setcolorder(fullData,c("timestamp","parkingID","parkingName","value"))

###################################################################################################
# missingData cleaning
###################################################################################################

# Split parking name in ID and Name
missingData <- cbind(as.data.table(str_split_fixed(missingData$Parking," ",2)),missingData)
missingData <- missingData[,c("Open.Gesloten","Totaal","Begin.Periode","Vol.Vrij","Parking"):=NULL]
colnames(missingData) <- c("parkingID","parkingName","value","timestamp")
missingData$timestamp <- as.POSIXct(substr(missingData$timestamp,1,19),"Europe/Paris")
setcolorder(missingData,c("timestamp","parkingID","parkingName","value"))

# Assemble both datasets
fullData <- subset(fullData,timestamp < "2016-12-01")
allData <- data.table(rbind(fullData,missingData))

# Aggregate data per hour
minute(allData$timestamp) <- 0
second(allData$timestamp) <- 0
hourData <- allData[,.(vrij = mean(value)),by=.(timestamp,parkingID,parkingName)]
hour(allData$timestamp) <- 0
dayData <- allData[,.(vrij = mean(value)),by=.(timestamp,parkingID,parkingName)]

# To do
## Clean data -- DONE!
## Aggregate per hour/day/week/etc -- DONE!
## Time series
## Histograms per aggregation
## Correlation with other data sets? Traffic count? VISA transactions? Cambio or bluebike occupation?