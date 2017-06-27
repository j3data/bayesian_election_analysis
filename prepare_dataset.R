# Predicting delayed flights at time of booking.
# No knowledge of weatherforecast

#install.packages("package")

library(readxl)
library(data.table)
library(chron)

##################
###    ETL    ####
##################

### 12 files of monthly data are downloaded to a local directory
### The source data includes all US airports, ~5.6M rows
### Only rows with Origin=Philadelphia are loaded

# set working directory
file_path <- 'C:\\statistics\\scratch\\flights\\data\\'
setwd(file_path)

# create list of files to load by name pattern
temp = list.files(pattern="*.csv")

# stream in and rbind tables keeping rows where Origin=PHL
bigtable <- data.table()
for (i in 1:length(temp)){
  tempDT <- as.data.table(fread(temp[i]),na.strings = c("NA","N/A",""))
  bigtable <- rbindlist(list(bigtable, tempDT[Origin=='PHL']))
  tempDT <- data.table()
  gc()
}

dim(bigtable)
# n = 73588

FLIGHTS.PHL <- bigtable

#########################
## Drop empty columns  ##
#########################

# Preview data
# Transpose the 110 columns to rows and look at the first few observations
snip <- cbind(order=seq_len(ncol(FLIGHTS.PHL)),t(FLIGHTS.PHL[DepDel15==1][1:3]))
snip

# FILTER: Flights not cancelled
PHL.tmp1 <- FLIGHTS.PHL[Cancelled==0]

# FILTER: Flights not diverted 
PHL.tmp2 <- PHL.tmp1[Diverted==0]

# DROP: Drop columns containing diverted flight data (i.e. Div* columns)
PHL.tmp3 <- PHL.tmp2[,-c(65:110)]

dim(PHL.tmp3)
# n = 72498

########################
## Keep key variables ##
########################

snip <- cbind(order=seq_len(ncol(PHL.tmp3)),t(PHL.tmp3[DepDel15==1][1:3]))
snip

## Variables of Interest

## PREDICTORS

# Year
# Quarter
# Month
# DayofMonth
# DayOfWeek
# FlightDate              Flight Date (yyyy-mm-dd)
# UniqueCarrier           Unique Carrier Code.
# Origin                  Origin Airport
# Dest                    Destination Airport
# CRSDepTime              CRS Departure Time (local time: hhmm)
# DepTimeBlk              CRS Departure Time Block, Hourly Intervals
# CRSArrTime              CRS Arrival Time (local time: hhmm)
# ArrTimeBlk              CRS Arrival Time Block, Hourly Intervals
# CRSElapsedTime          CRS Elapsed Time of Flight, in Minutes
# Distance                Distance between airports (miles)
# DistanceGroup           Distance Intervals, every 250 Miles, for Flight Segment

## RESPONSE VARIABLES

# DepDelay
# DepDelayMinutes         Difference in minutes between scheduled and actual 
#                           departure time. Early departures set to 0.	
# DepDel15                Departure Delay Indicator, 15 Minutes or More (1=Yes)
# DepartureDelayGroups    Departure Delay intervals, every (15 minutes from <-15 to >180)
# ArrDelay
# ArrDelayMinutes         Difference in minutes between scheduled and actual 
#                           arrival time. Early arrivals set to 0.
# ArrDel15                Arrival Delay Indicator, 15 Minutes or More (1=Yes)
# ArrivalDelayGroups      Arrival Delay intervals, every (15-minutes from <-15 to >180)

PHL <- PHL.tmp3[,.(Year, Quarter, Month, DayofMonth, DayOfWeek, FlightDate,
                   UniqueCarrier, Origin, Dest, CRSDepTime, DepTimeBlk,
                   CRSArrTime, ArrTimeBlk, CRSElapsedTime, Distance, DistanceGroup,
                   DepDelay, DepDelayMinutes, DepDel15, DepartureDelayGroups,
                   ArrDelay, ArrDelayMinutes, ArrDel15, ArrivalDelayGroups,
                   CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay)]

###########################################
### Update datetime and factor variables ##
###########################################

snip <- cbind(order=seq_len(ncol(PHL)),t(PHL[DepDel15==1][1:5]))
snip

# Convert variable to factor (Nominal)
PHL$DepDel15.nom                <- factor(PHL$DepDel15)
PHL$Year.nom                    <- factor(PHL$Year)
PHL$Quarter.nom                 <- factor(PHL$Quarter)
PHL$Month.nom                   <- factor(PHL$Month)
PHL$DayofMonth.nom              <- factor(PHL$DayofMonth)
PHL$DayOfWeek.nom               <- factor(PHL$DayOfWeek)
PHL$DistanceGroup.nom           <- factor(PHL$DistanceGroup)
PHL$DepartureDelayGroups.nom    <- factor(PHL$DepartureDelayGroups)
PHL$ArrivalDelayGroups.nom      <- factor(PHL$ArrivalDelayGroups)
PHL$DepTimeBlk.nom              <- factor(PHL$DepTimeBlk)
PHL$ArrTimeBlk.nom              <- factor(PHL$ArrTimeBlk)

# Convert variables to ordered factor (Ordinal)
PHL$Year.ord                    <- factor(PHL$Year, ordered = TRUE)
PHL$Quarter.ord                 <- factor(PHL$Quarter, ordered = TRUE)
PHL$Month.ord                   <- factor(PHL$Month, ordered = TRUE)

PHL$DayofMonth.ord              <- factor(PHL$DayofMonth, ordered = TRUE)
PHL$DayOfWeek.ord               <- factor(PHL$DayOfWeek, ordered = TRUE)
PHL$DistanceGroup.ord           <- factor(PHL$DistanceGroup, ordered = TRUE)
PHL$DepTimeBlk.ord              <- factor(PHL$DepTimeBlk, ordered = TRUE)
PHL$ArrTimeBlk.ord              <- factor(PHL$ArrTimeBlk, ordered = TRUE)

PHL$DepartureDelayGroups.ord    <- factor(PHL$DepartureDelayGroups, ordered = TRUE)
PHL$ArrivalDelayGroups.ord      <- factor(PHL$ArrivalDelayGroups, ordered = TRUE)

# Convert to datetime format (FlightDate)
PHL$FlightDate.dt               <- as.Date(PHL$FlightDate)

# Parse Departure Time (CRSDepTime)
# Using regular expressions, convert text to datetime format
PHL$CRSDepTime.dt               <- chron(times=sub("(.*)(\\d\\d)", "\\1:\\2:00", PHL$CRSDepTime))

## Parse Arrival Time (CRSArrTime)
PHL$CRSArrTime.num            <- as.numeric(PHL$CRSArrTime)

# CRSArrTime < 10
PHL[CRSArrTime.num < 10,CRSArrTime.dt:=chron(times=sub(".*(\\d)", "00:0\\1:00", CRSArrTime))]
PHL[CRSArrTime.num < 10,]

# CRSArrTime ∈ [10-59]
PHL[CRSArrTime.num > 9 & CRSArrTime.num < 60,CRSArrTime.dt:=chron(times=sub(".*(\\d\\d)", "00:\\1:00", CRSArrTime))]
PHL[CRSArrTime.num > 9 & CRSArrTime.num < 60]

# CRSArrTime > 59
PHL[CRSArrTime.num > 59,CRSArrTime.dt:=chron(times=sub("(.*)(\\d\\d)", "\\1:\\2:00", CRSArrTime))]
PHL[CRSArrTime.num > 59,]

# Convert 'Cause of Delay' to numeric and change NAs to 0s
PHL[is.na(CarrierDelay),CarrierDelay:=0]
PHL[is.na(WeatherDelay),WeatherDelay:=0]
PHL[is.na(NASDelay),NASDelay:=0]
PHL[is.na(SecurityDelay),SecurityDelay:=0]
PHL[is.na(LateAircraftDelay),LateAircraftDelay:=0]

PHL$CarrierDelay.num            <- as.numeric(PHL$CarrierDelay)
PHL$WeatherDelay.num            <- as.numeric(PHL$WeatherDelay)
PHL$NASDelay.num                <- as.numeric(PHL$NASDelay)
PHL$SecurityDelay.num           <- as.numeric(PHL$SecurityDelay)
PHL$LateAircraftDelay.num       <- as.numeric(PHL$LateAircraftDelay)

################
### Check NAs ##
################

summary(PHL)

snip <- cbind(order=seq_len(ncol(PHL)),t(PHL[DepDel15==1][1:3]))
snip

# Check for NAs
sapply(PHL,function(x) sum(is.na(x)))
anyNA(PHL)
# FALSE...No NAs.

nrow(PHL)
# n = 72,498

#####################################
### Export cleaned dataset to .Rds ##
#####################################

# The dataset has been cleaned and is ready for analysis.

# The .rds file preserves data types such as factors and dates eliminating the need
# to redefine data types after loading the file. The .rds file saves a representation 
# of the object and not the name. Finally, the .rds file is compressed.

# Save final dataset to file
saveRDS(PHL, "dataset.rds")



# Resources

# Flight data from Bureau of Transportation Statistics (BTS)
# https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time
# https://www.transtats.bts.gov/TableInfo.asp

# Review of nominal, ordinal and interval scales
# http://www.perceptualedge.com/articles/dmreview/quant_vs_cat_data.pdf

# Reading and Writing Data Files
# http://mgimond.github.io/ES218/Week02b.html
