##
##
## SCRIPT FOR IMPORTING, SUBSETTING AND ANALYSING DATA RECORDED USING RFID LOGGERS
## from Kelly E (2019) Automated RFID Logger: Inexpensive and versatile method to remotely monitor animals for ecological research
##
##


##
## IMPORTING DATA
##

## Loading RFID data from .DAT files
setwd("../data/RFIDdata")
files <- list.files(pattern=".DAT")
logger.data <- NULL
for (f in files) {
  dat <- read.table(f ,header=FALSE, sep= " ")
  dat<-cbind(dat,as.character(f))
  logger.data <- rbind(logger.data, dat)
}

## cleaning data
colnames(logger.data) <- c("rawdate","M","microchip", "logger") #name cols
logger.data <- subset(logger.data, select = -M ) #remove extra col
logger.data$logger<-substring(logger.data$logger, 1, 1) #create logger label 
date.time<-as.POSIXct(paste(substring(logger.data$rawdate, 1, 6), substring(logger.data$rawdate, 7)), format="%y%m%d %H%M%S") #reformat dates
date<-as.Date(substring(logger.data$rawdate, 1, 6), "%y%m%d") #reformat dates
date<-as.POSIXct(date, origin="1970-01-01") #reformat dates
logger.data<-cbind(logger.data, date.time, date) #good to save all RFID data as a csv file

##
## SUBSETTING AND EXTRACTING DATA
##


## Preparing data 

# funtion for selecting rows from RFID data based on microchip number, logger label and date range
row.selection<-function(data, micro, logger,start.date,end.date){
  data.subset<- data[data[,"microchip"]==micro & data[,"logger"]==logger,]
  date.subset<-data.subset[data.subset[,"date"]>=start.date & data.subset[,"date"]<=end.date,]
  date.subset
}

# load and clean details on the experiment
exp<-read.csv("../experiment.csv",na.strings=c("","NA"))# load spreadsheet with details on the experiment
exp[,"date.1"]<-as.POSIXct(exp[,"date.1"], origin="1970-01-01") # convert dates to right format for subsetting
enddate.1<-exp[,"date.1"]+24*60*60 # add one day to create enddate (experiment ran for 1 day)
exp<-cbind(exp, enddate.1) # combine

##
## How many movements in/out of the nestbox?
##
movements<-vector(length=nrow(exp))
permin.movements<-vector(length=nrow(exp))
for (i in 1:nrow(exp)){
  output<-row.selection(logger.data, exp[i,"microchip"], as.character(exp[i,"logger.1"]),exp[i,"date.1"],exp[i,"enddate.1"])  
  movements[i]<-nrow(output) # extract number of records
  permin.movements[i]<-length(unique(trunc(output[,"date.time"], units="mins"))) #records per minute
}

##  
## How long until each quoll first emerged from the nestbox? 
##

# extract experiment start times 
logon<-vector(length=nrow(exp))
for (i in 1:nrow(exp)){
  output<-row.selection(logger.data, 956000005342356, as.character(exp[i,"logger.1"]),exp[i,"date.1"],exp[i,"enddate.1"]) # we scanned microchip number 956000005342356 at beginning of experiment to record start time
  x<-as.POSIXct(head(output[,"date.time"], 1), origin="1970-01-01")
  logon[i]<-x[1]
}
logon<- as.POSIXct(logon, origin="1970-01-01")

# extract quoll emergance times
emerge<-vector(length=nrow(exp))
for (i in 1:nrow(exp)){
  output<-row.selection(logger.data, exp[i,"microchip"], as.character(exp[i,"logger.1"]),exp[i,"date.1"],exp[i,"enddate.1"])  
  x<-as.POSIXct(head(output[,"date.time"], 1), format="%y%m%d %H%M%S") # extract first record 
  emerge[i]<-x[1]
}
emerge<- as.POSIXct(emerge, origin="1970-01-01") # format with correct date/time conventions 

# calculate time until emergance (difference between start and emergance times)
second.diff<-difftime(emerge, logon) # difference in seconds
min.diff<-second.diff/60 # difference in minutes

# highlight those that didn't emerge
for(i in 1:length(min.diff)) {
  if(is.na(min.diff[i]) == TRUE){
    min.diff[i] <- "did not emerge"
  }
}


## Combine results
rfid.results<-cbind(exp, movements, permin.movements, logon, emerge, min.diff)

## 
## ANALYSING DATA
##

# number of movements
boxplot(as.numeric(movements)~as.factor(sex), data=rfid.results, ylab="Number of movements in/out of nestbox")
boxplot(as.numeric(movements)~as.factor(sex), data=rfid.results, outline=F, ylab="Number of movements in/out of nestbox")

summary(as.numeric(rfid.results$movements))
sd(as.numeric(rfid.results$movements))
sd(as.numeric(rfid.results$movements))/sqrt(40)

boxplot(as.numeric(permin.movements)~as.factor(sex), data=rfid.results, ylab="Number of minutes spent moving in/out of nestbox")

summary(as.numeric(rfid.results$permin.movements))
sd(as.numeric(rfid.results$permin.movements))
sd(as.numeric(rfid.results$permin.movements))/sqrt(40)


# emergance time
onlyemerge<-rfid.results[!rfid.results$min.diff=="did not emerge",]
boxplot(as.numeric(min.diff)~as.factor(sex), data=rfid.results, ylab="Minutes until emergance")

summary(as.numeric(onlyemerge$min.diff))
sd(as.numeric(onlyemerge$min.diff))
sd(as.numeric(onlyemerge$min.diff))/sqrt(40)

anova(lm(as.numeric(min.diff)~as.factor(sex), data=rfid.results))
anova(lm(as.numeric(movements)~as.factor(sex), data=rfid.results))



