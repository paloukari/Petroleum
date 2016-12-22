#1.1
## ------- User-Defined Parameters ------ ##
test.length <- 35
seasonality <- 7

horizon <- 35
timeformat <- "%Y-%m-%d"
makeParallel <- FALSE

min.length <- 2*seasonality
#min.length <- seasonality
value.threshold <- 20

## ----------------------------------------- ##
observation.freq <- "day"

input<- data.frame(test.length = test.length, seasonality = seasonality, 
                       observation.freq = observation.freq, timeformat = timeformat,
                   makeParallel = makeParallel, horizon=horizon,
                   stringsAsFactors = FALSE)

# Select data.frame to be sent to the output Dataset port
#maml.mapOutputPort("input");

#1.2
# data input
#data <- maml.mapInputPort(1) # class: data.frame
#input <- maml.mapInputPort(2) # class: data.frame
data <- allData
attach(input)

# Date format clean-up
data$DATE <- as.POSIXct(as.numeric(as.POSIXct(data$DATE, format = timeformat, tz = "UTC", origin = "1970-01-01"), tz = "UTC"), tz = "UTC", origin = "1970-01-01")

library(plyr)

# apply business rules
businessrule <- function(data){  
  tsvalues <- data$NETTO_EUR
  
  # Select Eligible Time Series:
  # Rule 1: if a time series has no more than <min.length> non-NA values, discard
  if (sum(!is.na(tsvalues)) < min.length) return(c(judge = 1))
  
  # Rule 2: if a time series has any sales quantity <= value.threshold , discard
  #if (length(tsvalues[tsvalues > value.threshold]) != length(tsvalues)) return(c(judge = 2))
  
  return(c(judge = 0))
}

judge.all <- ddply(data, .(SITEID, AGRID), businessrule)
judge.good <- judge.all[judge.all$judge == 0, c("SITEID", "AGRID")]
data.good <- join(data, judge.good, by = c("SITEID", "AGRID"), type = "inner")

#maml.mapOutputPort("data.good");

#1.3

# Map 1-based optional input ports to variables
#data <- maml.mapInputPort(1) # class: data.frame
#input <- maml.mapInputPort(2)
#attach(input)

#library(plyr)

data$DATE <- as.Date(data$DATE, format=timeformat)

min.DATE <- min(data$DATE)
max.DATE <- max(data$DATE)

unique.DATE <- seq(from = min.DATE, to = max.DATE, by = observation.freq)

# For every (SITEID, AGRID) pair, create (SITEID, AGRID, DATE) combination 
unique.SITEAGRID <- unique(data[, c("SITEID", "AGRID")])
comb.SITEID <- rep(unique.SITEAGRID$SITEID, each = length(unique.DATE))
comb.AGRID <- rep(unique.SITEAGRID$AGRID, each = length(unique.DATE))
comb.DATE <- rep(unique.DATE, times = dim(unique.SITEAGRID)[1])
comb <- data.frame(SITEID = comb.SITEID, AGRID = comb.AGRID, DATE = comb.DATE)

# Join the combination with original data
data <- join(comb, data, by = c("SITEID", "AGRID", "DATE"), type = "left")

# Select data.frame to be sent to the output Dataset port
#maml.mapOutputPort("data");

#1.4

# data input
#data <- maml.mapInputPort(1) # class: data.frame
#input <- maml.mapInputPort(2) # class: data.frame
#attach(input)

# Date format clean-up
data$DATE <- as.POSIXct(as.numeric(as.POSIXct(data$DATE, format = timeformat, tz = "UTC", origin = "1970-01-01"), tz = "UTC"), tz = "UTC", origin = "1970-01-01")

#library(plyr)

# apply business rules
businessrule2 <- function(data){
  # Train and test split
  data.length <- dim(data)[1]
  train.length <- data.length - test.length
  
  tsvalues <- data$NETTO_EUR
  
  # Select Eligible Time Series based on training and testing principals:
  # Rule 3: if the last 6 values in trainning set are all NA, discard
  if (sum(is.na(tsvalues[(train.length - 5) : train.length])) == 6) return(c(judge = 3))
  
  # Rule 4: if test data has more than a half NA, discard
  if (test.length > 0 && sum(is.na(tsvalues[(train.length+1):data.length])) > test.length / 2) return(c(judge = 4))
  
  return(c(judge = 0))
}

judge.all <- ddply(data, .(SITEID, AGRID), businessrule2, .progress = "win")
judge.good <- judge.all[judge.all$judge == 0, c("SITEID", "AGRID")]
data.good <- join(data, judge.good, by = c("SITEID", "AGRID"), type = "inner")

#maml.mapOutputPort("data.good");