# Map 1-based optional input ports to variables
#data <- maml.mapInputPort(1) # class: data.frame
#input <- maml.mapInputPort(2)
#attach(input)

library(zoo)
library(timeDate)

# Date format clean-up
data$DATE <- as.POSIXct(as.numeric(as.POSIXct(data$DATE, format = timeformat, tz = "UTC", origin = "1970-01-01"), tz = "UTC"), tz = "UTC", origin = "1970-01-01")

## ----------------------------------------------------------- User-defined Features --------------------------------------------- ##

# Date Features
data$year <- as.numeric(format(data$DATE, "%Y"))
data$month <- as.numeric(format(data$DATE, "%m"))
data$weekofmonth <- ceiling(as.numeric(format(data$DATE, "%d"))/7)

obsdayofweek <- as.numeric(format(data$DATE[1], "%u"))
adjStartofWeek <- 60*60*24*(7-obsdayofweek)
data$weekofyear <- as.numeric(format(data$DATE+adjStartofWeek, "%U"))
data$dayofmonth <- as.numeric(format(data$DATE, "%d"))
data$dayofweek <- as.numeric(format(data$DATE, "%u"))
#data$hourofday <- as.numeric(format(data$DATE, "%H"))

# DATE Features
#data$time_cat <- "Night"
#data$time_cat[data$hourofday>=5 & data$hourofday <=11] <- "Morning"
#data$time_cat[data$hourofday>=12 & data$hourofday <=15] <- "Afternoon"
#data$time_cat[data$hourofday>=16 & data$hourofday <=20] <- "Evening"

# Season Features - American east coast
data$season <-"Winter"
data$season[data$month>=3 & data$month <=5] <- "Spring"
data$season[data$month>=6 & data$month <=8] <- "Summer"
data$season[data$month>=9 & data$month <=11] <- "Fall"

# Weekday-and-weekend Features
data$weekend <-FALSE
data$weekend[data$dayofweek>=6 & data$dayofweek<=7] <- TRUE

# Holiday Features
# These codes only apply to weekly data
CyberMonday <- function(years){as.timeDate(as.Date(USThanksgivingDay(years))+4)}

years = unique(data$year)

adjHolidays <- function(holidays){
  holidays <- as.Date(holidays)
  hlddayofweek <- as.numeric(format(holidays, "%u"))
  return(as.timeDate(holidays + obsdayofweek - hlddayofweek  +7*(hlddayofweek > obsdayofweek)))
}

data.DATE <- as.timeDate(data$DATE)

#data$USNewYearsDay <- isHoliday(data.DATE, holidays = adjHolidays(USNewYearsDay(years)), wday = 0:6)
#data$USLaborDay <- isHoliday(data.DATE, holidays = adjHolidays(USLaborDay(years)), wday = 0:6)
#data$USThanksgivingDay <- isHoliday(data.DATE, holidays = adjHolidays(USThanksgivingDay(years)), wday = 0:6)
#data$CyberMonday <- isHoliday(data.DATE, holidays = adjHolidays(CyberMonday(years)), wday = 0:6)
#data$ChristmasDay <-  isHoliday(data.DATE, holidays = adjHolidays(ChristmasDay(years)), wday=0:6)

# Another way of adding holiday features.
# Applies to daily/hourly data
# data$holiday <- FALSE
# holidays <- c("2013-08-15", "2013-11-30", "2013-12-01", "2014-04-20", "2014-04-21", 
#              "2014-05-01", "2014-06-08", "2014-06-09", "2014-08-15", "2014-11-30")
# for (i in 1:length(holidays)){
#   data$holiday[as.Date(data$DATE) == holidays[i]] <- TRUE
# }

# Fourier Features
num.ts <- nrow(unique(data[, c("SITEID", "AGRID")]))
ts.length <- nrow(data)/num.ts
t <- (index(data) - 1) %% ts.length %% seasonality 

for (s in 1:4){
  data[[paste("FreqCos", toString(s), sep="")]] = cos(t*2*pi*s/seasonality)
  data[[paste("FreqSin", toString(s), sep="")]] = sin(t*2*pi*s/seasonality)
}

#3.8
## --------------------------------------------------------------------------------------------------------------------------------- ##

# Select data.frame to be sent to the output Dataset port
#maml.mapOutputPort("data");


#data <- maml.mapInputPort(1) # class: data.frame
#input <- maml.mapInputPort(2)
#attach(input)

## ------- User-Defined Parameters ------ ##
lags <- 1:26
ratio <- 1
## ----------------------------------------- ##

#horizon <- test.length 

library(plyr)

shift<-function(lag, x){c(rep(NA, lag), head(x,-lag))}
shift<- Vectorize(shift, vectorize.args = "lag")

addlags_oneh <- function(h, lags, df, var){
  res <- shift(lags+h-1, x=df[,var])
  colnames(res) <- paste("lag", lags, sep = "")
  return(cbind(df,res))
}

addlags <- function(df, var, lags, maxh){
  horizons <- 1:maxh
  res <- adply(horizons, .margin = 1, .fun = addlags_oneh, lags = lags, df = df, var = var)
  res <- rename(res, replace = c("X1" = "horizon"))
  res <- res[complete.cases(res), ]
  return(res)
}

train.addlags <- function(df, var, lags, maxh){
  data.length <- nrow(df)
  train.length <- data.length - test.length
  train <- df[1:train.length, , drop = FALSE]
  
  res <- addlags(train, var, lags, maxh)
  return(res)
}

output <- ddply(data, .variables = .(SITEID, AGRID), .fun = train.addlags, var = "NETTO_EUR", lags = lags, maxh = horizon)

#print(dim(output))
#print(ddply(output, .variables = .(SITEID, AGRID), .fun = nrow))

if(ratio < 1){
  downsample <- function(data, ratio){ data[sample(nrow(data), size = ratio*nrow(data)),]}
  output <- ddply(output, .variables = .(SITEID, AGRID), .fun = downsample, ratio = ratio)
}

#print(dim(output))
#print(ddply(output, .variables = .(SITEID, AGRID), .fun = nrow))

# Select data.frame to be sent to the output Dataset port
#maml.mapOutputPort("output");

#3.9

#data <- maml.mapInputPort(1) # class: data.frame
#input <- maml.mapInputPort(2)
#attach(input)

## ------- User-Defined Parameters ------ ##
lags <- 1:26
ratio <- 1
## ----------------------------------------- ##

horizon <- test.length 

library(plyr)

test.addlags <- function(df, var, lags){
  data.length <- nrow(df)
  train.length <- data.length - test.length
  test  <- df[(train.length+1):data.length, , drop = FALSE]
  train  <- df[1:train.length, , drop = FALSE]
  
  # Missing data: replace NA with average
  train$NETTO_EUR[is.na(train$NETTO_EUR)] <- mean(train$NETTO_EUR, na.rm = TRUE)
  
  # Create lag features
  test$horizon <- as.factor(1:test.length)
  
  test.lags <- df[train.length - lags + 1, var]
  test.lags <- matrix(rep(test.lags, test.length), nrow = test.length, byrow = TRUE)
  colnames(test.lags) <- paste("lag", lags, sep = "")
  res <- cbind(test, test.lags)
  
  return(res)
}

output <- ddply(data, .variables = .(SITEID, AGRID), .fun = test.addlags, var = "NETTO_EUR", lags = lags)

#print(dim(output))
#print(ddply(output, .variables = .(SITEID, AGRID), .fun = nrow))

if(ratio < 1){
  downsample <- function(data, ratio){ data[sample(nrow(data), size = ratio*nrow(data)),]}
  output <- ddply(output, .variables = .(SITEID, AGRID), .fun = downsample, ratio = ratio)
}

#print(dim(output))
#print(ddply(output, .variables = .(SITEID, AGRID), .fun = nrow))

# Select data.frame to be sent to the output Dataset port
#maml.mapOutputPort("output");