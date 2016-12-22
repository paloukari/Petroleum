#2.1
# data input
#data <- maml.mapInputPort(1) # class: data.frame
#input <- maml.mapInputPort(2) # class: data.frame
#attach(input)


library(forecast)
library(plyr)


data <- data.good
input$makeParallel <- FALSE

# Date format clean-up
data$DATE <- as.POSIXct(as.numeric(as.POSIXct(data$DATE, format = timeformat, tz = "UTC", origin = "1970-01-01"), tz = "UTC"), tz = "UTC", origin = "1970-01-01")

# Helper functions extracting date-related information
weeknum <- function(date){ date <- as.Date(date, format=timeformat); as.numeric(format(date, "%U"))}
year <- function(date){ date <- as.Date(date, format=timeformat); as.numeric(format(date, "%Y"))}
date.info <- function(df){ date <- df$DATE[1]; c(year(date), weeknum(date))}

# Forecasting set-up
if (!("horizon" %in% colnames(input)) & "test.length" %in% colnames(input)) {print(TRUE); input$horizon <- input$test.length}



# Forecasting Function
stlets.single.id <- function(datum){
  method.name <- "STL_ETS"
  
  # Train and test split
  datum.length <- nrow(datum)
  train.length <- datum.length - input$horizon
  train <- datum[1:train.length, ]
  test <- datum[(train.length+1):datum.length, ]
  
  # Missing data: replace na with average
  train$NETTO_EUR[is.na(train$NETTO_EUR)] <- mean(train$NETTO_EUR, na.rm = TRUE)
  
  # Build forecasting models
  train.ts <- ts(train$NETTO_EUR, frequency = input$seasonality, start = date.info(train))
  train.stl <- stl(train.ts, s.window="periodic")
  train.model <- forecast(train.stl, h = input$horizon, method = 'ets', ic = 'bic', opt.crit='mae')
  
  forecast.NETTO_EUR <- train.model$mean
  forecast.lo95 <- train.model$lower[,1]
  forecast.hi95 <- train.model$upper[,1]
  
  output <- data.frame(DATE = test$DATE, cbind(forecast.NETTO_EUR, forecast.lo95, forecast.hi95))
  colnames(output)[-1] <- paste(c("forecast", "lo95", "hi95"), method.name, sep = ".") 
  
  return(output)
}



#maml.mapOutputPort("output");

#2.2

# Forecasting Function
snaive.single.id <- function(datum){
  method.name <- "snaive"
  
  # Train and test split
  datum.length <- nrow(datum)
  train.length <- datum.length - input$horizon
  train <- datum[1:train.length, ]
  test <- datum[(train.length+1):datum.length, ]
  
  # Missing data: replace na with average
  train$NETTO_EUR[is.na(train$NETTO_EUR)] <- mean(train$NETTO_EUR, na.rm = TRUE)
  
  # Build forecasting models
  train.ts <- ts(train$NETTO_EUR, frequency = input$seasonality, start = date.info(train))
  train.model <- snaive(train.ts, h = input$horizon)
  
  forecast.NETTO_EUR <- train.model$mean
  forecast.lo95 <- train.model$lower[,1]
  forecast.hi95 <- train.model$upper[,1]
  
  output <- data.frame(DATE = test$DATE, cbind(forecast.NETTO_EUR, forecast.lo95, forecast.hi95))
  colnames(output)[-1] <- paste(c("forecast", "lo95", "hi95"), method.name, sep = ".") 
  
  return(output)
}



arima.single.id <- function(datum){
  method.name <- "STL_ARIMA"
  
  # Train and test split
  datum.length <- nrow(datum)
  train.length <- datum.length - input$horizon
  train <- datum[1:train.length, ]
  test <- datum[(train.length+1):datum.length, ]
  
  # Missing data: replace na with average
  train$NETTO_EUR[is.na(train$NETTO_EUR)] <- mean(train$NETTO_EUR, na.rm = TRUE)
  
  # Build forecasting models
  train.ts <- ts(train$NETTO_EUR, frequency = input$seasonality, start = date.info(train))
  train.model <- stlf(train.ts, h = input$horizon, method = "arima", s.window = "periodic")
  
  forecast.NETTO_EUR <- train.model$mean
  forecast.lo95 <- train.model$lower[,1]
  forecast.hi95 <- train.model$upper[,1]
  
  output <- data.frame(DATE = test$DATE, cbind(forecast.NETTO_EUR, forecast.lo95, forecast.hi95))
  colnames(output)[-1] <- paste(c("forecast", "lo95", "hi95"), method.name, sep = ".") 
  
  return(output)
}
if(input$makeParallel){
	
  library(parallel)
  library(foreach)
  #library(doParallel)  
  #parallelize

  nodes <- detectCores()
  cl <- makeCluster(nodes/2)
  #registerDoParallel()  

  output1 <- ddply(data, .(SITEID, AGRID), arima.single.id, .parallel = TRUE, .paropts = list(.packages = "forecast",.export=c("data", "input", "date.info", "weeknum", "year")))
  output2 <- ddply(data, .(SITEID, AGRID), snaive.single.id, .parallel = TRUE, .paropts = list(.packages = "forecast",.export=c("data", "input", "date.info", "weeknum", "year")))
  output3 <- ddply(data, .(SITEID, AGRID), stlets.single.id, .parallel = TRUE, .paropts = list(.packages = "forecast",.export=c("data", "input", "date.info", "weeknum", "year")))
  
  stopCluster(cl) 
}
if(!input$makeParallel){
  output1 <- ddply(data, .(SITEID, AGRID), arima.single.id, .progress = "win")
  output2 <- ddply(data, .(SITEID, AGRID), snaive.single.id, .progress = "win")
  output3 <- ddply(data, .(SITEID, AGRID), stlets.single.id, .progress = "win")  
}
output4 <- join(output1, output2, by = c("SITEID", "AGRID", "DATE"), type = "inner")
output <- join(output3, output4, by = c("SITEID", "AGRID", "DATE"), type = "inner")


#maml.mapOutputPort("output");