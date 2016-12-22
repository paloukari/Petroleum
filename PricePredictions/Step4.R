#data <- maml.mapInputPort(1) # class: data.frame
#input <- maml.mapInputPort(2) # class: data.frame
#attach(input)
data <- results
# Date format clean-up
data$DATE <- as.POSIXct(as.numeric(as.POSIXct(data$DATE, format = timeformat, tz = "UTC", origin = "1970-01-01"), tz = "UTC"), tz = "UTC", origin = "1970-01-01")

# Helper function
extract.col <- function(pattern, df = data){
  col.indices <- grep(pattern, colnames(df), ignore.case = TRUE)
  return(df[, col.indices, drop = FALSE])
}

# Metric Functions
library(forecast)

mase <- function(true, forecast){
  error = 0;
  if (length(true) != length(forecast)) {
    return (NA);
  } else if (length(true) == 0 || length(forecast) == 0) {
    return (NA);
  }
  else {
    denom = (sum(abs(true[2:length(true)] - true[1:(length(true) - 1)])))/(length(true) - 1)
    error = sum((abs(true-forecast)) / denom)/length(true);
  }
  return (error);
}

metrics <- function(true, forecast){
  forecast.metrics <- as.data.frame(accuracy(forecast, true))
  return(data.frame(forecast.metrics[, colnames(forecast.metrics) !="ME"], MASE = mase(true, forecast)))
}

# Extract forecast values
data.forecast <- extract.col("forecast [a_z]*")

# Split true data and forecast values

is.test <- (data.forecast[,1])!=0
test.true <- data$NETTO_EUR[is.test]
test.forecast <- data.forecast[is.test, ]
test.DATE <- data$DATE[is.test]

test.nonna <- complete.cases(test.true)
test.true <- test.true[test.nonna]
test.forecast <- test.forecast[test.nonna,]
test.DATE <- test.DATE[test.nonna]

# Calculate error metrics
output <- t(sapply(test.forecast, metrics, true = test.true))

methods <- sub("[a-z]+ ", "", rownames(output), ignore.case = TRUE)
output <- apply(output, c(1,2),as.numeric)
output <- data.frame(Method = methods, output)
rownames(output) <- NULL

# Plot
DATE <- data$DATE
true <- as.numeric(data$NETTO_EUR)
value.data <- unlist(data[, !(names(data) %in% c("DATE", "SITEID", "AGRID"))])
min.data <- min(value.data, na.rm = TRUE)
max.data <- max(value.data, na.rm = TRUE)
res <- as.data.frame(value.data)
graph.ts <- function(method){
  forecast.NETTO_EUR <- data[, paste("forecast ", method, sep = "")]
  
  have.ci <- tryCatch(
{
  lo95 <- data[, paste("lo95 ", method, sep = "")]
  hi95 <- data[, paste("hi95 ", method, sep = "")]
  have.ci <- TRUE
},
error = function(e){
  have.ci <- FALSE
}
  )
graph.title <- paste("Forecast by", method)

#plot(DATE, true, type="l",col="blue",xlab="Time",ylab="Data",lwd=2, bty="l", main = "Time Series Plot", ylim = c(min(0,min.data*0.95), max.data * 1.05))
plot(DATE, true, type="l",col="blue",xlab="Time",ylab="Data",lwd=2, bty="l", main = "Time Series Plot", ylim = c(min(0,-40434.68*0.95), 166594.8 * 1.05))

grid(col = "gray")
lines(DATE, forecast.NETTO_EUR, col = "red", lwd = 2)

# plot confidence interval
if (have.ci){
  ci.color <- adjustcolor("gray",alpha.f=0.5)
  lines(DATE, lo95, col = ci.color, lwd = 2)
  lines(DATE, hi95, col = ci.color, lwd = 2)
  polygon(c(DATE, rev(DATE)), c(hi95, rev(lo95)), col = ci.color, border = NA)
  
  # add legend
  legend("top",legend = c("True Data", "Forecast", "95% Confidence Interval"),
         bty=c("n","n"), lty=c(1, 1, 1), lwd = c(2, 2, 10), horiz = TRUE,
         col=c("blue","red", ci.color), cex = 1.5)
}
else{
  # add legend
  legend("top",legend = c("True Data", "Forecast"),
         bty=c("n","n"), lty=c(1, 1), lwd = c(2, 2), horiz = TRUE,
         col=c("blue","red"), cex = 1.5)
  return(NULL)
}
}


error.forecast <- apply(test.forecast, 2, "-", test.true)
colnames(error.forecast) <- sub("forecast", "error", colnames(error.forecast))
max.abs.error <- max(abs(error.forecast), na.rm = TRUE)

graph.error <- function(method){
  error <- error.forecast[, paste("error ", method, sep = "")]
  error.mean <- mean(error)
  error.sd <- sd(error)
  
  # error vs DATE
  plot(test.DATE, error, type = "h", bty = "l", xlab = "Time", ylab = "Prediction Error", 
       main = "Prediction Error VS Time",  ylim = c(-max.abs.error, max.abs.error))
  abline(0,0)
  abline(1.96*error.sd, 0, lty = 2, col = "blue")
  abline(-1.96*error.sd, 0, lty = 2, col = "blue")
  
  # error histogram
  error.hist <- hist(error, density = 20, bty = "l", xlab = "Prediction Error", 
                     main = "Histogram of Prediction Error", xlim = c(-max.abs.error*1.1, max.abs.error*1.1))
  x <- (-max.abs.error*1.1) : (max.abs.error*1.1)
  multiplier <- (error.hist$counts / error.hist$density)[1]
  lines(x, dnorm(x, 0, error.sd)*multiplier, col = "red")
  
  # ACF and PACF
  acf(error, main = "", bty = "l", ylim = c(-1, 1))
  title("Auto-Correlation Function of Errors", line = 1)
  pacf(error, , main = "", bty = "l", ylim = c(-1, 1), xlim = c(1, 20))
  title("Partial Auto-Correlation Function of Errors", line = 1)
  
  return(NULL)
}

SITEID <- data$SITEID[1]
AGRID <- data$AGRID[1]

graph <- function(method){
  png(filename = paste(method,".png", sep = ""), width = 1080, height = 1620)
  layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE), heights = c(1.1, 1, 1))
  par(oma = c(1, 1.2, 3, 1), mar = c(3.5, 3.5, 2, 1), mgp = c(1.8, 0.5, 0), 
      cex.lab=1.4, cex.axis=1.3, cex.main=2, cex.sub=1.5)
  graph.ts(method)
  graph.error(method)
  graph.title <- paste("Model Goodness of", method, "for SITEID =", SITEID, "and AGRID =", AGRID)
  title(graph.title, outer = TRUE)
  box("inner", lty = 3)
  dev.off()
}

g <- lapply(methods, FUN = graph)

#maml.mapOutputPort("output");