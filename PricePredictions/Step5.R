#data <- maml.mapInputPort(1) # class: data.frame
#input <- maml.mapInputPort(2) # class: data.frame
#attach(input)
data <- results
# Date format clean-up
data$DATE <- as.POSIXct(as.numeric(as.POSIXct(data$DATE, format = DATEformat, tz = "UTC", origin = "1970-01-01"), tz = "UTC"), tz = "UTC", origin = "1970-01-01")

library(forecast)
library(plyr)
library(car)

print("Note that MASE is less than one if it arises from a better forecast than the average naive forecast.")

# Helper function
extract.col <- function(pattern, df){
  col.indices <- grep(pattern, colnames(df), ignore.case = TRUE)
  return(df[, col.indices, drop = FALSE])
}

# Metric Functions
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

get.metrics <- function(true, forecast){
  forecast.metrics <- as.data.frame(accuracy(forecast, true))
  return(data.frame(forecast.metrics[, colnames(forecast.metrics) !="ME"], MASE = mase(true, forecast)))
}

get.metrics.single.id <- function(data){
  # Extract forecast values
  data.forecast <- extract.col("forecast [a_z]*", df = data)
  
  # Split true data and forecast values
  is.test <- !is.na(data.forecast[,1])
  
  test.true <- data$NETTO_EUR[is.test]
  test.forecast <- data.forecast[is.test, ]
  test.DATE <- data$DATE[is.test]
  
  test.nonna <- complete.cases(test.true)
  test.true <- test.true[test.nonna]
  test.forecast <- test.forecast[test.nonna,]
  #test.DATE <- test.DATE[test.nonna]
  
  # Calculate error metrics
  output <- t(sapply(test.forecast, get.metrics, true = test.true))
  
  methods <- sub("[a-z]+ ", "", rownames(output), ignore.case = TRUE)
  output <- apply(output, c(1,2),as.numeric)
  output <- data.frame(test.length.nonna = sum(test.nonna), Method = methods, output)
  rownames(output) <- NULL
  return(output)
}

metrics.all.ids <- ddply(data, .(SITEID, AGRID), get.metrics.single.id)

get.mean.metric.summary <- function(mean.metric, N){
  return(weighted.mean(mean.metric, N))
}

get.rmse.summary <- function(rmse.metric, N){
  return(c("RMSE" = sqrt(weighted.mean(rmse.metric^2, N))))
}

get.metrics.summary <- function(metrics.all.ids){
  summary.mean <- apply(extract.col("^M[A-Z]+E", metrics.all.ids), MARGIN = 2, 
                        FUN = get.mean.metric.summary, N = metrics.all.ids$test.length.nonna)
  
  summary.rmse <- get.rmse.summary(metrics.all.ids$RMSE, metrics.all.ids$test.length.nonna)
  return(c(summary.mean, summary.rmse))
}

metrics.summary <- ddply(metrics.all.ids, .(Method), get.metrics.summary)

metric.names <- grep("^M[A-Z]+E|RMSE", colnames(metrics.all.ids), ignore.case = TRUE, value = TRUE)

plot.metric <- function(metric.name){
  png(filename = paste(metric.name,".png", sep = ""), width = 2040, height = 720)
  par(oma = c(1, 1.2, 3, 1), mar = c(6, 6, 6, 2), mfrow = c(1, 2), cex.lab=1.1, cex.axis=1.1, cex.main=1.3, cex.sub=1.3)
  command <- paste("Boxplot(", metric.name, "~Method, data = metrics.all.ids, labels = paste(SITEID, AGRID, sep = \",\"), id.method = \"y\", id.n = Inf, main = \"", metric.name, "\")", sep = "")
  eval(parse(text = command))
  mtext("The (SITEID, AGRID) of each outlier is provided beside the point", side = 3, line = 0.5, cex = 1.3)
  command <- paste("Boxplot(", metric.name, "~Method, data = metrics.all.ids, labels = paste(SITEID, AGRID, sep = \",\"), id.method = \"none\", id.n = 0, main = \"", metric.name, " without outliers\", outline = FALSE)", sep = "")
  eval(parse(text = command))
  mtext("The outliers are removed to better present the scale of boxes", side = 3, line = 0.5, cex = 1.3)
  graph.title = paste("Model Comparison under Metric", metric.name)
  title(graph.title, outer = TRUE)
  box("inner", lty = 3)
  dev.off()
}

g <- lapply(metric.names, FUN = plot.metric)

output <- metrics.summary

#maml.mapOutputPort("output");