

library(RODBC)

myServer <- "localhost"
myUser <- "user"
myPassword <- "password"
myDatabase <- "petroleum"
myDriver <- "{SQL Server Native Client 11.0}"

connectionString <- paste0(
  "Driver=", myDriver, 
  ";Server=", myServer, 
  ";Database=", myDatabase, 
  ";Uid=", myUser, 
  ";Pwd=", myPassword)


conn <- odbcDriverConnect(connectionString)

odbcGetInfo(conn)
readResultsQuery <- "SELECT [SITEID]
      ,[AGRID]
      ,[DATE]
      ,[NETTO_EUR]
      ,[YEAR]
      ,[IS_HOLIDAY]
      ,[IS_WEEKEND]
      ,[forecast STL_ETS]
      ,[lo95 STL_ETS]
      ,[hi95 STL_ETS]
      ,[forecast snaive]
      ,[lo95 snaive]
      ,[hi95 snaive]
      ,[forecast STL_ARIMA]
      ,[lo95 STL_ARIMA]
      ,[hi95 STL_ARIMA]
      ,[forecast BstDecTree]
      ,[forecast DecFore]
      ,[lo95 DecFore]
      ,[hi95 DecFore]
      ,[lo95 FastFore]
      ,[forecast FastFore]
      ,[hi95 FastFore]
  FROM [Petrom].[dbo].[Price Predictions Visualizations]
              WHERE AGRID=15371 AND [SITEID]=370816			  
              ORDER BY [DATE]"
results <- sqlQuery(channel = conn, query = readResultsQuery )


close(conn) # don't leak connections !

str(results)

summary(results)

head(results)

