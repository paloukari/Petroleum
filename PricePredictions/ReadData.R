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
readAllDataQuery <- "SELECT 
              [SITEID]                    
              ,[AGRID]
              ,SUM([NETTO_EUR]) as [NETTO_EUR]
              ,[DATE]	  
              ,[YEAR]
              ,[IS_HOLIDAY]
              ,[IS_WEEKEND]
              FROM [dbo].[TS_KAT_TAG_PROCESSED] 
              WHERE AGRID=15371 AND [SITEID]=370816
			  GROUP BY [SITEID],[AGRID],[DATE],[YEAR],[IS_HOLIDAY],[IS_WEEKEND]
              ORDER BY [DATE]"
allData <- sqlQuery(channel = conn, query = readAllDataQuery )


close(conn) # don't leak connections !

str(allData)

summary(allData)

head(allData)

