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
readSiteTransationsQuery <- "
  SELECT [TRANID]      
      ,[AGR_BEZ]
	,[SITEID]
  FROM [dbo].[TS_UMS_PROCESSED_SMALL]
  WHERE [SITEID] IN (434564, 434314, 434537, 434628)
  ORDER BY [AGR_BEZ],[TRANID]"
				
siteTransations <- sqlQuery(channel = conn, query = readSiteTransationsQuery )

#siteTransations<-lapply(siteTransations, as.character)

close(conn) # don't leak connections !

str(siteTransations)

summary(siteTransations)

#head(siteTransations)

