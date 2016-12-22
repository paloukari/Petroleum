
library("arules")
library("arulesViz")

## for convenience [ceeboo 2007]
.rm.duplicates <- function(x) {
    n <- sapply(x, length, USE.NAMES = FALSE)
    x <- lapply(x, unique)
    n <- n - sapply(x, length, USE.NAMES = FALSE)
    if (any(n)) {
        n <- table(items = n)[-1]
        cat("distribution of transactions with duplicates:\n")
        print(n)
    }
    x
}


# Input transactions in Single mode
#dataset1 <- maml.mapInputPort(1) # class: data.frame
#dataset2 <- maml.mapInputPort(2) # class: data.frame



#First column is transactionID
#Second column is itemID
cols<-c(1,2)
#support and confidence as parameters
#sup<-dataset2[1,1]
#conf<-dataset2[1,1]
sup <- 0.01
conf <- 0.01
sortProducts <- function(x){
	x[order(x, na.last = NA)]
}

siteIDs <- unique(siteTransations$SITEID)
res <- NA
for(i in 1:length(siteIDs))
{
	print(siteIDs[i])
	
	subGroup <- siteTransations[siteTransations$SITEID == siteIDs[i], c("TRANID", "AGR_BEZ")]
	mylist<-as.list( subGroup, sorted = TRUE)
	entries <- split(mylist[[cols[2]]], mylist[[cols[1]]])
		entries <- .rm.duplicates(entries)
	entries<-lapply(entries, sortProducts)
	subGroup<-as(entries, "transactions")
	#Set Support and Confidence as appropriate
	rules <- apriori(subGroup,parameter = list(sup = sup, conf = conf,target="rules"));		
	inspect(rules)
	results <-as(rules, "data.frame");
	results$SITEID <- siteIDs[i]
	if(is.na(res)){
		res <- results
    }else{
		res <- rbind(res, results)
	}
	str(results)
}


png(file="BasketAnalysisDistr.png",width=1024,height=768)
itemFrequencyPlot(mytransactions,topN=20,type="absolute")
dev.off()

# Select data.frame to be sent to the output Dataset port
#maml.mapOutputPort("df");