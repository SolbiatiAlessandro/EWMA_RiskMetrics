# Creates a dataset of max 10 timeSeries objects, getting historical data from yahoofinance.com 
# Require as argument an array of the quote symbols (of the financial instrument)
# Usage:
#	tt <- c("ibm","^gdax",...) #max 10 elems
#	my_data=create_dataset(tt)

create_dataset <- function(name_codes){
	require(tseries)
	require(timeSeries)

	getprice <- function(name_code){
		pp <- get.hist.quote(instrument=name_code,quote="Close")
		pp <- as.timeSeries(pp)
		pp
	}

	ts1 <- getprice(name_codes[1])
	dataset <- cbind(ts1,NA,NA,NA,NA,NA,NA,NA,NA,NA)
	dataset <- dataset[,1:length(name_codes)]
	colnames(dataset)=name_codes
	
	for(i in 2:length(name_codes)){
		dataset[,i][1:length(dataset[,1])]=getprice(name_codes[i][1:length(dataset[,1])])
	}
	
	dataset
}
