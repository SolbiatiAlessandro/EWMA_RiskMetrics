

getprice <- function(name_code){
	
	require(tseries)
	require(timeSeries)
		
	
	pp <- get.hist.quote(instrument=name_code,quote="Close")
	pp <- as.timeSeries(pp)
	pp
	

}
