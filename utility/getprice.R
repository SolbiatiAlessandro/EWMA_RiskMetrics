#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project - 26/06/2016 
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#
#	getprice() : return a timeSerie obj with the historical data from YahooFinance
#

getprice <- function(name_code){
	
	require(tseries)
	require(timeSeries)
		
	
	pp <- get.hist.quote(instrument=name_code,quote="Close")
	pp <- as.timeSeries(pp)
	pp
	

}
