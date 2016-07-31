
#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project - 26/06/2016 
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#
#	display_study() : return an array of VaR from the different titles in input (this is the main utility funct)
#

display_study <- function(titles, start, end){

	studies_var <- c()
	for(i in 1:length(titles)){
		sst <- getprice(titles[i])
		studies_var[i]=study(sst,start,end)[1]
	}

	studies_var
}
