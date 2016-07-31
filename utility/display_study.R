
#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project - 26/06/2016 
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#

display_study <- function(titles, start, end){
	data=create_dataset(titles)
	studies_var <- c()
	for(i in 1:length(titles)){
		studies_var[i]=study(data[,i],"2000-01-01","2015-12-31")[1]
	}

	studies_var
}