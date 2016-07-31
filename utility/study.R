#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project - 26/06/2016 
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#
#	study() : study a timeSerie defining the best method/confidence couple and return the last 10 VaR and ES
# 

study <- function(serie,start,end){
	
	vect <- max_pv(serie,start,end)
	usage <- vect[2]
	col <- vect[3]
	
	if(col==1){conf <- 0.9}
	if(col==2){conf <- 0.95}
	if(col==3){conf <- 0.99}
	if(col==4){conf <- 0.995}
	
	ewma_VaR <- EWMA_RiskMetrics(serie, conf, usage, start, end, VaR=TRUE)
	ewma_ES <- EWMA_RiskMetrics(serie, conf, usage, start, end, VaR=FALSE)
	l=length(ewma_VaR[,1])
	
	out <- cbind("Value at Risk"=ewma_VaR[,4],"Expected Shortfall"=ewma_ES[,4],"p-value"=ewma_VaR[l,5])
	out <- cbind(out[l,],"confidence"=conf)
	out
	
}