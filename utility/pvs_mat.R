#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project - 26/06/2016 
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#
#	pvs_mat() : computes pvalues matrix
#	rows: confidence level (0.9,0.95,0.99,0.995) 
#	cols: usage (1,2,3)
#
#	Usage:
#		my_matrix=pvs_mat(IBM,"start_date","end_date")
#		my_matrix
#

pvs_mat <- function(serie,start,end){

	pvs <- matrix(nrow=3,ncol=4)
	rownames(pvs) <- c("Non-Parametric","Normal Distr","Student T Distr")
	colnames(pvs) <- c("conf 0.90","conf 0.95","conf 0.99","conf 0.995")

	conf <- 0.9
	j <- 1
	for(i in 1:3){
		pvs[i,j]=as.numeric(EWMA_RiskMetrics(serie,conf,i,start,end,VaR=TRUE)[1,5])
		}

	conf <- 0.95
	j <- 2
	for(i in 1:3){
		pvs[i,j]=as.numeric(EWMA_RiskMetrics(serie,conf,i,start,end,VaR=TRUE)[1,5])
		}
		
	conf <- 0.99
	j <- 3
	for(i in 1:3){
		pvs[i,j]=as.numeric(EWMA_RiskMetrics(serie,conf,i,start,end,VaR=TRUE)[1,5])
		}

	conf <- 0.995
	j <- 4
	for(i in 1:3){
		pvs[i,j]=as.numeric(EWMA_RiskMetrics(serie,conf,i,start,end,VaR=TRUE)[1,5])
	}
	
	pvs
	}
