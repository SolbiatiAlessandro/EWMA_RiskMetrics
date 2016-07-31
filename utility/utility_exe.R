#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project - 26/06/2016 
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#
#	this is the R executable file with all Utilites functions
#

getprice <- function(name_code){
	
	require(tseries)
	require(timeSeries)
		
	
	pp <- get.hist.quote(instrument=name_code,quote="Close")
	pp <- as.timeSeries(pp)
	pp
	

}


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


max_pv <- function(serie,start,end){
	
	my_mat <- pvs_mat(serie,start,end)
	
	pv <- 0
	row <- 0
	col <- 0
	for(i in 1:3){
		for(j in 1:4){
			if(my_mat[i,j]>pv){
				pv <- my_mat[i,j]
				row=i
				col=j
			}
		}
	}
	
	out <- c(pv,row,col)
	out
	
}

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


display_study <- function(titles, start, end){

	studies_var <- c()
	for(i in 1:length(titles)){
		sst <- getprice(titles[i])
		studies_var[i]=study(sst,"2000-01-01","2015-12-31")[1]
	}

	studies_var
}
