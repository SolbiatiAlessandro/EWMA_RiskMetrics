#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project - 26/06/2016 
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#
#	max_pv(): return the max p-value and the method/conf of the test
#	return a vect(max p-value, row, col)
#	where row=1 <- NonParametric
#		  row=2 <- Normal Distr
#	      row=3 <- T Student Distr
#	where col(1,2,3,4)=(0.90,0.95,0.99,0.995)


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