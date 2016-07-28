#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#
#	Backtesting Binomial Test Exe
#

exe_backtesting <- function(X_t,VaR,Ns,Nt,conf){
	
  w<- sum(X_t[(Ns+1):Nt]<(-VaR[(Ns+1):Nt])) #compute the number of excpetions
  ttest<- binom.test(w,Nt-Ns,1-conf,'t') #execute a binomial test on the numbers of exceptions
 
  #could be implemented other statistical tests, like a approximated normal distribution test
 
  ttest
}
