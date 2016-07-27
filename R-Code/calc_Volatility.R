#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
# 	
#	Compute the conditioned volatility with the EWMA formula
#

 calc_Volatility <- function(X_t, N){
  
  require(timeSeries)
  sigma_t <- c()
  is.timeSeries(sigma_t)
  w<- 0.94^(0:74) #lambda = 0.94, T=77 as specified in RiskMetrics
  sigma_t[77]<- sum(w*X_t[76:2]^2)/sum(w) #compute the 77th term 
  for (s in 78:N){ #and all the others
    sigma_t[s]<- 0.94*sigma_t[s-1]+0.06*X_t[s-1]^2
  }
  sigma_t<- sqrt(sigma_t) #here it is the conditioned volatility serie
  sigma_t 
  
  
  }
