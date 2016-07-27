#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#
# 	compute Value at Risk under Normal Distribution Hypotesis (Var), if VaR==FALSE compute Expected Shortfall
#

calc_VaRnorm<- function(Serie,conf,VaR,s_startdate,s_enddate)
{
  
  require(timeSeries)
  
  cc<- -(Serie) 
  cc<- window(cc,start=s_startdate,end=s_enddate)
  cc<- as.vector(cc)
  
  me<- mean(cc) #mean
  ds<- sd(cc)	#standard deviation
  
  if (VaR) { out<- me+ds*qnorm(conf) } #compute VaR as a normal distr
  else { out<- me+ds*dnorm(qnorm(conf))/(1-conf) } #compute ES
 
  out
}
