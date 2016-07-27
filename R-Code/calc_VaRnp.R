#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#
# 	compute Non Parametric Value at Risk (Var), if VaR==FALSE compute Non Parametric Expected Shortfall
#

calc_VaRnp<- function(Serie,conf,VaR,s_startdate,s_enddate)
{
  
  require(timeSeries)
  
  cc<- -returns(Serie) 
  cc<- window(cc,start=s_startdate,end=s_enddate)
  cc<- sort(as.vector(cc))
  out<- cc[ceiling(conf*length(cc))] #NP VaR
  if (!VaR) { out<- mean(cc[cc>out]) }  #NP ES
  
  out
}