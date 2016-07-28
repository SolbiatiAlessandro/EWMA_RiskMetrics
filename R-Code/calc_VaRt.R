#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#
# 	compute Value at Risk under Student's t distr Hypotesis (Var), if VaR==FALSE compute Expected Shortfall
#


#first define the density function for student's t
funzDens<- function(x,me,ds,nu)
{
  out<- 1/ds*dt((x-me)/ds,nu)
  out
}
   
calc_VaRt <- function(Serie,conf,VaR,s_startdate,s_enddate,init=c(0,1,1))
{
  
  require(timeSeries)
  require(MASS)
  
  cc<- -(Serie) 
  cc<- window(cc,start=s_startdate,end=s_enddate)
  cc<- as.vector(cc)
  
  #through a likelihood function we estimate distribution parameters
  ss<- fitdistr(cc,funzDens,start=list(me=init[1],ds=init[2],nu=init[3]))
  
  if (VaR) { 
  	#computing VaR 
    out<- Stime$estimate[1]+Stime$estimate[2]*qt(conf,Stime$estimate[3])
  }
  
  else {
  	#computing ES 
  	if (Stime$estimate[3]>1) {
    	depo<- dt(qt(conf,Stime$estimate[3]),Stime$estimate[3])/(1-conf)
    	depo<- depo*(Stime$estimate[3]+(qt(conf,Stime$estimate[3]))^2)/(Stime$estimate[3]-1)
    	}
  	else { depo<- NA }
    out<- Stime$estimate[1]+Stime$estimate[2]*depo
  }
  
  as.numeric(out)
  
}
