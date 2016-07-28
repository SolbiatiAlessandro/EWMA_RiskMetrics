#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project - 26/06/2016 
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#
#	EWMA_RiskMetrics main function: computes and test all the process of the EWMA Volatility and VaR estimation
#
#	functions required: (in the same folder)
#
#	calc_Volatility()			[calc_Volatility.R]
#	calc_VaRnp()				[calc_VaRnp.R     ]
#	calc_VaRnorm()				[calc_VaRnorm.R   ]
#	calc_VaRt()					[calc_VaRt.R      ]
#	exe_backtesting()			[exe_backtesting.R]
#
# The function return an matrix 'EWMA_result'
# To obtain the p-value of the binomial test on the effectiviness of the computed Value at Risk use "as.numeric(EWMA_RiskMetrics(...)[1,5])"
# For further informations see the documentations on the project page


EWMA_RiskMetrics<- function(Serie,conf,usage,s_startdate,s_enddate)
{
  
  require(timeSeries)
  
  # Settings: calculate returns of the prices and put it in a vector SSt
  SSt<- cbind(Price_t=Serie,X_t=c(NA,returns(Serie)),sigma_t=NA,VaR=NA)
  Nt<- length(SSt[,1]) #total length of the serie
  Ns<- length(window(SSt[,2],start=s_startdate,end=s_enddate)) #length of the statistical sample
  
  # Compute the conditioned volatility with the EWMA formula
  SSt$sigma_t=calc_Volatility(SSt$X_t,Nt)
  
  # Compute standardized residuals 
  Z_t<- as.timeSeries(SSt$X_t[77:Ns]/SSt$sigma_t[77:Ns])
  rownames(Z_t)<- rownames(SSt)[77:Ns] 
  
  #VaR_Z estimations

  if (usage==1) { VaR_Z<- calc_VaRnp(-Z_t,conf,VaR=TRUE,s_startdate=s_startdate,s_enddate=s_enddate) }
  if (usage==2) { VaR_Z<- calc_VaRnorm(-Z_t,conf,VaR=TRUE,s_startdate=s_startdate,s_enddate=s_enddate) }
  if (usage==3) { VaR_Z<- calc_VaRt(-Z_t,conf,VaR=TRUE,s_startdate=s_startdate,s_enddate=s_enddate) }
  #in the 3rd VaR_Z method (student's t) there could be a convergence error due to the initial value of the computing algorithm, 
  #to fix this issue just need to add the argument "init=c(a,b,c)" where a, b and c are three numbers you can aribtrarily choose
  
  
  #Compute conditioned VaR serie
  
  SSt$VaR<- SSt$sigma_t*VaR_Z
  

  # backtesting
  test=exe_backtesting(X_t = SSt$X_t, VaR = SSt$VaR, Ns=Ns, Nt=Nt, conf=conf)
  
  #output
  EWMA_result=cbind(SSt,test$p.value,test$estimate)
  EWMA_result
  
  #to get the test$p.value use the command "as.numeric(EWMA_RiskMetrics(...)[1,5])"
  
}
