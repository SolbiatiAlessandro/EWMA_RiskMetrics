#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project - 26/06/2016 
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#	|||				EXECUTABLE FILE (with all functions needed)			   |||
#	--------------------------------------------------------------------------

#	EWMA_RiskMetrics() main function: computes and tests all the processes of the EWMA Volatility and VaR estimations
#
#	USAGE:
#	- load the file on the R console
#	- just create an object 
#	"my_object <- EWMA_RiskMetrics(Serie, conf, usage, s_startdate, s_enddate)" [see documentations for help]
#   - The function return a matrix 'EWMA_result'
#   - To obtain the p-value of the binomial test on the effectiviness of the computed Value at Risk use "as.numeric(my_object$p.value[1])"
#   
#	For further informations see the documentations on the project page
#
#	functions required: 
#
#	calc_Volatility()			[calc_Volatility.R]
#	calc_VaRnp()				[calc_VaRnp.R     ]
#	calc_VaRnorm()				[calc_VaRnorm.R   ]
#	calc_VaRt()					[calc_VaRt.R      ]
#	exe_backtesting()			[exe_backtesting.R]


calc_VaRnorm<- function(Serie,conf,VaR,s_startdate,s_enddate){
  
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

calc_VaRnp<- function(Serie,conf,VaR,s_startdate,s_enddate){
  
  require(timeSeries)
  
  cc<- -(Serie) 
  cc<- window(cc,start=s_startdate,end=s_enddate)
  cc<- sort(as.vector(cc))
  out<- cc[ceiling(conf*length(cc))] #NP VaR
  if (!VaR) { out<- mean(cc[cc>out]) }  #NP ES
  
  out
}

funzDens<- function(x,me,ds,nu){
  out<- 1/ds*dt((x-me)/ds,nu)
  out
}
   
calc_VaRt <- function(Serie,conf,VaR,s_startdate,s_enddate,init=c(0,1,1)){
  
  require(timeSeries)
  require(MASS)
  
  cc<- -(Serie) 
  cc<- window(cc,start=s_startdate,end=s_enddate)
  cc<- as.vector(cc)
  
  #through a likelihood function we estimate distribution parameters
  ss<- fitdistr(cc,funzDens,start=list(me=init[1],ds=init[2],nu=init[3]))
  
  if (VaR) { 
  	#computing VaR 
    out<- ss$estimate[1]+ss$estimate[2]*qt(conf,ss$estimate[3])
  }
  
  else {
  	#computing ES 
  	if (ss$estimate[3]>1) {
    	depo<- dt(qt(conf,ss$estimate[3]),ss$estimate[3])/(1-conf)
    	depo<- depo*(ss$estimate[3]+(qt(conf,ss$estimate[3]))^2)/(ss$estimate[3]-1)
    	}
  	else { depo<- NA }
    out<- ss$estimate[1]+ss$estimate[2]*depo
  }
  
  as.numeric(out)
  
}

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

exe_backtesting <- function(X_t,VaR,Ns,Nt,conf){
	
  w<- sum(X_t[(Ns+1):Nt]<(-VaR[(Ns+1):Nt])) #compute the number of excpetions
  ttest<- binom.test(w,Nt-Ns,1-conf,'t') #execute a binomial test on the numbers of exceptions
 
  #could be implemented other statistical tests, like a approximated normal distribution test
 
  ttest
}


EWMA_RiskMetrics<- function(Serie,conf,usage,s_startdate,s_enddate,VaR)
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

  if (usage==1) { VaR_Z<- calc_VaRnp(-Z_t,conf,VaR,s_startdate=s_startdate,s_enddate=s_enddate) }
  if (usage==2) { VaR_Z<- calc_VaRnorm(-Z_t,conf,VaR,s_startdate=s_startdate,s_enddate=s_enddate) }
  if (usage==3) { VaR_Z<- calc_VaRt(-Z_t,conf,VaR,s_startdate=s_startdate,s_enddate=s_enddate) }
  #in the 3rd VaR_Z method (student's t) there could be a convergence error due to the initial value of the computing algorithm, 
  #to fix this issue just need to add the argument "init=c(a,b,c)" where a, b and c are three numbers you can aribtrarily choose
  
  
  #Compute conditioned VaR serie
  
  SSt$VaR<- SSt$sigma_t*VaR_Z
  

  # backtesting
  test=exe_backtesting(X_t = SSt$X_t, VaR = SSt$VaR, Ns=Ns, Nt=Nt, conf=conf)
  
  #output
  EWMA_result=cbind(SSt,test$p.value,test$estimate)
  colnames(EWMA_result)=c("Prices","Returns","Volatility(>77th)","VaR(>77th)","p.value","exceed freq.")
  EWMA_result
  
  #to get the test$p.value use the command "as.numeric(EWMA_RiskMetrics(...)[1,5])"
  
}
