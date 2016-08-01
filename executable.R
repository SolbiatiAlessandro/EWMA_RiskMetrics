#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project - 26/06/2016 
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	------------------------------------------------------------------------------------------------------
#	|||				EXECUTABLE FILE (with all functions needed)			   |||
#	------------------------------------------------------------------------------------------------------



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

EWMA_RiskMetrics<- function(Serie,conf,usage,s_startdate,s_enddate,VaR){
  
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
  EWMA_result
  
  #to get the test$p.value use the command "as.numeric(EWMA_RiskMetrics(...)[1,5])"
  
}

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
