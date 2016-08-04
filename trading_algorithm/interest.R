interest <- function(perc,mode=1,cap=1,yinf=0.5,ysup=1.5,title="Backtest",plot=TRUE){
	if(mode==1){
		li <- sum(perc)
		if(plot==TRUE){
			plot(cumsum(perc),type='l',ylab="variation",xlab="time",main=title)
			}	
		return(li)
		}
		
	if(mode==2){
		capt <- c(cap)
		for(i in 1:length(perc)){capt[i+1] <- capt[i]*(1+perc[i])}
		if(plot==TRUE){
			plot(x=c(1:length(capt)),y=capt, type='l', ylim=c(yinf*cap,ysup*cap), ylab="capital", xlab="time",main=title)
			}
		return(capt)
		}
		
	#if(mode==3) <- EWMA_RiskMetrics()®
	
}