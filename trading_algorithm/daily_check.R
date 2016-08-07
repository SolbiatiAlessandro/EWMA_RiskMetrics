

hma_sign <- function(hma){
	sign <- hma
	l <- length(hma)
	for(i in 24:l){
		if(hma[i]>hma[i-1]){sign[i]=1}
		else{sign[i]=0}
	}
	sign
}
tether <- function(HL,t=50){
	l=length(HL[,1])
	tether <- c()
	for(i in 51:l){
		high <- max(HL[(i-t):i,1])
		low <- min(HL[(i-t):i,2])
		tether[i] <- (high+low)/2
		#print(c(high,low,tether[i],serie[i],i,(i-t)))
		
	}
	tether
}


daily_check <- function(titoli,posizioni_up=NULL,posizioni_down=NULL,med_up=NULL,med_down=NULL){
	
	signals <- c() #check_signal()
	for(i in 1:length(titoli)){
	
		signals[i] <- check_signal(titoli[i])
		if(signals[i]!=0) print("*********************** SIGNAL ************************")
		else print(signals[i])
		print(i)
		
	}
	
	sell <- c() #check_sell
	for(i in 1:length(posizioni_up)){
		
		sell[i] <- check_sell(posizioni_up[i])
		
	}
	
	buy <- c() #check_buy
	for(i in 1:length(posizioni_down)){
		
		buy[i] <- check_buy(posizioni_down[i])
	}
	
	go_up <- c()
	for(i in 1:length(med_up)){
		go_up[i] <- check_to_up(med_up[i])
	}
	
	go_down <- c()
	for(i in 1:length(med_down)){
		go_down[i] <- check_to_down(med_down[i])
	}
	
	cbind(signals,sell,buy,go_up,go_down)

}

check_signal <- function(name){
	
	#HMA -> 20p
	#tether -> 25p
	#ADX -> 14p
	
	
	library(tseries)
	library(zoo)
	library(TTR)
	
	
	#print("check")
	
	data_OHLC <- get.hist.quote(name, start="2016-05-01")
	end <- length(data_OHLC[,4])
	
	#print("check")

	data_hma <- HMA(data_OHLC[(end-24):end,4],n=20)
	
	
	#print("check")
	
	data_hma_sign <- hma_sign(data_hma)
	
	
	#print("check")
	
	data_adx <- ADX(cbind(data_OHLC[(end-50):end,2],data_OHLC[(end-50):end,3],data_OHLC[(end-50):end,4]),n=14)
	
	
	#print("check")
	
	data_tt <- tether(cbind(data_OHLC[(end-51):end,2],data_OHLC[(end-51):end,3]),t=25)
	
	
	#print("check j")
	#print(data_tt[(length(data_tt)-1)])
	#print(data_OHLC[(end-1),4])
	#print(data_tt[length(data_tt)])
	#print(data_OHLC[end,4])
	#print(as.numeric(data_adx[length(data_adx[,4]),4]))
	#print(data_hma_sign[length(data_hma_sign)])
	
	if(data_tt[(length(data_tt)-1)]>data_OHLC[(end-1),4] && data_tt[length(data_tt)]<data_OHLC[end,4] && data_adx[length(data_adx[,4]),4]>20 && data_hma_sign[length(data_hma_sign)]==1){		segnale <- 0.5		}
	
	else if(data_tt[(length(data_tt)-1)]<data_OHLC[(end-1),4] && data_tt[length(data_tt)]>data_OHLC[end,4] && data_adx[length(data_adx[,4]),4]>20 && data_hma_sign[length(data_hma_sign)]==0){		segnale <- -0.5		}
	
	else segnale <- 0
	
	return(segnale)
	
}

check_to_up <- function(name){
	
	#HMA -> 20p
	#tether -> 25p
	#ADX -> 14p
	
	
	library(tseries)
	library(zoo)
	library(TTR)
	
	
	#print("check")
	
	data_OHLC <- get.hist.quote(name, start="2016-05-01")
	end <- length(data_OHLC[,4])	
	#print("check")
	
	data_tt <- tether(cbind(data_OHLC[(end-51):end,2],data_OHLC[(end-51):end,3]),t=25)
	
	#print("check j")
	#print(data_tt[(length(data_tt)-1)])
	#print(data_OHLC[(end-1),4])
	#print(data_tt[length(data_tt)])
	#print(data_OHLC[end,4])
	
	if(data_tt[length(data_tt)]<data_OHLC[end,1]){
		
		segnale <- 1
		
	}	
	
	cbind(segnale, data_tt[length(data_tt)-1]	,data_tt[length(data_tt)], data_OHLC[end,1])
}

check_to_down <- function(name){
	
		
	#HMA -> 20p
	#tether -> 25p
	#ADX -> 14p
	
	
	library(tseries)
	library(zoo)
	library(TTR)
	
	
	#print("check")
	
	data_OHLC <- get.hist.quote(name, start="2016-05-01")
	end <- length(data_OHLC[,4])	
	#print("check")
	
	data_tt <- tether(cbind(data_OHLC[(end-51):end,2],data_OHLC[(end-51):end,3]),t=25)
	
	#print("check j")
	#print(data_tt[(length(data_tt)-1)])
	#print(data_OHLC[(end-1),4])
	#print(data_tt[length(data_tt)])
	#print(data_OHLC[end,4])
	
	if(data_tt[length(data_tt)]>data_OHLC[end,1]){
		
		segnale <- 1
		
	}	
	
	else segnale <- 0
	
	cbind(segnale, data_tt[length(data_tt)-1]	,data_tt[length(data_tt)], data_OHLC[end,1])
	
}

check_sell <- function(titolo){
	
}

check_buy <- function(titolo){

}

cosa_faccio <- function(titoli, ups = NULL, downs = NULL){
	
	signs <- daily_check(titoli)
	print(signs)
	for(i in 1:length(signs)){
		if(signs[i]==0.5){
			
			print("se domani apre sopra thether ENTRA LONG su")
			print(titoli[i])
			
		}
		else if(sign[i]==-0.5){
			
			print("se domani apre sotto theter ENTRA SHORT su")
			print(titoli[i])
			
		}
		else print("oggi relax!")
	}
}

