hma_sign <- function(hma){
	sign <- hma
	l <- length(hma)
	for(i in 24:l){
		if(hma[i]>hma[i-1]){sign[i-1]=1}
		else{sign[i-1]=0}
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
HTA_signal <- function(name, length_sample, tt_t=25, hma_n=20, adx_n=14){
	
	library(tseries)
	library(zoo)
	library(TTR)
	
	data_OHLC <- get.hist.quote(name)
	end <- length(data_OHLC[,4])
	start <- end-(length_sample)
	
	data_hma <- HMA(data_OHLC[,4],n=hma_n)
	data_hma_sign <- hma_sign(data_hma)
	data_adx <- ADX(cbind(data_OHLC[,2],data_OHLC[,3],data_OHLC[,4]),n=adx_n)
	data_tt <- tether(cbind(data_OHLC[,2],data_OHLC[,3]),t=tt_t)
	
	data <- cbind(data_OHLC[,4],data_hma,data_hma_sign,data_adx[,1],data_tt,data_adx[,2])
	#data[,4] DI+ data[,6] DI-
	
	up_signals <- c()
	up_c <- 1
	sell_signals <- c()
	sell_c <- 1
	down_signals <- c()
	down_c <- 1
	buy_signals <- c()
	buy_c <- 1
	
	
	for(t in start:end){
		if(data[(t-1),5]>data[(t-1),1] && data[t,5]<data[t,1] && data[t,3]==1 && data[t,4]>20){
			#up_signal
			up_signals[up_c] <- t
			up_c <- up_c+1
			flag_up=1
			for(tt in t:end){
				#print(cbind(tt,data[(tt-1),5],data[(tt-1),1]))  CHECK
				if(data[(tt-1),5]<data[(tt-1),1] && data[tt,5]>data[tt,1] && flag_up==1){
					#quit
					sell_signals[sell_c] <- tt
					sell_c <- sell_c+1
					flag_up <- 0
				}
			}
		}
		if(data[(t-1),5]<data[(t-1),1] && data[t,5]>data[t,1] && data[t,3]==0 && data[t,6]>20){
			#down_signal
			down_signals[down_c] <- t
			down_c <- down_c+1
			flag_down=1
			for(tt in t:end){
				if(data[(tt-1),5]>data[(tt-1),1] && data[tt,5]<data[tt,1] && flag_down==1){
					#quit
					buy_signals[buy_c] <- tt
					buy_c <- buy_c+1
					flag_down <- 0
				}
			}
		}
	}	
	
	cbind(up_signals, sell_signals, down_signals, buy_signals)
}


date_sign <- function(serie,sign,mode){
	date_sign_first <- time(serie)[sign[1,mode]]
	date_sign <- c(date_sign_first)
	for(i in 2:length(sign[,mode])){
		date_sign[i]=time(serie)[sign[i,mode]]
		#print(date_sign[i])
	}
	date_sign
}
proceed <- function(serie,HTA_sign){
	
	sign_up <- c(date_sign(serie,HTA_sign,1),as.Date("3000-01-01"))
	#print(sign_up)
	count_up <- 1
	sign_sell <- c(date_sign(serie,HTA_sign,2),as.Date("3000-01-01"))
	sign_down <- c(date_sign(serie,HTA_sign,3),as.Date("3000-01-01"))
	#print(sign_down)
	count_down <- 1
	sign_buy <- c(date_sign(serie,HTA_sign,4),as.Date("3000-01-01"))

	output <- c(0)
	count_out <- 2
	
	for(i in 1:(4*length(sign_up))){
		
			if(sign_up[count_up]<sign_down[count_down]){	
				if(count_up==1 || HTA_sign[count_up,1]>HTA_sign[(count_up-1),1]){
				
				ss <- as.numeric(serie[HTA_sign[count_up,1]+1,1])
				print("OPEN POSITION - buy")
				print(sign_up[count_up]) #trigger's date
				print(ss) #open of the next day
				
				ee <- as.numeric(serie[HTA_sign[count_up,2]+1,1])
				print("CLOSE POSITION - sell")
				print(sign_sell[count_up])
				print(ee)
				
				
				
				
				output[count_out] <- (ee-ss)/ss
				print(output[count_out])
				
		
		
				count_out <- count_out+1
				count_up <- count_up+1
				
				print("---------------")
				
				}
				else{ count_up = length(sign_up) }
			}
			
			if(sign_down[count_down]<sign_up[count_up]){
				if(count_down==1 || HTA_sign[count_down,3]>HTA_sign[(count_down-1),3]){
					
				ss <- as.numeric(serie[HTA_sign[count_down,3]+1,1])	
				print("OPEN POSITION - sell")
				print(sign_down[count_down])
				print(ss)
				
				ee <- as.numeric(serie[HTA_sign[count_down,4]+1,1])
				print("CLOSE POSITION - buy")
				print(sign_buy[count_down])
				print(ee)			
				
				
				
				
				output[count_out] <- (ss-ee)/ss
				print(output[count_out])
				
				count_out <- count_out+1
				count_down <- count_down+1
				
				
				print("---------------")

				
			}
				else{ count_down = length(sign_down) }
			}
	
	}
	
	
	output

}
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
trade <- function(name, l=500, tit="BACKTEST", plot=TRUE){
	library(tseries)
	stock <- get.hist.quote(name)
	stock_hta <- HTA_signal(name, l)
	stock_r <- proceed(stock,stock_hta)
	stock_p <- interest(stock_r,title=tit,plot=plot)
	stock_r
}



trade_basket <- function(titoli,l,single_plot=FALSE){
	
	basket <- c(cumsum(trade(titoli[1],l,plot=FALSE)))
	plot(basket[1],xlab="time",ylab="variations",main="basket backtesting",type="l",ylim=c(-1,1),xlim=c(1,30))
	for(i in 2:length(titoli)){
		new_tit <- trade(titoli[i],l,plot=FALSE)
		lines(cumsum(new_tit))
		basket[i] <- new_tit
	}

	basket
}
