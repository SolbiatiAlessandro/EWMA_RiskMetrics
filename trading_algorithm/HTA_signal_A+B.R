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
	
	up_signals <- c()
	up_c <- 1
	sell_signals <- c()
	sell_c <- 1
	down_signals <- c()
	down_c <- 1
	buy_signals <- c()
	buy_c <- 1
	
	#aggiunto che anche giorno di entrata il prezzo deve essere sopra teth
	
	for(t in start:end){
		if(data[(t-1),5]>data[(t-1),1] && data[t,5]<data[t,1] && data[(t+1),5]<data[(t+1),1] && data[t,4]>20){
			if(data[t,3]==1){ #no delta HMA
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
		}
		if(data[(t-1),5]<data[(t-1),1] && data[t,5]>data[t,1] && data[(t+1),5]>data[(t+1),1] && data[t,6]>20){
			if(data[t,3]==0){  #no delta HMA
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
	}	
	
	cbind(up_signals, sell_signals, down_signals, buy_signals)
}

