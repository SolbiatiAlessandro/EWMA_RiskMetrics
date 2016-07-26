# estimate of the stochastic process for the volatility sigma_t
# reference: Quantitative Finance with R http://www.apogeonline.com/libri/9788850315963 
# reference: JPMorgan and Reuters (1996) https://www.msci.com/documents/10199/5915b101-4206-4ba0-aee2-3449d5c7e95a

#library required: timeSeries, tseries

head(sst) #sst is a timeSeries object with the daily prices

require(timeSeries)
out<- cbind(returns(sst),NA,NA)
colnames(out)<- c('Rend','sigma_t','Z_t')

  # EWMA formulas
  
  # lambda = 0.94
w<- 0.94^(0:74)  
  # here we estimate the 76th component for our volatility process  
out$sigma_t[76]<- sum(w*out$Rend[75:1]^2)/sum(w) 
  #now the cycle recursively calculate the sigma_t array
for (i in 77:length(out[,1])) {
    out$sigma_t[i]<- 0.94*out$sigma_t[i-1]+0.06*out$Rend[i-1]^2 
  }
out$sigma_t<- sqrt(out$sigma_t)
  #here we have the volatilty

