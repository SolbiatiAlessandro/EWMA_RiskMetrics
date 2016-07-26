# estimate of the stochastic process for the volatility sigma_t
# reference: Quantitative Finance with R http://www.apogeonline.com/libri/9788850315963 
# reference: JPMorgan and Reuters (1996) https://www.msci.com/documents/10199/5915b101-4206-4ba0-aee2-3449d5c7e95a

#library required: timeSeries, tseries

#sst is a timeSeries object with the daily prices

# Ci assicuriamo che i pacchetti necessari siano stati caricati
  require(timeSeries)
  require(fGarch)
  # Carichiamo il file con la serie storica delle quotazioni
  load('S&P500.RData')
  # Creiamo l'oggetto timeSeries in cui memorizzare le due serie
  out<- cbind(returns(SP500),NA)
  colnames(out)<- c('Rend','sigma_t')
  # Applichiamo il metodo EWMA di RiskMetrics
  w<- 0.94^(0:74)
  out$sigma_t[76]<- sum(w*out$Rend[75:1]^2)/sum(w)
  for (i in 77:length(out[,1]))
  {
    out$sigma_t[i]<- 0.94*out$sigma_t[i-1]+0.06*out$Rend[i-1]^2
  }
  out$sigma_t<- sqrt(out$sigma_t)
  # Diamo le impostazioni della finestra grafica
  IGS()
  # Tracciamo il grafico delle due serie storiche
  plot(out,plot.type='single',col=c('grey60','black'),xlab='',ylab='',main='')
  # Aggiungiamo la griglia
  depo<- seq(time(SP500)[2],time(SP500)[length(SP500)],length.out=6)
  abline(v=depo,lty=2,col='lightgray')
  grid(NA,NULL,lty=2,col='lightgray')
  # Salvataggio
  savePlot('EWMA','pdf')
