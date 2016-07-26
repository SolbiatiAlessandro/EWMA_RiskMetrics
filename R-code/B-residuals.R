
#given an array of returns(sst) of our timeseries and an array of its related volatility stochastic process sigma_t 
#we can calculate the Z_t array for the standardized residuals

Z_t <- returns(sst)/sigma_t

#Z_t is distributed as a standard normal, and we can test its autocorrelation to examine the correctness of our model

qqnorm(Z_t)  #distributed as a normal?
acf(Z_t[76:length(Z_t)])  #plot correlgram, start from 76 because the sigma_t is calculated from the 76th value (view A-volatility.R)
pacf(Z_t[76:length(Z_t)])  #partial autocorrelation

#we can execute a LjungBoxTest for the p-values of correlations till the 20th order of delay

require(FitAR)
LjungBoxTest(Z_t,20)
LBQPlot(Z_t,20)
