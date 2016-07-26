
#given an array of returns(sst) of our timeseries and an array of its related volatility stochastic process sigma_t 
#we can calculate the Z_t array for the standardized residuals

Z_t <- returns(sst)/sigma_t

#Z_t is distributed as a standard normal, and we can test its autocorrelation to examine the correctness of our model

qqplot(Z_t)
acf(Z_t)
pacf(Z_t)  

#we can execute a LjungBoxTest for the p-values of correlations till the 20th order of delay

require(FitAR)
LjungBoxTest(Z_t,20)
LBQPlot(Z_t,20)
