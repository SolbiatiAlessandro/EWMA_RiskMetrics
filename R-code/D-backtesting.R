#given our conditioned VaR (derived from our volatility computation) we conduct a binomial test on our backtesting sample 

VaR #is our conditioned VaR, timeserie
returns #our returns

w<- sum(returns < -VaR)
binom.test(w,length(returns),0.01,'t')

#we conduct alfa = 0.01 binomial test to verify our Null Hypotesis on the correctness of the estimated VaR, and thus
#on the correctness of our EWMA volatility model
