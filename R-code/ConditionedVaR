#we calcuate the conditioned VaR given from the residuals, and then we conduct a binomial test to verify our hypotesis

Z_t #array of estimated residuals

require(MASS)
ss <- fitdistr(-Z_t, 't') #we suppose our Z_t is distributed as a T-Student and we calc. parameters

VaR_Z <- ss$estimate[1]+ss$estimate[2]*qt(0.99,ss$estimate[3]) #by the VaR formula

VaR #is a timeseries object
VaR <- sigma_t*VaR_Z #here we have our serie of conditioned VaR 
