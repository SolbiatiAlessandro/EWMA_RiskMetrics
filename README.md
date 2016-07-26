# EWMA-RiskMetrics

An implementation on R of the EWMA filter for volatility by RiskMetrics™ (JPMorgan &amp; Reuters 1996) 



## Current State of the Project:


### Implemented in 'R-code' folder the basic code for the EWMA filter:

–> A-volatility.R for the estimate of the volatility

–> B-residuals.R for the estimate of the residuals

-> C-conditionedVaR.R for the estimate of the conditioned VaR from the EWMAed volatility

-> D-backtesting.R for the binomial test that verify the correctness of the conditioned VaR and thus of the volatility models

## Further developments:

- upload statistical model and formulas explaination in the project page
- apply the model to data and upload results




##Reference for the project

The project has been developed based on the Techincal Document by JPMorgan and Reuters (specifically on chapter 5 https://www.msci.com/documents/10199/5915b101-4206-4ba0-aee2-3449d5c7e95a)







