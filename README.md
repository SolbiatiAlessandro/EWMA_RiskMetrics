# EWMA-RiskMetrics

An implementation on R of the EWMA filter for volatility by RiskMetrics™ (JPMorgan &amp; Reuters 1996) 



## Current State of the Project:


### Implemented in 'R-code' folder the basic functions for the EWMA filter:

- calculate the EWMA volatility with RiskMetrics formulas
- compute the Value at Risk with three diffrent methods: non parametric, returns distributed as a normal, returns distributed as a student's t
- binomial backtest of the Value at Risk correctness

### Currently working EWMA function in the EWMA_executable.R file

- once uploaded in the R's IDE the function correctly compute the p-values and returns the VaR series

### Utility

- pvs_mat.R allows to compute many EWMAs at different confidence levels and usage types all at once

## Further developments:

- upload statistical model and formulas explaination in the project page
- apply the model to data and upload results




##Reference for the project

The project has been developed based on the Techincal Document by JPMorgan and Reuters (specifically on chapter 5 https://www.msci.com/documents/10199/5915b101-4206-4ba0-aee2-3449d5c7e95a)







