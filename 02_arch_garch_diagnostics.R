
# Modeling time-vvarying risk with ARCH and GARCH
# Example: TCS Daily Returns (you use what company has been assigned to you)

# Objectives:
# 1. Understand volatility clustering in returns.
# 2. Justify the use of ARCH/GARCH models.
# 3. Estimate ARCH(1) and GARCH(1,1).
# 4. Run proper diagnostics BEFORE and AFTER estimation.



# Load required packages


library(quantmod)   # for downloading stock data
library(tseries)    # for stationarity tests (ADF)
library(rugarch)    # for ARCH/GARCH estimation
library(FinTS)      # for ARCH-LM test

# download data

# Download daily TCS stock prices from Yahoo Finance
getSymbols("TCS.NS", from = "2015-01-01", src = "yahoo")

# Extract Adjusted Closing Price (preferred for returns)
price <- Ad(TCS.NS)

# log returns

# Compute daily log returns: r_t = log(P_t) - log(P_{t-1})
returns <- diff(log(price))

# Remove first NA observation
returns <- na.omit(returns)

# some visalizations


# Puurpose: See basic behavior of returns (mean, variability, crises)

plot(
  returns,
  main = "TCS Daily Log Returns",
  ylab = "Returns",
  xlab = "Time",
  col = "steelblue"
)
abline(h = 0, col = "red", lty = 2)

# Interpretation for students:
# Mean should be near zero.
# Look for periods of calm vs turbulence.

# plot squared returns
# Purpose: Visual check for volatility clustering

plot(
  returns^2,
  main = "Squared Log Returns of TCS",
  ylab = "Squared Returns",
  xlab = "Time",
  col = "darkorange"
)

# Interpretation:
# - Clusters of large values indicate volatility clustering.

#stationary check

#  ADF test on prices (for teaching contrast)
# Expectation: Prices are usually non-stationary.

adf.test(na.omit(price))

#  ADF test on returns (WHAT MATTERS)
# Requirement for ARCH/GARCH: RETURNS must be stationary.

adf.test(returns)

# autocrerlation check

# ACF of returns (mean structure)
# Purpose: Check if we need an ARMA model in the mean.

acf(
  returns,
  main = "ACF of Returns (Check Mean Structure)"
)

# If strong autocorrelation exists, you should model ARMA first.

# ACF of squared returns (KEY JUSTIFICATION FOR ARCH) 

acf(
  returns^2,
  main = "ACF of Squared Returns (Volatility Clustering)"
)

# Interpretation:
# Significant spikes justify ARCH/GARCH modeling.

# arch test (early diagnostics)

# Engle's ARCH-LM test on raw returns
ArchTest(returns, lags = 5)

# Decision rule:
# p-value < 0.05  -> ARCH effects present -> Proceed to ARCH/GARCH
# p-value > 0.05  -> ARCH/GARCH may not be necessary

# 
#  ESTIMATE ARCH(1) MODEL

spec_arch1 <- ugarchspec(
  
  variance.model = list(
    
    # "sGARCH" refers to the standard GARCH framework in rugarch.
    # Even when we want a pure ARCH model, rugarch still requires "sGARCH"
    # because ARCH is implemented as a special case of GARCH.
    model = "sGARCH",  
    
    # garchOrder = c(p, q)
    # p = number of GARCH terms (lags of past conditional variance h_{t-1}, h_{t-2}, ...)
    # q = number of ARCH terms (lags of past squared shocks ε_{t-1}^2, ε_{t-2}^2, ...)
    #
    # Here we use c(1, 0):
    # p = 1, q = 0
    # Since q = 0, the GARCH part drops out in practice.
    # This effectively gives:
    # h_t = α0 + α1 * ε_{t-1}^2
    # which is an ARCH(1) model.
    #
    # you could later change this to:
    # c(2,0)  -> ARCH(2)
    # c(1,1)  -> GARCH(1,1)
    # c(2,1)  -> GARCH(2,1)
    garchOrder = c(1, 0)
  ),
  
  mean.model = list(
    
    # armaOrder = c(p, q) specifies the mean equation for returns.
    # p = number of AR terms, q = number of MA terms.
    #
    # c(0,0) means no AR and no MA terms, so the mean equation is simply:
    # r_t = μ + ε_t
    #
    # If you wanted to allow predictability in returns, they could use:
    # c(1,0) for AR(1)
    # c(0,1) for MA(1)
    armaOrder = c(0, 0),
    
    # include.mean = TRUE means we estimate a constant mean μ.
    # If this were FALSE, the model would force μ = 0.
    include.mean = TRUE
  ),
  
  # distribution.model specifies the assumed distribution of ε_t given past information.
  #
  # "norm" means:
  # ε_t | F_{t-1} ~ Normal(0, h_t)
  #
  # if you want you could later change this to:
  # "std"  for Student-t (if returns have fat tails)
  # "sstd" for skewed Student-t
  distribution.model = "norm"
)

# This actually estimates the model.

fit_arch1 <- ugarchfit(
  
  # 'spec' tells rugarch which model structure to estimate
  spec = spec_arch1,
  
  # 'data' is the series to which the model is fitted.
  # This must be returns, not prices.
  data = returns
)

# This prints the results:
# estimated parameters (μ, α0, α1),
# log-likelihood,
# AIC/BIC,
# and diagnostic tests (Ljung–Box, ARCH-LM, sign bias, etc.)



# Show results
show(fit_arch1)

#diagnostics 


# pplot estimated ARCH volatility 
plot(fit_arch1, which = 2)

# Interpretation:
# Spiky volatility
# Quick decay after shocks (typical ARCH behavior)

# Check remaining ARCH in residuals

res_arch <- residuals(fit_arch1, standardize = TRUE)

# ACF of squared standardized residuals
acf(res_arch^2, main = "ACF of Squared Residuals (ARCH)")

# Formal ARCH-LM test on residuals
ArchTest(res_arch, lags = 5)

# If still significant -> ARCH(1) is insufficient -> move to GARCH.

# 
# GARCH (1,1) model

spec_garch11 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), # GARCH(1,1)
  mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit_garch11 <- ugarchfit(
  spec = spec_garch11,
  data = returns
)

##expalined code in deatil for the same thing you ran aboce
spec_garch11 <- ugarchspec(
  
  variance.model = list(
    
    # "sGARCH" is the standard GARCH framework in rugarch.
    # This is the usual specification for most basic GARCH models.
    model = "sGARCH",
    
    # garchOrder = c(p, q)
    # p = number of GARCH terms (lags of past conditional variance h_{t-1}, h_{t-2}, ...)
    # q = number of ARCH terms (lags of past squared shocks ε_{t-1}^2, ε_{t-2}^2, ...)
    #
    # Here we use c(1, 1), which means:
    # p = 1 (one lag of past variance)
    # q = 1 (one lag of past squared shock)
    #
    # This gives the standard GARCH(1,1) variance equation:
    # h_t = α0 + α1 * ε_{t-1}^2 + β1 * h_{t-1}
    #
    # you could later experiment with:
    # c(2,1)  -> GARCH(2,1)
    # c(1,2)  -> GARCH(1,2)
    # c(2,2)  -> GARCH(2,2)
    garchOrder = c(1, 1)
  ),
  
  mean.model = list(
    
    # armaOrder = c(p, q) specifies the mean equation for returns.
    # p = number of AR terms, q = number of MA terms.
    #
    # c(0,0) means no AR and no MA terms, so the mean equation is simply:
    # r_t = μ + ε_t
    #
    # If you wanted to model predictability in returns, they could use:
    # c(1,0) for AR(1)
    # c(0,1) for MA(1)
    armaOrder = c(0, 0),
    
    # include.mean = TRUE means we estimate a constant mean μ.
    # If this were FALSE, the model would force μ = 0.
    include.mean = TRUE
  ),
  
  # distribution.model specifies the assumed conditional distribution of ε_t.
  #
  # "norm" means:
  # ε_t | F_{t-1} ~ Normal(0, h_t)
  #
  # you could later change this to:
  # "std"  for Student-t (if returns have fat tails)
  # "sstd" for skewed Student-t
  distribution.model = "norm"
)

# This estimates the GARCH(1,1) model.

fit_garch11 <- ugarchfit(
  
  # 'spec' tells rugarch which model structure to estimate
  spec = spec_garch11,
  
  # 'data' is the series to which the model is fitted.
  # This must be returns, not prices.
  data = returns
)

# This prints:
# estimated parameters (μ, α0, α1, β1),
# log-likelihood,
# AIC/BIC,
# and diagnostic tests (Ljung–Box, ARCH-LM, sign bias, etc.)





# Show results
show(fit_garch11)

# diagnostics

res_garch <- residuals(fit_garch11, standardize = TRUE)

#  Check autocorrelation in standardized residuals 
acf(res_garch, main = "ACF of Standardized Residuals (GARCH)")

# Check remaining ARCH in squared residuals 
acf(res_garch^2, main = "ACF of Squared Standardized Residuals (GARCH)")

#  Ljung–Box test (formal check) 

# Test for autocorrelation in residuals (mean structure)
Box.test(res_garch, lag = 10, type = "Ljung")

# Test for remaining ARCH in squared residuals
Box.test(res_garch^2, lag = 10, type = "Ljung")
# try this for ARCH and see why GARCH is better :)

#  ARCH-LM test on GARCH residuals 
ArchTest(res_garch, lags = 5)

# Interpretation:
# p-value > 0.05 -> GARCH successfully removed ARCH effects.

# why garch better than arch

#  Compare volatility plots

plot(fit_arch1, which = 2)    # ARCH volatility (spiky)
plot(fit_garch11, which = 2)  # GARCH volatility (smoother, persistent)

#  Compare model fit (AIC/BIC) 

infocriteria(fit_arch1)
infocriteria(fit_garch11)

# Lower AIC/BIC -> better model (usually GARCH).

# Compare residual diagnostics 

acf(res_arch^2,  main = "Squared Residuals: ARCH")
acf(res_garch^2, main = "Squared Residuals: GARCH")

# Typically:
#  ARCH still shows some structure
#  GARCH cleans it up.


