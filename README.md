# Stochastic Simulation & Time-Series Analytics

R-based analytics work spanning stochastic processes, market simulation, ARIMA/ARCH/GARCH modelling, tail-risk estimation and backtesting.

## Components
1. **Stochastic processes & GBM** — normal distributions, random walks, Wiener processes, AR(1), empirical drift/volatility, Euler-Maruyama GBM and Monte Carlo price/portfolio scenarios.
2. **ARCH/GARCH diagnostics** — ADF stationarity checks, ACF of returns and squared returns, Engle ARCH-LM, ARCH(1), GARCH(1,1), standardized-residual diagnostics, Ljung-Box tests and AIC/BIC comparison.
3. **Reliance end-to-end risk analytics** — 2015–2025 adjusted-price data, 2,714 daily log returns, ARIMA mean modelling, GARCH(1,1)-Student-t volatility modelling, VaR/Expected Shortfall, Kupiec backtesting and Black-Scholes sensitivity analysis.

## Key executed results
- Preferred volatility specification: GARCH(1,1)-Student-t.
- Final annualised conditional volatility: 17.78%.
- 99% VaR backtest: 29 violations; Kupiec p-value ≈ 0.3882.
- The analysis also diagnoses regime sensitivity when a short historical-simulation lookback excludes COVID-scale observations.

## R packages
See `R_packages.txt`.
