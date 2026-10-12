# European Options Pricing and Strategy Payoff Validation

This module refactors a supplied interactive **Black–Scholes–Merton options notebook** into a testable, numerically validated, standalone European-options research implementation. It is an *additional component* to the Reliance risk/time-series repository, not a claim of live trading or a historical Kotak Mahindra strategy backtest.

## Implemented
- European call and put prices using spot, strike, time, continuously compounded risk-free rate, volatility and optional continuous dividend yield.
- Put–call parity and Delta/Gamma/Theta/Vega/Rho; Theta is per calendar day, Vega and Rho are per **1 percentage-point** parameter shift.
- Eight illustrative expiry payoff structures: bull call, bear put, long/short straddle, long strangle, long call butterfly, covered call and protective put.
- Correct sign convention for strategy initiation premiums: **long options pay premiums; short options receive them**. The source notebook had incorrectly added the premium on long positions, which this module fixes.
- Parameter guards and regression tests, including finite-difference Greek checks.

## Run
```bash
python -m pip install numpy scipy pytest
python -m pytest options_pricing/tests -q
```

Example:
```python
from options_pricing.engine import BSM, expiry_profit, sample_strategies
m = BSM(100, 100, 1, 0.05, 0.20)
print(m.price('call'), m.greeks('call'))
legs, shares = sample_strategies()['bull_call_spread']
print(expiry_profit(legs, [90, 100, 110], stock_quantity=shares))
```

**Limitations:** All numerical inputs are illustrative. No observed market quotes, Kotak-specific contracts, volatility-surface calibration, actual fills, contract lot sizes, corporate actions, early exercise, transaction fees, dividends received from stock legs, stock financing/margin, or empirical profitability. Analytic formula prices assume European exercise and lognormal underlying dynamics; Indian equity stock options may differ in their market conventions and historical contractual terms.
