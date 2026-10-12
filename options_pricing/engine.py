"""European Black-Scholes-Merton pricing, Greeks and expiry strategy P&L.

Analytical study only: no market-data feed, contract lots, brokerage, margin or
validated exchange profits. Dividend yield is continuous in the pricing model.
"""
from dataclasses import dataclass
from math import exp, isfinite, log, sqrt
from typing import Literal

import numpy as np
from scipy.stats import norm

OptionKind = Literal["call", "put"]


@dataclass(frozen=True)
class BSM:
    spot: float
    strike: float
    years: float
    rate: float
    volatility: float
    dividend_yield: float = 0.0

    def __post_init__(self):
        vals = (self.spot, self.strike, self.years, self.rate,
                self.volatility, self.dividend_yield)
        if not all(isfinite(float(v)) for v in vals):
            raise ValueError("Pricing inputs must be finite numbers")
        if min(self.spot, self.strike, self.years, self.volatility) <= 0:
            raise ValueError("Spot, strike, time and volatility must be positive")

    @property
    def d1(self):
        return (log(self.spot / self.strike) +
                (self.rate - self.dividend_yield + self.volatility**2 / 2) * self.years) / (
                    self.volatility * sqrt(self.years))

    @property
    def d2(self):
        return self.d1 - self.volatility * sqrt(self.years)

    def price(self, kind: OptionKind) -> float:
        if kind == "call":
            return float(self.spot * exp(-self.dividend_yield * self.years) * norm.cdf(self.d1)
                         - self.strike * exp(-self.rate * self.years) * norm.cdf(self.d2))
        if kind == "put":
            return float(self.strike * exp(-self.rate * self.years) * norm.cdf(-self.d2)
                         - self.spot * exp(-self.dividend_yield * self.years) * norm.cdf(-self.d1))
        raise ValueError("Option kind must be 'call' or 'put'")

    def greeks(self, kind: OptionKind) -> dict[str, float]:
        if kind not in ("call", "put"):
            raise ValueError("Invalid option kind")
        discount_q = exp(-self.dividend_yield * self.years)
        discount_r = exp(-self.rate * self.years)
        phi = norm.pdf(self.d1)
        t = self.years
        common_theta = -(self.spot * discount_q * phi * self.volatility) / (2 * sqrt(t))
        if kind == "call":
            delta = discount_q * norm.cdf(self.d1)
            theta_y = (common_theta - self.rate * self.strike * discount_r * norm.cdf(self.d2)
                       + self.dividend_yield * self.spot * discount_q * norm.cdf(self.d1))
            rho_y = self.strike * t * discount_r * norm.cdf(self.d2)
        else:
            delta = discount_q * (norm.cdf(self.d1) - 1)
            theta_y = (common_theta + self.rate * self.strike * discount_r * norm.cdf(-self.d2)
                       - self.dividend_yield * self.spot * discount_q * norm.cdf(-self.d1))
            rho_y = -self.strike * t * discount_r * norm.cdf(-self.d2)
        return {
            "delta": float(delta),
            "gamma": float(discount_q * phi / (self.spot * self.volatility * sqrt(t))),
            "theta_per_calendar_day": float(theta_y / 365),
            "vega_per_1pct_vol": float(self.spot * discount_q * phi * sqrt(t) / 100),
            "rho_per_1pct_rate": float(rho_y / 100),
        }


@dataclass(frozen=True)
class OptionLeg:
    kind: OptionKind
    strike: float
    side: int  # +1 buy; -1 sell
    quantity: int = 1  # per underlying unit; exchange lot size not applied

    def __post_init__(self):
        if (self.kind not in ("call", "put") or self.side not in (-1, 1)
                or not isfinite(float(self.strike)) or self.strike <= 0
                or not isinstance(self.quantity, int) or self.quantity <= 0):
            raise ValueError("Invalid option leg specification")


def expiry_profit(legs: list[OptionLeg], terminal_spot, *,
                  initial_spot: float = 100, years: float = 0.5,
                  rate: float = 0.05, volatility: float = 0.25,
                  stock_quantity: int = 0):
    """Expiry profit per underlying unit, based on premiums at initiation.

    +long option = intrinsic payoff MINUS premium; short is opposite.
    A long stock position gains terminal - initial spot. Financing, dividends,
    bid-ask spreads, taxes, commissions, margin and assignment are excluded.
    """
    s = np.asarray(terminal_spot, dtype=float)
    if not np.all(np.isfinite(s)) or np.any(s < 0):
        raise ValueError("Terminal prices must be nonnegative finite values")
    if not isfinite(initial_spot) or initial_spot <= 0:
        raise ValueError("Initial spot must be positive")
    pnl = np.zeros_like(s) + stock_quantity * (s - initial_spot)
    for leg in legs:
        model = BSM(initial_spot, leg.strike, years, rate, volatility)
        premium = model.price(leg.kind)
        intrinsic = (np.maximum(s - leg.strike, 0) if leg.kind == "call"
                     else np.maximum(leg.strike - s, 0))
        pnl += leg.side * leg.quantity * (intrinsic - premium)
    return float(pnl) if pnl.ndim == 0 else pnl


def sample_strategies() -> dict[str, tuple[list[OptionLeg], int]]:
    """Illustrative strikes only; NOT observed market quotes or Kotak data."""
    leg = OptionLeg
    return {
        "bull_call_spread": ([leg("call", 95, 1), leg("call", 105, -1)], 0),
        "bear_put_spread": ([leg("put", 105, 1), leg("put", 95, -1)], 0),
        "long_straddle": ([leg("call", 100, 1), leg("put", 100, 1)], 0),
        "short_straddle": ([leg("call", 100, -1), leg("put", 100, -1)], 0),
        "long_strangle": ([leg("call", 110, 1), leg("put", 90, 1)], 0),
        "long_call_butterfly": ([leg("call", 90, 1), leg("call", 100, -1, 2), leg("call", 110, 1)], 0),
        "covered_call": ([leg("call", 110, -1)], 1),
        "protective_put": ([leg("put", 90, 1)], 1),
    }
