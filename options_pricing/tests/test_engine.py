import numpy as np
import pytest

from options_pricing.engine import BSM, OptionLeg, expiry_profit, sample_strategies


def test_classic_textbook_call_put():
    m = BSM(100, 100, 1, 0.05, 0.2)
    assert m.price('call') == pytest.approx(10.45058357, abs=1e-7)
    assert m.price('put') == pytest.approx(5.57352602, abs=1e-7)


def test_discounted_put_call_parity_with_dividends():
    m = BSM(123, 115, 0.75, 0.04, 0.31, 0.02)
    expected = 123 * np.exp(-0.02 * 0.75) - 115 * np.exp(-0.04 * 0.75)
    assert m.price('call') - m.price('put') == pytest.approx(expected, abs=1e-10)


def test_call_and_put_greeks_finite_differences():
    m = BSM(100, 97, 0.8, 0.05, 0.24, 0.015)
    eps = 1e-3
    for kind in ('call', 'put'):
        g = m.greeks(kind)
        up = BSM(100 + eps, 97, 0.8, 0.05, 0.24, 0.015)
        down = BSM(100 - eps, 97, 0.8, 0.05, 0.24, 0.015)
        assert g['delta'] == pytest.approx((up.price(kind) - down.price(kind)) / (2 * eps), rel=1e-5)
        assert g['gamma'] == pytest.approx((up.price(kind) - 2 * m.price(kind) + down.price(kind)) / eps**2, rel=1e-4)
        vol_up = BSM(100, 97, 0.8, 0.05, 0.24 + eps, 0.015)
        vol_down = BSM(100, 97, 0.8, 0.05, 0.24 - eps, 0.015)
        assert g['vega_per_1pct_vol'] == pytest.approx((vol_up.price(kind) - vol_down.price(kind)) / (2*eps*100), rel=1e-5)


def test_long_call_below_strike_loses_paid_premium():
    leg = OptionLeg('call', 100, 1)
    premium = BSM(100, 100, 0.5, 0.05, 0.25).price('call')
    assert expiry_profit([leg], 70) == pytest.approx(-premium)
    assert expiry_profit([OptionLeg('call', 100, -1)], 70) == pytest.approx(+premium)


def test_bull_call_spread_bounded_at_expiry():
    legs, stock = sample_strategies()['bull_call_spread']
    values = expiry_profit(legs, np.array([0., 95., 100., 105., 150., 300.]), stock_quantity=stock)
    assert values[0] == pytest.approx(values[1])
    assert values[-1] == pytest.approx(values[-2])
    assert values[-1] - values[0] == pytest.approx(10)


def test_butterfly_peak_and_tail_bounded():
    legs, stock = sample_strategies()['long_call_butterfly']
    pnl = expiry_profit(legs, np.array([60, 90, 100, 110, 140.]), stock_quantity=stock)
    assert pnl[0] == pytest.approx(pnl[1])
    assert pnl[-1] == pytest.approx(pnl[0])
    assert pnl[2] - pnl[0] == pytest.approx(10)


def test_stock_strategy_loss_at_terminal_zero():
    covered, shares = sample_strategies()['covered_call']
    assert shares == 1
    premium = BSM(100, 110, 0.5, 0.05, 0.25).price('call')
    assert expiry_profit(covered, 0, stock_quantity=shares) == pytest.approx(-100 + premium)


def test_invalid_inputs_rejected():
    with pytest.raises(ValueError): BSM(100, 100, 0, 0.05, 0.2)
    with pytest.raises(ValueError): BSM(-1, 100, 1, 0.05, 0.2)
    with pytest.raises(ValueError): OptionLeg('call', 100, 0)
    with pytest.raises(ValueError): BSM(100, 100, 1, 0.05, 0.2).price('invalid')


def test_all_strategies_evaluable_no_nan():
    for legs, shares in sample_strategies().values():
        values = expiry_profit(legs, np.linspace(0, 200, 21), stock_quantity=shares)
        assert np.all(np.isfinite(values))
