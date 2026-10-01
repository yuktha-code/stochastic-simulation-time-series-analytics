

#UNDERSTANDING THE NORMAL DISTRIBUTION (RANDOM VARIABLE)


# We begin by exploring the normal distribution, which is the foundation
# of continuous-time finance and GBM.

set.seed(123)   # Ensures results are reproducible

n <- 1000

# Generate 100,000 standard normal random variables (mean = 0, sd = 1)
z <- rnorm(100000)

# Plot histogram and overlay estimated density
hist(z, prob = TRUE, 
     main = "Standard Normal Distribution",
     col = "lightblue")

lines(density(z), lty = 1, col = "red", lwd = 3)  
# The red curve is the kernel density estimate of the sample



# Different normal distributions with same mean but diffferent variances


n <- 1000

# Base case: mean = 3, sd = 2
z <- rnorm(n, mean = 3, sd = 2)

hist(z, prob = TRUE,
     main = "Normal Distributions with Different Volatility",
     col = "lightblue")

lines(density(z), col = "red", lwd = 3)  # base distribution (sd = 2)

# Overlay other densities with same mean but different volatilities
lines(density(rnorm(n, 3, 1)), col = "blue", lwd = 2)   # lower risk
lines(density(rnorm(n, 3, 3)), col = "green", lwd = 2)  # higher risk
lines(density(rnorm(n, 3, 5)), col = "purple", lwd = 2) # very high risk

legend("topright",
       legend = c("sd=2 (base)", "sd=1", "sd=3", "sd=5"),
       col = c("red","blue","green","purple"),
       lwd = c(3,2,2,2), lty = 1, bty = "n")

# Interpretation:
# - Mean controls location (expected return)
# - Standard deviation controls spread (risk/volatility)


# Visualising how sample size affects approximation to normal density


hist(z, breaks = 30, probability = TRUE,
     main = "Random Samples from Standard Normal",
     col = "lightblue")

curve(dnorm(x, mean = 0, sd = 1),
      col = "red", lwd = 2, add = TRUE)

# Value of normal density at x = 0
dnorm(0, 0, 1)
# observe the value at console


#RANDOM WALK (discrete time walk)


set.seed(123)

n <- 200
epsilon <- rnorm(n)   # independent normal shocks

# Random walk: cumulative sum of shocks
random_walk <- cumsum(epsilon)

plot(random_walk, type = "l",
     main = "Random Walk",
     ylab = "Value", xlab = "Time")

# Financial intuition:
# A random walk represents a price that moves up/down randomly
# without any drift (no systematic trend).



# WIENER PROCESS (CONTINUOUS-TIME LIMIT OF RANDOM WALK)


set.seed(123)

T <- 1       # total time horizon (1 year or 1 unit of time)
N <- 500     # number of time steps
dt <- T / N  # time step size

# Generate standard normal shocks
Z <- rnorm(N)

# Construct Wiener process by cumulative sum of scaled shocks
W <- cumsum(sqrt(dt) * Z)

# Plot single Wiener path
plot(seq(0, T, length.out = N),
     W, type = "l",
     main = "Wiener Process (Brownian Motion)",
     xlab = "Time", ylab = "W(t)")

# Interpretation:
# - Starts at zero
# - Continuous but highly irregular
# - Variance increases with time



# Multiple Wiener Paths (scenario analysis)


n_sim <- 10  # number of simulated paths

W_mat <- matrix(0, nrow = N, ncol = n_sim)

for (i in 1:n_sim) {
  W_mat[, i] <- cumsum(sqrt(dt) * rnorm(N))
}

matplot(seq(0, T, length.out = N),
        W_mat, type = "l", lty = 1,
        col = rgb(0, 0, 1, 0.4),
        main = "Multiple Wiener Paths",
        xlab = "Time", ylab = "W(t)")

# Each line is one possible realization of randomness



# EMPIRICAL RETURNS AND GBM ESTIMATION

library(quantmod)

# Download real stock data (TCS from NSE)
getSymbols("TCS.NS", src = "yahoo")

# Extract adjusted closing prices
price <- Ad(TCS.NS)

plot(price, main = "TCS Daily Prices")

# Compute daily log returns
returns <- diff(log(price))
returns <- na.omit(returns)

# Estimate daily drift and volatility from data
mu  <- mean(returns)   # expected daily return
sig <- sd(returns)     # daily volatility

# Plot distribution of returns
hist(returns, prob = TRUE, col = "lightgray", border = "white",
     main = "TCS Daily Log Returns",
     xlab = "Log Return")

lines(density(returns), col = "red", lwd = 3)



#  AR(1) MODEL FOR RETURNS (ALTERNATIVE TO GBM)


ar1_model <- arima(returns, order = c(1, 0, 0))
print(ar1_model)

set.seed(123)

n_days <- 200
sim_returns_ar1 <- numeric(n_days)

phi <- ar1_model$coef["ar1"]
c   <- ar1_model$coef["intercept"]
sig <- sqrt(ar1_model$sigma2)

sim_returns_ar1[1] <- tail(returns, 1)

for (t in 2:n_days) {
  sim_returns_ar1[t] <- c +
    phi * sim_returns_ar1[t - 1] +
    sig * rnorm(1)
}

plot(sim_returns_ar1, type = "l",
     main = "Simulated TCS Returns using AR(1)",
     ylab = "Return", xlab = "Days Ahead")

S0 <- as.numeric(last(price))

# Convert simulated returns into price path
price_ar1 <- S0 * exp(cumsum(sim_returns_ar1))

plot(price_ar1, type = "l",
     main = "Price Path Implied by AR(1) Returns",
     ylab = "Price", xlab = "Days Ahead")



# GBM SIMULATION (DELTA FORM)


# Fix the random seed so that results are reproducible
# This ensures every student gets the same simulated paths
set.seed(123)

# Starting stock price today (S_0)
# We take the most recent observed price from the data
S0 <- as.numeric(last(price))

# Number of days to simulate into the future
n_days <- 200

# Number of Monte Carlo simulation paths
# Each column will represent one possible future price path
n_sim  <- 100

# Time step: 1 day (since we are working with daily returns)
dt <- 1

# Create a matrix to store simulated prices
# Rows = time (0 to 200), Columns = simulation paths (1 to 100)
sim_prices_delta <- matrix(NA, nrow = n_days + 1, ncol = n_sim)

# Set the initial price at time 0 for all simulations
# Every simulated path starts from the same current market price
sim_prices_delta[1, ] <- S0

# Begin Monte Carlo simulation of GBM using Euler–Maruyama (delta form)

# Outer loop: repeat for each simulation path
for (i in 1:n_sim) {
  
  # Inner loop: simulate day-by-day price evolution
  for (t in 2:(n_days + 1)) {
    
    # Generate one standard normal random shock
    # This represents unpredictable market news on day t
    eps <- rnorm(1)
    
    # Discrete GBM update (delta form):
    # S_{t} = S_{t-1} 
    #        + drift component (expected growth)
    #        + volatility component (random risk)
    
    sim_prices_delta[t, i] <- sim_prices_delta[t - 1, i] +
      mu  * sim_prices_delta[t - 1, i] * dt +              # drift: μ S_t dt
      sig * sim_prices_delta[t - 1, i] * sqrt(dt) * eps   # randomness: σ S_t dW_t
  }
}

# Plot all simulated price paths together
# Each thin blue line is one possible future under GBM assumptions
matplot(sim_prices_delta, type = "l", lty = 1,
        col = rgb(0, 0, 1, 0.3),  # semi-transparent blue
        main = paste(n_days, "Day Simulation using Delta S_t"),
        xlab = "Days", ylab = "Price")

# Highlight 5 randomly selected simulation paths for clarity
set.seed(1)
idx <- sample(1:n_sim, 5)

matlines(sim_prices_delta[, idx],
         col = c("red", "blue", "green", "purple", "orange"),
         lwd = 2)

# Extract the final simulated prices at day 200
# This gives the distribution of possible prices at the horizon
final_prices <- sim_prices_delta[n_days + 1, ]

# Compute the average (expected) price across all simulations
mean_price   <- mean(final_prices)

# Compute the median price across all simulations
# Median is often more robust than mean when distribution is skewed
median_price <- median(final_prices)

# Display results
mean_price
median_price

# Compare with the last observed real price in data
last(price)



# GBM USING sde PACKAGE (THEORETICAL FORM)


library(sde)

gbm <- GBM(x = S0, 
           r = mu * 252, 
           sigma = sig * sqrt(252),
           T = 1, N = 252)

plot(gbm, type = "l",
     main = "GBM using sde Package",
     xlab = "Time", ylab = "Stock Price")





# S PORTFOLIO MONTE CARLO SIMULATION


# Load package to download financial data from Yahoo Finance
library(quantmod)

# Specify the four Indian stocks we want to include in the portfolio
# These will be the assets whose joint risk we are studying
stocks <- c("TCS.NS", "INFY.NS", "RELIANCE.NS", "HDFCBANK.NS")

# Download historical price data for all four stocks
# Each ticker will be stored as an xts time-series object in R
getSymbols(stocks, src = "yahoo")

# Extract Adjusted Closing Prices for each stock
# 'lapply' applies the same function to each stock in the list
# Ad(get(s)) pulls the adjusted price series for stock 's'
prices <- lapply(stocks, function(s) Ad(get(s)))

# Compute daily log returns for each stock
# diff(log(p)) gives log returns: ln(P_t / P_{t-1})
# na.omit() removes the first NA value created by differencing
returns <- lapply(prices, function(p) na.omit(diff(log(p))))

# Estimate daily mean return (drift) for each stock
# This will be used as the expected growth rate in the GBM simulation
mu  <- sapply(returns, mean)

# Estimate daily volatility (standard deviation of returns) for each stock
# This represents risk for each asset
sig <- sapply(returns, sd)

# Define total initial investment in the portfolio
V0 <- 100000   # ₹1,00,000 total capital

# Define portfolio weights
# These represent how the total investment is allocated across assets
# w1 = 30% in TCS, w2 = 25% in INFY, etc.
W <- c(w1 = 0.30,
       w2 = 0.25,
       w3 = 0.25,
       w4 = 0.20)

# Check that weights sum to 1 (i.e., 100% of capital is invested)
sum(W)

# Extract the most recent (current) price of each stock
# This will be our starting price S0 for simulation
S0 <- sapply(prices, function(p) as.numeric(last(p)))

# Compute how much money is invested in each stock
# For example, if V0 = 100,000 and weight = 0.30, then 30,000 goes to that stock
investment_per_stock <- V0 * W

# Convert money invested into number of shares held
# shares = money invested / current stock price
# This fixes the portfolio holdings before simulation
shares <- investment_per_stock / S0

# Set seed for reproducibility of Monte Carlo simulation
set.seed(123)

# Number of days to simulate into the future (around 2 months)
n_days <- 40

# Number of Monte Carlo simulation paths
n_sim  <- 100

# Number of assets in the portfolio (should be 4 here)
n_assets <- length(stocks)

# Create a 3-dimensional array to store simulated prices:
# Dimension = (time) x (simulation) x (asset)
sim_prices <- array(NA, dim = c(n_days + 1, n_sim, n_assets))

# Initialize the first row (time 0) with today's prices for all simulations
# This ensures all paths start from the same current market price
for (j in 1:n_assets) {
  sim_prices[1, , j] <- S0[j]
}

# Time step = 1 day (discrete daily simulation)
dt <- 1

# Simulate future stock prices using GBM (Euler–Maruyama / delta form)
# Outer loop: over each asset
for (j in 1:n_assets) {
  
  # Middle loop: over each Monte Carlo simulation path
  for (i in 1:n_sim) {
    
    # Inner loop: over each future day
    for (t in 2:(n_days + 1)) {
      
      # Generate a standard normal random shock
      eps <- rnorm(1)
      
      # Discrete GBM update:
      # New price = old price + drift component + volatility component
      sim_prices[t, i, j] <- sim_prices[t - 1, i, j] +
        mu[j]  * sim_prices[t - 1, i, j] * dt +            # expected growth
        sig[j] * sim_prices[t - 1, i, j] * sqrt(dt) * eps # random shock (risk)
    }
  }
}

# Create a matrix to store portfolio value for each day and each simulation
# Rows = time, Columns = simulation paths
portfolio_value <- matrix(0, nrow = n_days + 1, ncol = n_sim)

# Compute portfolio value at each time step and simulation
# Portfolio value = sum of (shares held * simulated stock price)
for (t in 1:(n_days + 1)) {
  for (i in 1:n_sim) {
    portfolio_value[t, i] <- sum(shares * sim_prices[t, i, ])
  }
}

# Create a time index from day 0 to day 40
time_index <- 0:n_days

# Plot all simulated portfolio value paths
# Each line represents one possible future scenario
matplot(time_index, portfolio_value,
        type = "l", lty = 1,
        col = rgb(0, 0, 0.7, 0.2),  # semi-transparent blue lines
        main = "Portfolio Value Simulation (₹)",
        xlab = "Days Ahead",
        ylab = "Portfolio Value")

# Highlight 5 randomly selected simulation paths for clarity
set.seed(1)
idx <- sample(1:n_sim, 5)

matlines(time_index, portfolio_value[, idx],
         col = c("red", "blue", "green", "purple", "orange"),
         lwd = 2)

# Compute mean portfolio value across all simulations at each day
portfolio_mean   <- apply(portfolio_value, 1, mean)

# Compute median portfolio value across simulations at each day
portfolio_median <- apply(portfolio_value, 1, median)

# Overlay mean and median on the same plot
lines(time_index, portfolio_mean, col = "red", lwd = 2)
lines(time_index, portfolio_median, col = "darkgreen", lwd = 2)

# Add legend to interpret the plot
legend("topleft",
       legend = c("Simulated Paths", "Mean", "Median"),
       col = c("blue", "red", "darkgreen"),
       lwd = c(1, 2, 2))

# Extract portfolio value at different horizons:
# Day 0 = today
# Day 20 ≈ 1 month
# Day 40 ≈ 2 months
rbind(
  day0   = c(mean = portfolio_mean[1],  median = portfolio_median[1]),
  month1 = c(mean = portfolio_mean[21], median = portfolio_median[21]),
  month2 = c(mean = portfolio_mean[41], median = portfolio_median[41])
)

