library(astsa)

# No. 19)

# a)
u1_acf <- ARMAacf(ar = c(0.7, -0.1), lag.max = 10)
plot(u1_acf)

u1_pacf <- ARMAacf(ar = c(0.7, -0.1), pacf = TRUE, lag.max = 10)
plot(u1_pacf)

z <- c(1, -0.7, 0.1)
a <- polyroot(z)
print(a)

# b)
u2_acf <- ARMAacf(ar = c(1.5, -0.75), lag.max = 40)
plot(u2_acf)

u2_pacf <- ARMAacf(ar = c(1.5, -0.75), pacf = TRUE, lag.max = 10)
plot(u2_pacf)

z <- c(1, -1.5, 0.75)
a <- polyroot(z)
print(a)

# c)
u3_acf <- ARMAacf(ar = c(1, -0.25), lag.max = 10)
plot(u3_acf)

u3_pacf <- ARMAacf(ar = c(1, -0.25), pacf = TRUE, lag.max = 10)
plot(u3_pacf)

z <- c(1, -1, 0.25)
a <- polyroot(z)
print(a)


# No. 20)
# a)
ma_1 <- ARMAtoMA(ar = 0.9, ma = 0.5, lag.max = 10)
plot(ma_1)

# hier alternativ durch Vertauschen mit ARMAtoMA
ar_1 <- ARMAtoAR(ar = 0.9, ma = 0.5, lag.max = 10)
plot(ar_1)

# No. 21)
lag <- 20
acf_1 <- ARMAacf(ar = 0.6, ma = 0.9, lag.max = lag)
acf_2 <- ARMAacf(ar = 0.6, lag.max = lag)
acf_3 <- ARMAacf(ma = 0.9, lag.max = lag)

plot(acf_1, main = 'ACF')
points(acf_2, col = 'red')
points(acf_3, col = 'blue')

pacf_1 <- ARMAacf(ar = 0.6, ma = 0.9, lag.max = lag, pacf = TRUE)
pacf_2 <- ARMAacf(ar = 0.6, lag.max = lag, pacf = TRUE)
pacf_3 <- ARMAacf(ma = 0.9, lag.max = lag, pacf = TRUE)

plot(pacf_1, main = 'PACF')
points(pacf_2, col = 'red')
points(pacf_3, col = 'blue')

# Simulation
sim_1 <- arima.sim(model = list(ar = 0.6, ma = 0.9), n = 100)
acf_1_empirical <- acf(sim_1)
pacf_1_empirical <- acf(sim_1, type = 'empirical')

sim_2 <- arima.sim(model = list(ar = 0.6), n = 100)
acf_2_empirical <- acf(sim_2)
pacf_2_empirical <- acf(sim_2, type = 'empirical')

plot(acf_1_empirical$acf)
lines(acf_1)
points(acf_2_empirical$acf, col = 'red')
lines(acf_2, col = 'red')

# No.22
ARMA_simulation <- function(ar_coefficients, ma_coefficients, n) {
  results <- numeric(n+2)
  noise <- numeric(length(results))
  for (i in 3:length(results)) {
    noise_term <- rnorm(1)
    ar_term <- t(ar_coefficients) %*% results[(i-length(ar_coefficients)):(i-1)]
    ma_term <- t(ma_coefficients) %*% noise[(i-length(ma_coefficients)):(i-1)]
    results[i] <- ar_term + noise_term
    noise[i] <- noise_term
  }
  return(results[3:length(results)])
}

print(ARMA_simulation(c(0.5, 0.25), n = 10))
