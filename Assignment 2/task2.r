# Required package to read excel
library("readxl")
library("ggplot2")

# Set seed
set.seed(42)

lagdf <- function(x, lagseq) {
    ## Return a data.frame
    tmp <- as.data.frame(do.call("cbind", lapply(lagseq, function(lag){
        return(lagvec(x, lag))
    })))
    names(tmp) <- paste0("k",lagseq)
    return(tmp)
}

# A simulation function for ARMA simulation, use model as arima.sim, i.e. flip sign of phi (into ar) coefficients
sim <- function(model, n, nburnin=100){
  n <- n + nburnin
  # Take the ar and ma part
  ar <- model$ar
  ma <- model$ma
  # The order (i.e. the number of lags)
  p <- length(ar)
  q <- length(ma)
  # The vector for the simulation result
  y <- numeric(n)
  # Generate the random normal values
  eps <- rnorm(n)
  # Run the simulation
  for(i in (max(p,q)+1):n){
    y[i] <- eps[i] + sum(y[i-(1:p)] * ar) + sum(eps[i-(1:q)] * ma)
  }
  # Return without the burn-in period
  return(y[(nburnin+1):n])
}

# Import data and set coefficients
solar <- read.csv("Assignment 2\\datasolar.csv")
solar <- as.vector(solar[['power']])
mu <- 5.72
phi1 <- -0.38
Phi1 <- -0.94
sigmaeps <- 0.22

# Plot solar data
N <- length(solar)
t <- seq(1, N, 1)
plot(t, solar, type = "l", xlab = "Time", ylab = "Power", main = "Solar Power")


### Q2.1 ###
# Redefine Y to X
X <- log(solar) - mu

# Plot X
plot(t, X, type = "l", xlab = "Time", ylab = "Power", main = "Solar Power")

# create epsilon
epsilon <- rnorm(N, mean = 0, sd = sigmaeps)

# Define AR model and run on historical data
xHat <- -phi1 * lagdf(X, 1) - Phi1 * lagdf(X, 12) - phi1 * Phi1 * lagdf(X, 13) + epsilon

# Plot 
plot.ts(t, xHat$k1, col = "red")
lines(t, X, col = "black")



epsHat <- X - xHat
epsHat <- na.omit(epsHat)
row.names(epsHat) <- NULL

sapply(epsHat, mean)
sapply(epsHat, var)
sapply(epsHat, sd)

qqnorm(epsHat$k1, ylab = "Error quantiles")
qqline(epsHat$k1)
acf(epsHat$k1, main = "ACF of the residuals")
pacf(epsHat$k1, main = "PACF of the residuals")



### Q2.2 ###

k <- 12
epsilon <- rnorm(k, mean = 0, sd = sigmaeps)
modelEst <- X

for (i in 1:k) {
    est <- -phi1 * modelEst[36+i-1] - Phi1 * modelEst[36+i-12] - phi1 * Phi1 * modelEst[36+i-13] + epsilon[i]
    modelEst <- c(modelEst, est)
}

# Plot
plotdata1 <- data.frame(t = seq(1, N, 1), X = X)
plotdata2 <- data.frame(t = seq(37, N+k, 1), modelEst = modelEst[-(1:36)])

ggplot() +
    geom_line(data = plotdata1, aes(x = t, y = X), color = "black") +
    geom_line(data = plotdata2, aes(x = t, y = modelEst), color = "red") +
    xlab("Time") +
    ylab("Power") +
    ggtitle("Solar Power")
