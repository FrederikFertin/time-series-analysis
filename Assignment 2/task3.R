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

plotting <- function(x) {
  # Set up the layout
  layout(matrix(c(1, 1,
                  2, 3), nrow = 2, byrow = TRUE), heights = c(1,1,1))
  
  # Plot the time series on the first plot
  par(mar = c(4, 4, 4, 4)) # Adjust margin for the first plot
  plot(x, type = "l", main = "Time Series")
  
  # ACF plot on the second plot
  par(mar = c(4, 4, 4, 4)) # Adjust margin for the second plot
  acf(x, lag.max = 30, main = "ACF")
  
  # PACF plot on the third plot
  par(mar = c(4, 4, 4, 4)) # Adjust margin for the third plot
  pacf(x, lag.max = 30, main = "PACF")
}

set.seed(1)
l <- 200

## Task 3.1
model <- list(ar=c(0.6))
x <- arima.sim(model, l, n.start=100)
plotting(x)

## Task 3.2
model <- list(ar=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.9))
x <- arima.sim(model, l, n.start=100)
plotting(x)

## Task 3.3
model <- list(ar=c(0.9), ma=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.7))
x <- arima.sim(model, l, n.start=100)
plotting(x)

## Task 3.4
model <- list(ar=c(-0.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.8))
# x <- arima.sim(model, 100, n.start=100)
x <- sim(model, l, nburnin=100)
plotting(x)

## Task 3.5
model <- list(ma=c(0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.8))
x <- arima.sim(model, l, n.start=100)
plotting(x)

## Task 3.6
model <- list(ar=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.7), ma=c(-0.4))
x <- arima.sim(model, l, n.start=100)
plotting(x)
