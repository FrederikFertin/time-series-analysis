
library("ggplot2")

### 1.1
p <- c(1, -0.7, -0.2)
polyroot(p)

### 1.4
# Using the autocorrelation function 
N <- 30
rho <- rep(NA, N)
phi <- c(-0.7, -0.2)
rho[1] <- -phi[1] / (1 + phi[2])
rho[2] <- - phi[2] - phi[1]*rho[1] 
for (k in 3:N) {
  rho[k] <- - phi[1] * rho[k-1] - phi[2] * rho[k-2]
}
barplot(rho, c(1:30), width=0.01, axes=TRUE)

### 1.5
# Using arima.sim and acf
model <- list(ar=c(0.7, 0.2))
x <- arima.sim(model, 200, n.start=100)
acf(x, lag.max=30)


lpot <- ggplot()
for (i in 1:5) {
  set.seed(i)
  x[i] <- arima.sim(model, 200, n.start=100)
  
}  

set.seed(1)
x <- arima.sim(model, 200, n.start=100)
lpot <- plot(x)
for (i in 2:5) {
  set.seed(i)
  x <- arima.sim(model, 200, n.start=100)
  lpot <- lpot + plot(x)
}
print(lpot)

x1 <- arima.sim(model, 200, n.start=100)
x2 <- arima.sim(model, 200, n.start=100)
x3 <- arima.sim(model, 200, n.start=100)
x4 <- arima.sim(model, 200, n.start=100)
x5 <- arima.sim(model, 200, n.start=100)
#plot(x1,x2,x3,x4,x5)



