# Required package to read excel
library("readxl")
library("ggplot2")

# Set seed
set.seed(42)

# Define lag functions
lagvec <- function(x, lag){
    if (lag > 0) {
        ## Lag x, i.e. delay x lag steps
        return(c(rep(NA, lag), x[1:(length(x) - lag)]))
    }else if(lag < 0) {
        ## Lag x, i.e. delay x lag steps
        return(c(x[(abs(lag) + 1):length(x)], rep(NA, abs(lag))))
    }else{
        ## lag = 0, return x
        return(x)
    }
}

lagdf <- function(x, lagseq) {
    ## Return a data.frame
    tmp <- as.data.frame(do.call("cbind", lapply(lagseq, function(lag){
        return(lagvec(x, lag))
    })))
    names(tmp) <- paste0("k",lagseq)
    return(tmp)
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
t <- seq(2008, 2008+(N-1)/12, 1/12)
plot(t, solar, type = "l", xlab = "Time", ylab = "Power", main = "Solar Power")


### Q2.1 ###
# Redefine Y to X
X <- log(solar) - mu

# Plot X
plot(t, X, type = "l", xlab = "Time", ylab = "Power", main = "Solar Power")

# create epsilon
epsilon <- rnorm(N, mean = 0, sd = sigmaeps)

# Define AR model and run on historical data
xHat <- -phi1 * lagdf(X, 1) - Phi1 * lagdf(X, 12) - phi1 * Phi1 * lagdf(X, 13) 


# Plot 
plot.ts(t, xHat$k1, col = "red", ylab = "log(power)-mu", xlab = "Year", xy.labels = FALSE, xy.lines = TRUE, xaxt = "n") # xaxt = "n" to suppress the x-axis
lines(t, X, col = "black")
axis(1, at = floor(min(t)):ceiling(max(t))) # Add this line to create a new x-axis with only integer values


# Compute mean, variance and standard deviation of the residuals
epsHat <- X - xHat
epsHat <- na.omit(epsHat)
row.names(epsHat) <- NULL

sapply(epsHat, mean)
sapply(epsHat, var)
sapply(epsHat, sd)

# Check IID. Plot histogram and pointplot
hist(epsHat$k1, main ="", xlab = "Residuals")

# Plot epsHat as a pointplot versus the index
ggplot(data = epsHat, aes(x = seq_along(k1), y = k1)) +
    geom_point() +
    xlab("Index") +
    ylab("residual") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

# Autocorrelation
acf(epsHat$k1, main = "")



### Q2.2 ###

k <- 12
epsilon <- rnorm(k, mean = 0, sd = sigmaeps)
modelEst <- X

for (i in 1:k) {
    est <- -phi1 * modelEst[36+i-1] - Phi1 * modelEst[36+i-12] - phi1 * Phi1 * modelEst[36+i-13]
    modelEst <- c(modelEst, est)
}
# Plot log power
modelEst <- modelEst[-(1:36)]
plotdata1 <- data.frame(t = t, X = X)
plotdata2 <- data.frame(t = seq(2011, 2011+(k-1)/12, 1/12), modelEst = modelEst)
ggplot() +
    geom_line(data = plotdata1, aes(x = t, y = X), color = "black") +
    geom_line(data = plotdata2, aes(x = t, y = modelEst), color = "red") +
    xlab("Time") +
    ylab("Power") +
    ggtitle("Log Solar Power")

# Plot actual power
solarEst <- exp(modelEst + mu)
plotdata1 <- data.frame(t = t, solar = solar)
plotdata2 <- data.frame(t = seq(2011, 2011+(k-1)/12, 1/12), modelEst = solarEst)
ggplot() +
    geom_line(data = plotdata1, aes(x = t, y = solar), color = "black") +
    geom_line(data = plotdata2, aes(x = t, y = modelEst), color = "red") +
    xlab("Time") +
    ylab("Generation [MWh]") +
    ggtitle("Solar Power") +
    theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))


### Q2.3 ###

# Compute CI
alpha <- 0.05
z <- qnorm(1 - alpha/2)

vars <- list()
for (i in 1:k) {
    s <- 0
    for (j in 1:i) {
        s <- s + phi1^(2*(i-j))
    }
    vars[i] <-  s
}
vars <- lapply(vars, "*", sigmaeps^2)
vars <- unlist(vars)

# Compute CI
CI <- cbind(modelEst - z * sqrt(vars), modelEst + z * sqrt(vars)) 

# Plot historical data, model and CI
plotdata1 <- data.frame(t = t, solar = solar)
plotdata2 <- data.frame(t = seq(2011, 2011+(k-1)/12, 1/12), modelEst = exp(modelEst + mu))
plotdata3 <- data.frame(t = seq(2011, 2011+(k-1)/12, 1/12), CI1 = exp(CI[,1] + mu), CI2 = exp(CI[,2] + mu))
ggplot() +
    geom_line(data = plotdata1, aes(x = t, y = solar), color = "black") +
    geom_line(data = plotdata2, aes(x = t, y = modelEst), color = "red") +
    geom_ribbon(data = plotdata3, aes(x = t, ymin = CI1, ymax = CI2), fill = "blue", alpha = 0.2) +
    xlab("Time") +
    ylab("Power") +
    ggtitle("Solar Power") +
    theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

