# Required package to read excel
library("readxl")
library("ggplot2")

### DATA IMPORT ###
# Training Data
# my_data <- read_excel("Assignment 1/DST_BIL54_train.xlsx")
my_data <- read_excel("/Users/thomastrosborg/Desktop/Uni/Master/1. semester/Time Series Analysis/git/Assignment 1/DST_BIL54_train.xlsx")
n_train <- length(my_data)
train <- unlist(my_data[1, 2:n_train])
x <- seq(2018, 2022 + 10 / 12, by = 1 / 12)
n <- length(x)
y_train <- train

# Test Data
# my_data <- read_excel("Assignment 1/DST_BIL54_test.xlsx")
my_data <- read_excel("/Users/thomastrosborg/Desktop/Uni/Master/1. semester/Time Series Analysis/git/Assignment 1/DST_BIL54_test.xlsx")
n_test <- length(my_data)
test <- unlist(my_data[1, 2:n_test])


##### Q1 #####
plot(x, train, col = "blue", title("Training data"), xlab="Year", ylab="Number of Vehicles")

plot(2:n_test, test, col = "red", title("Test data"), xlab="Year", ylab="Number of Vehicles")




##### Q2 #####
# Q2.1
ones <- rep(1, length(x))
x_ols <- cbind(ones, x)
theta <- solve(t(x_ols) %*% x_ols) %*% t(x_ols) %*% train

y_hat <- x_ols %*% theta
#plot(x, y_hat, col = "blue", title("Training data - OLS estimate"))
print(theta)

# Error, squared errors and residual variance estimate
error <- train - y_hat
RSS_ols <- t(error) %*% error
sigma2 <- as.numeric(RSS_ols) / (n_train - length(theta))

# Variance and std of estimated errors
v_ols <- sigma2 * solve(t(x_ols) %*% x_ols)
s_ols <- sqrt(diag(v_ols))

# Make forecast for next 12 months
x_test <- seq(2022 + 11 / 12, 2023 + 10 / 12, by = 1 / 12)
x_forecast <- cbind(1, x_test)
y_forecast <- x_forecast %*% theta

# Confidence interval
alpha <- 0.05
vmatrix <- sigma2 * (1 + (x_forecast %*% solve(t(x_ols) %*% x_ols)) %*% t(x_forecast))

CI_lb <- y_forecast - qt(1 - alpha / 2, n_train - length(theta)) * sqrt(diag(vmatrix))
CI_ub <- y_forecast + qt(1 - alpha / 2, n_train - length(theta)) * sqrt(diag(vmatrix))

# Plot fitted model, training data, forecast and CI
df_train <- data.frame(year = x, y = train, y_hat = y_hat)
df_forecast <- data.frame(year = x_forecast[,2], y = test, yhat = y_forecast, CI_lb = CI_lb, CI_ub = CI_ub)


ggplot(df_train, aes(x = year, y=train), title("Prediction of car fleet")) +
  geom_point(col = "black") + 
  geom_line(aes(x = year, y=y_hat), col="red", size = 0.5) +
  geom_point(data = df_forecast, aes(x = year, y = yhat), col="red", size=1.5) +
  geom_point(data = df_forecast, aes(x = year, y = y), col="orange", size=1.5) +
  geom_ribbon(data=df_forecast, aes(x=year, ymin=CI_lb, ymax=CI_ub), inherit.aes=FALSE, alpha=0.2, fill="blue") +
  ggtitle("Future prediction of car fleet with 95% prediction interval") + ylab("Number of Vehicles") +
  theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

# Residual of model on historic data, clearly not white noise
plot(x, error, ylab = "Residuals")

# QQ plot of residuals
qqnorm(error, ylab = "Error quantiles")
qqline(error)

# Test error, model overestimates car fleet
plot(x_forecast[, 2], test - y_forecast, ylab = "Residuals", xlab = "x")




##### Q3 #####

# 3.1
lambda <- 0.9
SIGMA_OLS <- diag(n)
SIGMA_WLS <- diag(n)
for (i in 1:n) {
  SIGMA_WLS[i,i] <- 1/lambda^(n-i)
}
print(SIGMA_WLS[55:59,55:59])
W_WLS <- solve(SIGMA_WLS)

# 3.2
weights <- lambda^((n-1):0)
# plot the weights:
p <- barplot(weights, names = x, col = "blue", xlab = "Year", ylab = "Weight", axes = FALSE)
axis(side = 2, pos = -0.2)

# 3.3
print(sum(weights))

# 3.4
WLS <- solve(t(x_ols)%*%solve(SIGMA_WLS)%*%x_ols)%*%(t(x_ols)%*%solve(SIGMA_WLS)%*%y_train)
print(WLS)

# 3.5
y_wls <- x_test
lambdas <- c(0.9, 0.8, 0.7, 0.6)
df_forecast_wls <- data.frame(year = x_forecast[,2])
frame_names <- c('l09', 'l08', 'l07', 'l06')
df_forecast_wls[frame_names] <- NA

df_train[frame_names] <- NA

for (j in 1:length(lambdas)) {
  lambda = lambdas[j]
  for (i in 1:n) {
    SIGMA_WLS[i,i] <- 1/lambda^(n-i)
  }
  WLS <- solve(t(x_ols)%*%solve(SIGMA_WLS)%*%x_ols)%*%(t(x_ols)%*%solve(SIGMA_WLS)%*%y_train)
  y_pred <- x_forecast %*% WLS
  df_forecast_wls[frame_names[j]] <- y_pred
  df_train[frame_names[j]] <- x_ols %*% WLS
  print(WLS)
}

colors <- c("magenta", "blue", "red", "green")

ggplot(df_train, aes(x = year, y=train, colour='black')) +
  geom_point(col = "black") +
  geom_point(data=df_forecast_wls, aes(x=year, y=l09, colour="n9"), size=1)+
  geom_point(data=df_forecast_wls, aes(x=year, y=l08, colour="n8"), size=1)+
  geom_point(data=df_forecast_wls, aes(x=year, y=l07, colour="n7"), size=1)+
  geom_point(data=df_forecast_wls, aes(x=year, y=l06, colour="n6"), size=1)+
  geom_line(data=df_train, aes(x=year, y=l09, colour="n9"),linewidth=1, alpha=0.3)+
  geom_line(data=df_train, aes(x=year, y=l08, colour="n8"),linewidth=1, alpha=0.3)+
  geom_line(data=df_train, aes(x=year, y=l07, colour="n7"),linewidth=1, alpha=0.3)+
  geom_line(data=df_train, aes(x=year, y=l06, colour="n6"),linewidth=1, alpha=0.3)+
  theme(legend.position = c(0.9, 0.2), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c(n9 = colors[1], n8 = colors[2], n7 = colors[3], n6 = colors[4]),
                     labels = c(n9 = "\u03bb = 0.9", n8 = "\u03bb = 0.8", n7 = "\u03bb = 0.7", n6 = "\u03bb = 0.6")) +
  labs(y= "Number of Vehicles", x = "Year", title = "Forecasting 12 months with WLS", color = "Legend")
  
# 3.6


##### Q4 #####

# 4.1 
f <- function(j) rbind(1, j)
L <- matrix(c(1.,0., 1.,1.),
            byrow=TRUE, nrow=2)
Linv <- solve(L) 

# 4.2
i <- 1
F_N <-  (lambda^0) * f(0)%*%t(f(0))
h_N <-  (lambda^0) * f(0) * y_train[i]

# 4.3
# Using λ = 0.9 update FN and hN recursively and provide F10 and h10. We will 
# not calculate predictions for these first 10 steps.
lambda <- 0.9
for (i in 2:10){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0) * y_train[i]
  theta_N <- solve(F_N) %*% h_N
  
  yhat_N <- t(f(-(i-1):(59-i))) %*% theta_N
  plot_N <- ggplot(df_train, aes(x = year, y = y)) +
    geom_point() + 
    geom_point(data=df_train[1:i,], col="blue") + 
    geom_line(data=df_train[1:i,], aes(y=yhat_N[1:i]), col="blue") +
    #geom_line(aes(y=yhat_ols), col="red", linetype=2) + 
    #xlim(1980, 2005) + ylim(0,5.5) + 
    labs(y= "Number of Vehicles", x = "Year", title = paste0("N = ", i))
  
  print(plot_N)
}

# 4.4
# Now update the model recursively up to F59 and h59, while also calculating predictions at each
# step. You should calculate predictions for 1 month ahead, 6 months ahead and 12 months ahead.
# 4.5
#Plot the resulting 1-month, 6-month and 12-month prediction together with the training data.

idx_pred_1 <- seq(from = (11+1), to = 59, by = 1)
idx_pred_6 <- seq(from = (11+6), to = 59, by = 1)
idx_pred_12 <- seq(from = (11+12), to = 59, by = 1)

df_forecast_1m <- data.frame(year = x_ols[,2][idx_pred_1])
df_forecast_6m <- data.frame(year = x_ols[,2][idx_pred_6])
df_forecast_12m <- data.frame(year = x_ols[,2][idx_pred_12])
df_forecast_1m['values'] <- NA
df_forecast_6m['values'] <- NA
df_forecast_12m['values'] <- NA

for (i in 11:59){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))
  h_N <- lambda * Linv %*% h_N + f(0) * y_train[i] # y not found
  theta_N <- solve(F_N) %*% h_N
  
  yhat_N <- t(f(-(i-1):(59-i))) %*% theta_N
  df_forecast_1m[(i-10),'values'] <- yhat_N[(i+1)]
  df_forecast_6m[(i-10),'values'] <- yhat_N[(i+6)]
  df_forecast_12m[(i-10),'values'] <- yhat_N[(i+12)]
  #df_forecast_1m[(i-10),'values'] <- t(f(1)) %*% theta_N
  #df_forecast_6m[(i-10),'values'] <- t(f(6)) %*% theta_N
  #df_forecast_12m[(i-10),'values'] <- t(f(12)) %*% theta_N
}
colors <- c("blue", "red", "darkgreen")
ggplot(df_train, aes(x = year, y=train, colour='black')) +
  geom_line(col = "black") +
  geom_point(data=df_forecast_1m, aes(x=year, y=values, colour='n1'), size=0.7)+
  geom_point(data=df_forecast_6m, aes(x=year, y=values, colour='n2'), size=0.7)+
  geom_point(data=df_forecast_12m, aes(x=year, y=values, colour='n3'), size=0.7)+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(2018, 2023) + ylim(2350000,270000) +
  scale_color_manual(values = c(n1 = colors[1], n2 = colors[2], n3 = colors[3]),
    labels = c(n1 = "1 month", n2 = "6 months", n3 = "12 months")) +
    labs(y= "Number of Vehicles", x = "Year", title = "Forecasts with different forecast horizons", color = "Legend")


# 4.6 Repeat the iterative predictions for λ = 0.55, 0.56, 0.57, ..., 0.95, and 
# calculate the root-meansquare of the prediction errors for each forecast-horizon 
# (1 month, 6 months and 12 months) and for each value of λ.
# Plot the root-mean-square of the prediction errors versus λ for both 1 month, 6 months and 12
# months predictions.
df_forecast_1m <- data.frame(year = x_ols[,2][idx_pred_1])
df_forecast_6m <- data.frame(year = x_ols[,2][idx_pred_6])
df_forecast_12m <- data.frame(year = x_ols[,2][idx_pred_12])
df_forecast_1m['values'] <- NA
df_forecast_6m['values'] <- NA
df_forecast_12m['values'] <- NA

lambdas <- seq(from = 0.55, to = 0.95, by = 0.01)
df_prediction_errors <- data.frame(lambda = lambdas)
frame_names <- c('m1', 'm6', 'm12')
df_prediction_errors[frame_names] <- NA

for (j in 1:length(lambdas)) {
  lambda <- lambdas[j]
  i <- 1
  F_N <-  (lambda^0) * f(0)%*%t(f(0))
  h_N <-  (lambda^0) * f(0) * y_train[i]
  
  for (i in 2:59){
    F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))
    h_N <- lambda * Linv %*% h_N + f(0) * y_train[i] 
    if (i > 10) {
      theta_N <- solve(F_N) %*% h_N
      
      yhat_N <- t(f(-(i-1):(59-i))) %*% theta_N
      df_forecast_1m[(i-10),'values'] <- yhat_N[(i+1)]
      df_forecast_6m[(i-10),'values'] <- yhat_N[(i+6)]
      df_forecast_12m[(i-10),'values'] <- yhat_N[(i+12)]
    }
  }
  df_prediction_errors[j,'m1'] <- sqrt(mean((y_train[(11+1):59] - df_forecast_1m[(1:(59-10-1)),'values'])^2))
  df_prediction_errors[j,'m6'] <- sqrt(mean((y_train[(11+6):59] - df_forecast_6m[(1:(59-10-6)),'values'])^2))
  df_prediction_errors[j,'m12'] <- sqrt(mean((y_train[(11+12):59] - df_forecast_12m[(1:(59-10-12)),'values'])^2))
}

ggplot()+ #df_prediction_errors, aes(x = lambda, y=m1, colour='n1'), size=1) +
  #geom_point() +
  geom_point(data=df_prediction_errors, aes(x=lambda, y=m1, colour='n1'), size=1)+
  geom_point(data=df_prediction_errors, aes(x=lambda, y=m6, colour='n2'), size=1)+
  geom_point(data=df_prediction_errors, aes(x=lambda, y=m12, colour='n3'), size=1)+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(0.55, 0.95) + ylim(0,30000) +
  scale_color_manual(values = c(n1 = colors[1], n2 = colors[2], n3 = colors[3]),
                     labels = c(n1 = "1 month", n2 = "6 months", n3 = "12 months")) +
  labs(y= "RMSE", x = "lambda", title = "RMSE for different values of lambda", color = "Legend")

# 4.7
idx_min_1m <- which.min(df_prediction_errors[,'m1'])
opt_lambda_1m <- lambdas[idx_min_1m]

# 4.8
idx_min_6m <- which.min(df_prediction_errors[,'m6'])
opt_lambda_6m <- lambdas[idx_min_6m]

# 4.9
idx_min_12m <- which.min(df_prediction_errors[,'m12'])
opt_lambda_12m <- lambdas[idx_min_12m]

opt_lambdas <- c(opt_lambda_1m, opt_lambda_6m, opt_lambda_12m)

# 4.10. 
# It would be problematic to make λ as small as 0.5. Why is that? 
# (hint: compare the sum of weights to the number of parameters).
lambda <- 0.5
weights <- lambda^((n-1):0)
print(sum(weights))
# = 2

# 4.11. 
#Compare the 1-month-ahead prediction with λ = 0.55 to the naive persistence model 
# (i.e. using only the last observation to predict 1 month ahead). 
# Which one is better?

lambda <- 0.55
i <- 1
F_N <-  (lambda^0) * f(0)%*%t(f(0))
h_N <-  (lambda^0) * f(0) * y_train[i]

for (i in 2:59){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))
  h_N <- lambda * Linv %*% h_N + f(0) * y_train[i] 
  if (i > 10) {
    theta_N <- solve(F_N) %*% h_N
    
    yhat_N <- t(f(-(i-1):(59-i))) %*% theta_N
    df_forecast_1m[(i-10),'values'] <- yhat_N[(i+1)]
  }
}

df_persistence <- data.frame(year=df_train[12:59,'year'], values=df_train[11:58,'y'])

ggplot(df_train, aes(x=year, y=y)) +
  geom_line(colour='black') +
  geom_point(data=df_forecast_1m, aes(x=year, y=values, colour='n1'), size=0.7)+
  geom_point(data=df_persistence, aes(x=year, y=values, colour='n2'), size=0.7)+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(2018, 2023) + ylim(2350000,2650000) +
  scale_color_manual(values = c(n1 = colors[1], n2 = "magenta"),
                     labels = c(n1 = "1 month", n2 = "persistence")) +
  labs(y= "Number of Vehicles", x = "Year", title = "Comparison with persistence model", color = "Legend")

rmse_naive <- sqrt(mean((y_train[(11+1):59] - y_train[(11):58])^2))
# = 5330.434

# 4.12
# Now choose the best forecasts at time t = 59 of Yt for time t = 60, t = 65 and t = 71, i.e.
# ˆ Y59+1|59, ˆ Y59+6|59, and ˆ Y59+12|59 and plot them together with the training data AND the test
# data (i.e. 2nd data file).
df_pred <- data.frame(year = x_test[c(1,6,12)])
df_pred['values'] <- NA
l <- c(1,6,12)
for (k in 1:3) {
  lambda <- opt_lambdas[k]
  i <- 1
  F_N <-  (lambda^0) * f(0)%*%t(f(0))
  h_N <-  (lambda^0) * f(0) * y_train[i]
  
  for (i in 2:59){
    F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))
    h_N <- lambda * Linv %*% h_N + f(0) * y_train[i] 
  }
  theta_N <- solve(F_N) %*% h_N
  y_pred <- t(f(l[k])) %*% theta_N
  df_pred[k,'values'] <- y_pred
}

df_test <- data.frame(year = x_test, y = test)

ggplot(df_train, aes(x=year, y=y)) +
  geom_line(colour='black') +
  geom_point(data=df_test, aes(x=year, y=y, colour='n1'), size=0.5)+
  geom_point(data=df_pred, aes(x=year, y=values, colour='n2'), size=0.5)+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(2018, 2024) + ylim(2350000,2700000) + 
  scale_color_manual(values = c(n1 = 'black', n2 = "magenta"),
                     labels = c(n1 = "Test data", n2 = "Forecast")) +
  labs(y= "Number of Vehicles", x = "Year", title = "Forecasts compared to test data", color = "Legend")








