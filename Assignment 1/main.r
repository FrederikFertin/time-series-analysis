# Required package to read excel
library("readxl")
library("ggplot2")

### DATA IMPORT ###
# Training Data
my_data <- read_excel("Assignment 1/DST_BIL54_train.xlsx")
n_train <- length(my_data)
train <- unlist(my_data[1, 2:n_train])
x <- seq(2018, 2022 + 10/12, by = 1 / 12)
n = length(x)
y_train = train

# Test Data
my_data <- read_excel("Assignment 1/DST_BIL54_test.xlsx")
n_test <- length(my_data)
test <- unlist(my_data[1, 2:n_test])
y_test = test


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
x_forecast <- seq(2022 + 11 / 12, 2023 + 10 / 12, by = 1 / 12)
x_forecast <- cbind(1, x_forecast)
y_forecast <- x_forecast %*% theta

# Confidence interval
alpha <- 0.05
vmatrix <- sigma2*(1+(x_forecast%*%solve(t(x_ols)%*%x_ols))%*%t(x_forecast))

CI_lb <- y_forecast - qt(1 - alpha / 2, n_train - length(theta)) * sqrt(diag(vmatrix))
CI_ub <- y_forecast + qt(1 - alpha / 2, n_train - length(theta)) * sqrt(diag(vmatrix))

# Plot fitted model, training data, forecast and CI
df_train <- data.frame(year = x, y = train, y_hat = y_hat)
df_forecast <- data.frame(year = x_forecast[,2], y = y_forecast, CI_lb = CI_lb, CI_ub = CI_ub)


ggplot(df_train, aes(x = year, y=train), title("Prediction of car fleet")) +
  geom_point(col = "black") + 
  geom_line(aes(x = year, y=y_hat), col="red", size = 0.5) +
  geom_point(data = df_forecast, aes(x = year, y = y), col="orange", size=1.5) +
  geom_ribbon(data=df_forecast, aes(x=year,ymin=CI_lb, ymax=CI_ub), inherit.aes=FALSE, alpha=0.2, fill="blue") +
  ggtitle("Future prediction of car fleet with 95% prediction interval") + ylab("Number of Vehicles") +
  theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

# Risidual of model on historic data, clearly not white noise
plot(x, error, ylab = "Residuals")

# Test error, model overestimates car fleet
plot(x_forecast[,2], test - y_forecast, ylab = "Residuals", xlab = "x")




##### Q3 #####

# 3.1
lambda = 0.9
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
barplot(weights, names=1:n)

# 3.3
sum(weights)

# 3.4
WLS <- solve(t(x_ols)%*%solve(SIGMA)%*%x_ols)%*%(t(x_ols)%*%solve(SIGMA)%*%y_train)




##### Q4 #####
