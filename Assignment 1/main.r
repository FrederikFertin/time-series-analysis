# Required package to read excel
library("readxl")
library("ggplot2")

# Import the training and test data
my_data <- read_excel("Assignment 1/DST_BIL54_train.xlsx")
n_train <- length(my_data)
train <- unlist(my_data[1, 2:n_train])
x <- seq(2018, 2022 + 10/12, by = 1 / 12)

my_data <- read_excel("Assignment 1/DST_BIL54_test.xlsx")
n_test <- length(my_data)
test <- unlist(my_data[1, 2:n_test])



##### Q1 #####
plot(x, train, col = "blue", title("Training data"))

plot(2:n_test, test, col = "red")






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
s_ols <- sqrt(V_ols)

# Make forecast for next 12 months
x_forecast <- seq(2022 + 11 / 12, 2023 + 10 / 12, by = 1 / 12)
x_forecast <- cbind(1, x_forecast)
y_forecast <- x_forecast %*% theta

# Confidence interval
alpha <- 0.05
vmatrix <- sigma2*(1+(x_forecast%*%solve(t(x_ols)%*%x_ols))%*%t(x_forecast))

CI_lb <- y_forecast - qt(alpha / 2, n_train - length(theta)) * sqrt(diag(vmatrix))
CI_ub <- y_forecast + qt(alpha / 2, n_train - length(theta)) * sqrt(diag(vmatrix))

# Plot fitted model, training data, forecast and CI
plot(x, train, col = "blue", title("Training data - OLS estimate"),
     xlim=c(2018, 2024), ylim = c(2.3E6, 2.8E6)) 
lines(x, y_hat, col = "red")
points(x_forecast[, 2], y_forecast, col = "green")
lines(x_forecast[, 2], CI_lb, col = "green")
lines(x_forecast[, 2], CI_ub, col = "green")


df_train <- data.frame(year = x, y = train, y_hat = y_hat)
df_forecast <- data.frame(year = x_forecast[,2], y = y_forecast, CI_lb = CI_lb, CI_ub = CI_ub)


# Ribbon missing.....
ggplot(df_train, aes(x = year, y=train)) +
  geom_point() + 
  geom_line(aes(x = year, y=y_hat), col="red", size = 0.5) +
  geom_point(data = df_forecast, aes(x = year, y = y), col="red", size=.5) +
  geom_ribbon(data = df_forecast, aes(x=year, ymin=CI_lb, ymax=CI_ub), inherit.aes=FALSE, alpha=0.8, fill="red")

##### Q3 #####






##### Q4 #####
