# Required package to read excel
library("readxl")

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
y_forecast <- x_forecast %*% theta

# Confidence interval
alpha <- 0.05



##### Q3 #####






##### Q4 #####