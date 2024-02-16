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
ones <- rep(1, length(x))
x_ols <- cbind(ones, x)
theta <- solve(t(x_ols) %*% x_ols) %*% t(x_ols) %*% train

y_hat <- x_ols %*% theta
plot(x, y_hat, col = "blue", title("Training data"))



##### Q3 #####






##### Q4 #####