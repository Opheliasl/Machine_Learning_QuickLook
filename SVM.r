#install.packages("e1071") # only do this once
library(e1071)

# Simulate data
set.seed(1)
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y==1,] <- x[y==1,] + 1
plot(x[,c(2:1)], col=(3-y), xlab = "x.2", ylab = "x.1")

# Linear SVM
dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y~., data = dat, kernel = "linear",
              cost = 10, scale = FALSE)
plot(svmfit, dat)
svmfit$index # indeces of the support vectors
summary(svmfit)

# Change cost value
svmfit <- svm(y~., data = dat, kernel = "linear",
              cost = .1, scale = FALSE)
plot(svmfit, dat)
svmfit$index # indeces of the support vectors
summary(svmfit)

# Find the optimal parameter using cross validation
set.seed(1)
tune.out <- tune(svm, y~., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
summary(tune.out$best.model)

# Fit the model
ypred <- predict(tune.out$best.model, dat)
table(ypred, y)

# Kernel: polynomial or radial
set.seed(1)
x <- matrix(rnorm(200*2), ncol = 2)
y <- c(rep(-1, 150), rep(1, 50))
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
plot(x[,c(2:1)], col=(3-y), xlab = "x.2", ylab = "x.1")

# Kernel SVM
dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y~., data = dat, kernel = "radial",
              gamma = 1, cost = 1)
summary(svmfit)
plot(svmfit, dat)

svmfit <- svm(y~., data = dat, kernel = "radial",
              gamma = 1, cost = 1e5)
summary(svmfit)
plot(svmfit, dat)

# Tune parameter
set.seed(1)
tune.out <- tune(svm, y~., data = dat, kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out$best.model)
plot(tune.out$best.model, dat)
ypred <- predict(tune.out$best.model, dat)
table(ypred, y)