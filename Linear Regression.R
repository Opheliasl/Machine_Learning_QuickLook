library(rgl) # 3d visualization package

# Load data
data(mtcars)
summary(mtcars)

# Fit a linear model
mylm <- lm(mpg ~ cyl + wt, data = mtcars)
summary(mylm)

# Draw the data
plot3d(mtcars$cyl, mtcars$wt, mtcars$mpg, xlab="cyl", ylab="wt", zlab="mpg", cex=1.5, size=1, type="s")
# Draw the fitted plane
planes3d(a=mylm$coefficients[2], b = mylm$coefficients[3], c = -1, d=mylm$coefficients[1], alpha=.5)
# Visualization
myproj <- data.frame(cyl=mtcars$cyl, wt=mtcars$wt, mpg=mylm$fitted.values)
plot3d(myproj, col="red", cex=1.5, size=1, add=TRUE,type="s")
# Draw the residuals
mylist <- list(mtcars[,c("cyl", "wt", "mpg")],myproj)
segments3d(do.call(rbind,mylist)[order(sequence(sapply(mylist,nrow))),])


# Subset selection
mylm.full <- lm(mpg ~ . , data = mtcars)
summary(mylm.full)
smylm1 <- step(mylm.full, direction = "backward", trace = 1)
summary(smylm1)


mylm.null <- lm(mpg ~ 1, data = mtcars)
smylm2 <- step(mylm.null, direction = "forward", trace = 1, 
               scope =  ~ cyl + disp + hp + drat + wt + qsec +
                 vs + am + gear + carb)
summary(smylm2)

# Regularized regression
library(glmnet)

# No regularization
mylm <- lm(mpg ~ wt + cyl + hp, data = mtcars)
summary(mylm)

# Load data
x <- model.matrix(mpg ~ wt + cyl + hp, mtcars)[,-1]
y <- mtcars$mpg

# Ridge regression
ridge.mod <- glmnet(x, y, alpha = 0, lambda = 3.0)
coef(ridge.mod)

# Lasso regression
lasso.mod <- glmnet(x, y, alpha = 1, lambda = 3.0)
coef(lasso.mod)

