# Load data
mydata <- read.table("crx.new.data", header=FALSE)
mydata <- mydata[,c(1,4,5,41,47,48)]
summary(mydata)
myresponse <- factor(mydata[,1])

mydf <- data.frame(myresponse, 
                   mydata[,2:ncol(mydata)])
numobs <- nrow(mydf)
numobs

# Part I: k-fold cross validation (classification tree)
set.seed(1)
numFolds <- 10

# Assign observations to k groups
xvalFoldNumber <- sample(1:numobs %% numFolds + 1,
                         replace=FALSE)
xvalFoldNumber

# Create a list of test observations for each group
xvalSets <- lapply(1:numFolds, FUN=function(x) {
  list(test=which(xvalFoldNumber == x))
})
xvalSets

library(rpart)
# Create a function for each group
rpartFold <- function(x) {
  testdf <- mydf[x$test,]
  traindf <- mydf[-x$test,]
  
  myrpart <- rpart(myresponse ~ ., data=traindf)
  ## classification predictions
  myrpartPredict <- predict(myrpart, newdata=testdf, type="class")
  confusion <- table(testdf[,1], myrpartPredict) 
  confusion
}

# Apply the function to each group
myrpartResults <- lapply(xvalSets, FUN=rpartFold)
myrpartResults

# Sum up all the results
totalConfusion <- Reduce("+", myrpartResults)
totalConfusion
totalConfusion/rowSums(totalConfusion)

# Part II: train, validate, test framework (SVM)
set.seed(1) 
library(e1071)
randomsort <- sample(1:numobs, numobs, replace=FALSE)
randomsort
# lists of observations for train, validate, test
trainloc <- randomsort[1:floor(0.5*numobs)]
valloc <- randomsort[(floor(0.5*numobs)+1):floor(0.75*numobs)]
testloc <- randomsort[(floor(0.75*numobs)+1):numobs]
testloc

# Candidates for the parameters
Cparams <- c(0.1, 1.0, 10.0, 100.0, 1000.0, 10000.0)
gammas <- c(0.01, 0.1, 1.0, 10.0, 100.0)

# Function for SVM
trainSVM <- function(i) {
  # linear kernel
  mysvm <- svm(myresponse ~ ., data=mydf[trainloc,], 
               kernel = "linear",
               cost = Cparams[i]) 
  svmValPredict <- predict(mysvm, newdata=mydf[valloc,])
  misclassLin <- sum(svmValPredict != mydf[valloc,1])/length(valloc)
  # Gaussian kernel
  misclassGau <- lapply(1:length(gammas), FUN=tuneGamma, i)
  
  list(misclassrateLin=misclassLin, misclassrateGau=misclassGau)
}

# Function for nonlinear SVM
tuneGamma <- function(k, i) {
  mysvm <- svm(myresponse ~ ., data=mydf[trainloc,],
                kernel = "radial",
                cost = Cparams[i], 
                gamma=gammas[k])
  svmValPredict <- predict(mysvm, newdata=mydf[valloc,])
  misclassGau <- sum(svmValPredict != mydf[valloc,1])/length(valloc)
  misclassGau
}

myresults <- lapply(1:length(Cparams), FUN=trainSVM)
myresults

misclassrateLin <- sapply(myresults, 
                          FUN=function(x) x$misclassrateLin)
misclassrateGau <- sapply(myresults, 
                          FUN=function(x) x$misclassrateGau)

misclassrateLin
misclassrateGau

# Train with the settings on training data 
mysvmTest <- svm(myresponse ~ ., data=mydf[trainloc,], 
                 kernel = "radial",
                 cost = Cparams[5], 
                 gamma=gammas[1])
# Generate predictions on test data
svmTestPredict <- predict(mysvmTest, newdata=mydf[testloc,])
misclassTest <- sum(svmTestPredict != mydf[testloc,1])/length(testloc)
misclassTest

# Report confusion matrix on test data
svmTestCM <- table(mydf[testloc,1],svmTestPredict)
svmTestCM
svmTestCM/rowSums(svmTestCM)

