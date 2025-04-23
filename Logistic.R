library(ggplot2)
library(caret)  
library(MASS) 
library(neuralnet)
library(rpart)
library(randomForest)
library(dplyr)
library(gbm) 
library(class)

install.packages("ridge")
library(ridge)

# Read in the Excel file and give the data a name to refer to it
mydata <- read.csv(file.choose())

# Always check your data first
View(mydata)
mydata <- mydata[,-1]

# Partition the data for cross-validation
set.seed(123)
trainIndex   <- createDataPartition(mydata$Have.Tried, p=0.75, list=FALSE, times=1)
train        <- mydata[trainIndex, ]
test         <- mydata[-trainIndex, ]

# Logistic regression
olr <- glm(Have.Tried.Yes ~ ., data = train, family=binomial)

summary(olr)

# Predict probabilities for the test set
olrpred <- predict(olr, test, type="response")

# Convert predictions to dataframe
olrpred <- as.data.frame(olrpred)

# Assign instance to class with highest probability
olrpred %>%
  mutate(max_prob = round(olrpred)) -> olrpred

# View confusion matrix and statistics
confusionMatrix(factor(olrpred$max_prob), factor(test$Have.Tried.Yes), mode = 'everything')

# We can also do ridge logistic regression 
rlr <- logisticRidge(Have.Tried.Yes ~ ., data = train, family=binomial)
summary(rlr)

# k-nearest neighbor
train_var <- train[,-ncol(train)]
test_var <- test[,-ncol(train)]
knear <- knn(train = train_var, test = test_var, cl = train$Have.Tried.Yes, k=6)

# model prediction
tab <- table(knear,test$Have.Tried.Yes)
tab

# Neural Network
nnet <- neuralnet(Have.Tried.Yes ~ ., data = train,
                 hidden = c(11,5,2),
                 act.fct = "logistic",
                 algorithm = "rprop+")

# Predict probabilities for the test set
nnetpred <- predict(nnet, test, type="response")

# Convert predictions to dataframe
nnetpred <- as.data.frame(nnetpred)

# Assign instance to class with highest probability
nnetpred %>%
  mutate(max_prob = round(nnetpred$V1)) -> nnetpred

# View confusion matrix and statistics
confusionMatrix(factor(nnetpred$max_prob), factor(test$Have.Tried.Yes), mode = 'everything')

# Decision tree
dt <- rpart(Have.Tried.Yes ~ .,
            method="class", data = train)

plot(dt, uniform=TRUE,
     main="Classification Tree")
text(dt, use.n=TRUE, all=TRUE, cex=.8)
summary(dt)

# Predict probabilities for the test set
dtpred <- predict(dt, test)

# Convert predictions to dataframe
dtpred <- as.data.frame(dtpred)

# Assign instance to class with highest probability
dtpred %>%
  mutate(max_prob = round(dtpred$`1`)) -> dtpred

# View confusion matrix and statistics
confusionMatrix(factor(dtpred$max_prob), factor(test$Have.Tried.Yes), mode = 'everything')

# Random Forest
rf <- randomForest(Have.Tried.Yes ~ .,
                   method="class", data = train)

# Predict probabilities for the test set
rfpred <- predict(rf, test)

# Convert predictions to dataframe
rfpred <- as.data.frame(rfpred)

# Assign instance to class with highest probability
rfpred %>%
  mutate(max_prob = round(rfpred)) -> rfpred

# View confusion matrix and statistics
confusionMatrix(factor(rfpred$max_prob), factor(test$Have.Tried.Yes), mode = 'everything')

# Boosted trees
gbm <- gbm (Have.Tried.Yes ~ ., distribution = "bernoulli",
            data = train,
            n.trees = 1000,
            interaction.depth = 2,
            cv.folds = 5)

# Predict probabilities for the test set
gbmpred <- predict(gbm, test, type = "response")

# Convert predictions to dataframe
gbmpred <- as.data.frame(gbmpred)

# Assign instance to class with highest probability
gbmpred %>%
  mutate(max_prob = round(gbmpred)) -> gbmpred

# View confusion matrix and statistics
confusionMatrix(factor(gbmpred$max_prob), factor(test$Have.Tried.Yes), mode = 'everything')
