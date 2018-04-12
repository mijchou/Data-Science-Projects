# set up

library("rpart")
library("caret")

setwd("../Red wine rating predictions")
red.df <- read.csv("red.txt", sep=";")

# Define the number of folds used in cross-validation & the number of repeats of cross-validation. 

nfold <- 10
repeats <- 5

# n = number of observation
# m = splitting observations into 10 folds (using floor() to round down)

n <- nrow(red.df)
m <- floor(n/nfold)

# Regression tree model

fit.rt <- rpart(quality~., data=red.df, method="anova", cp=0.001)

# Caret

grid.rt <- expand.grid(.cp=c(0.1, 0.05, 0.01, 0.005, 0.001))
rtCV <- train(model.full, data = red.df,
              method = "rpart",
              tuneGrid = grid.rt,
              trControl = trainControl(method="cv", number=10, repeats=5))

# Building empty shells for cross-validation & for prediction error.

peCV.rt <- numeric(repeats)
pe.rt <- numeric(nfold)

# Building empty shells for r.squared CV.

rsqCV.rt <- numeric(repeats)
rsq.rt <- numeric(nfold)

# First loop: Randomise the data into random order. Take the first 1 to m as the initial test index.

for(i in 1:repeats){
  rand.order <- order(runif(n))
  data = red.df[rand.order,]
  test.index <- 1:m
  
  # Second loop: Split the data set into traning and test sets. Fit the model with the training set & predict with the test set. Find MSE between each point prediction and the real value. Generate another sequence of test index for the next iteration (to find MSE of the next set.)
  
  for (j in 1:nfold) {
    train <- data[-test.index,]
    test <- data[test.index,]
    fit.rt <- rpart(model.full, data=train, method="anova", cp=0.005)
    pred.rt = predict(fit.rt, newdata=test)
    pe.rt[j] <- mean((test$quality - pred.rt)^2)
    
    #Calculating R-squared
    nullSS <- sum((train$quality-mean(train$quality))^2)
    rsq.rt[j] <- 1-sum(residuals(fit.rt)^2)/nullSS
    
    test.index <- if(j==nfold) (max(test.index)+1):n else test.index + m
  }
  
  # The CV value is the average of all MSE generated in the second loop. Then the whole process is repeated (for repeats=5 as set.)
  peCV.rt[i] <- mean(pe.rt)
  rsqCV.rt[i] <- mean(rsq.rt)
}

# Find the final cross-validated prediction eror & R-squared 
mean(peCV.rt)
mean(rsqCV.rt)


# Pruned Regression tree

# Check cp values and find the lowest xerror
plotcp(fit.rt)
printcp(fit.rt)

# Prune tree with the cp found
fit.rt2 = prune(fit.rt, cp=0.0034070)
plotcp(fit.rt2)

# Building empty shells for cross-validation & for prediction error.
peCV.rt2 <- numeric(repeats)
pe.rt2 <- numeric(nfold)

# Building empty shells for r.squared CV.
rsqCV.rt2 <- numeric(repeats)
rsq.rt2 <- numeric(nfold)

# First loop: Randomise the data into random order. Take the first 1 to m as the initial test index.
for(i in 1:repeats){
  rand.order <- order(runif(n))
  data = red.df[rand.order,]
  test.index <- 1:m
  
  # Second loop: Split the data set into traning and test sets. Fit the model with the training set & predict with the test set. Find MSE between each point prediction and the real value. Generate another sequence of test index for the next iteration (to find MSE of the next set.)
  
  for (j in 1:nfold) {
    train <- data[-test.index,]
    test <- data[test.index,]
    fit.rt <- rpart(model.full, data=train, method="anova", cp=0.01)
    fit.rt2 = prune(fit.rt, cp=0.0034070)
    pred.rt2 = predict(fit.rt2, newdata=test)
    pe.rt2[j] <- mean((test$quality - pred.rt2)^2)
    
    #Calculating R-squared
    nullSS <- sum((train$quality-mean(train$quality))^2)
    rsq.rt2[j] <- 1-sum(residuals(fit.rt2)^2)/nullSS
    
    test.index <- if(j==nfold) (max(test.index)+1):n else test.index + m
  }
  
  # The CV value is the average of all MSE generated in the second loop. Then the whole process is repeated (for repeats=5 as set.)
  peCV.rt2[i] <- mean(pe.rt2)
  rsqCV.rt2[i] <- mean(rsq.rt2)
}

# Find the final cross-validated prediction eror & R-squared 
mean(peCV.rt2)
mean(rsqCV.rt2)
