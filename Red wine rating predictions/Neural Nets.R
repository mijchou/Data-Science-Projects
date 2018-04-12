# set up

setwd("../Red wine rating predictions")
red.df <- read.csv("red.txt", sep=";")

library("nnet")
library("caret")

# Define the number of folds used in cross-validation & the number of repeats of cross-validation. 

nfold <- 10
repeats <- 5

# n = number of observation
# m = splitting observations into 10 folds (using floor() to round down)

n <- nrow(red.df)
m <- floor(n/nfold)

# Neural network model

fit.nnet <- nnet(quality ~ . , data=red.df, size=2, linout=TRUE, decay=0.01, maxit=1000)

# Picking the smalles PE using Caret.
# Results: The final values used for the model were size = 2 and decay = 0.01. 

grid.nnet <- expand.grid(.decay=c(0.01, 0.001), .size=2:10)
nnetcv <- train(model.full, data = red.df,
                method = "nnet",
                maxit = 1000,
                tuneGrid = grid.nnet,
                trace = FALSE,
                linout = TRUE,
                trControl = trainControl(method="cv", number=2, repeats=2))

# Building empty shells for cross-validation & for prediction error.

peCV.nnet <- numeric(repeats)
pe.nnet <- numeric(nfold)

# Building empty shells for r.squared CV.

rsqCV.nnet <- numeric(repeats)
rsq.nnet <- numeric(nfold)

# First loop: Randomise the data into random order. Take the first 1 to m as the initial test index.

for(i in 1:repeats){
  rand.order <- order(runif(n))
  data = red.df[rand.order,]
  test.index <- 1:m
  
  # Second loop: Split the data set into traning and test sets. Fit the model with the training set & predict with the test set. Find MSE between each point prediction and the real value. Generate another sequence of test index for the next iteration (to find MSE of the next set.)
  
  for (j in 1:nfold) {
    train <- data[-test.index,]
    test <- data[test.index,]
    fit.nnet <- nnet(model.full, data=train, size=2, linout=TRUE, decay=0.01, maxit=1000)
    pred.nnet = predict(fit.nnet, newdata=test)
    pe.nnet[j] <- mean((test$quality - pred.nnet)^2)
    
    #Calculating R-squared
    nullSS <- sum((train$quality-mean(train$quality))^2)
    rsq.nnet[j] <- 1-sum(residuals(fit.nnet)^2)/nullSS
    
    test.index <- if(j==nfold) (max(test.index)+1):n else test.index + m
  }
  
  # The CV value is the average of all MSE generated in the second loop. Then the whole process is repeated (for repeats=5 as set.)
  peCV.nnet[i] <- mean(pe.nnet)
  rsqCV.nnet[i] <- mean(rsq.nnet)
}

# Find the final cross-validated prediction eror & R-squared 

mean(peCV.nnet)
mean(rsqCV.nnet)
