# set up

setwd("../Red wine rating predictions")
red.df <- read.csv("red.txt", sep=";")

# Define the number of folds used in cross-validation & the number of repeats of cross-validation. 

nfold <- 10
repeats <- 5

# n = number of observation
# m = splitting observations into 10 folds (using floor() to round down)

n <- nrow(red.df)
m <- floor(n/nfold)

# Linear model with all available covariates

fit.lm <- lm(quality~., data=red.df)

# Building empty shells for prediction error CV.

peCV.lm <- numeric(repeats)
pe.lm <- numeric(nfold)

# Building empty shells for r.squared CV.

rsqCV.lm <- numeric(repeats)
rsq.lm <- numeric(nfold)

# First loop: Randomise the data into random order. Take the first 1 to m as the initial test index.

for(i in 1:repeats){
  rand.order <- order(runif(n))
  data = red.df[rand.order,]
  test.index <- 1:m
  
  # Second loop: Split the data set into traning and test sets. Fit the model with the training set & predict with the test set. Find MSE between each point prediction and the real value. Generate another sequence of test index for the next iteration (to find MSE of the next set.)
  
  for (j in 1:nfold) {
    train <- data[-test.index,]
    test <- data[test.index,]
    fit.lm <- lm(model.full, data=train)
    pred.lm = predict(fit.lm, newdata=test)
    pe.lm[j] <- mean((test$quality - pred.lm)^2)
    rsq.lm[j] <- summary(fit.lm)$r.squared
    test.index <- if(j==nfold) (max(test.index)+1):n else test.index + m
  }
  
  # The CV value is the average of all MSE generated in the second loop. Then the whole process is repeated (for repeats=5 as set.)

    peCV.lm[i] <- mean(pe.lm)
  rsqCV.lm[i] <- mean(rsq.lm)
}

# Find the final cross-validated prediction eror & R-squared 

mean(peCV.lm)
mean(rsqCV.lm)
