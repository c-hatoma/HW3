# Let's get started on this homework, eh?

library(tidyverse)
library(rpart)
library(rattle)

#begin with the titanic dataset
titanic <- read_csv(file.choose())


# Bagging Function -------------------------------------------------------------------


bagging.function <- function(data, yvar, xvar, n){
  #Inputs:
  #data: dataset to use, either built-in on R or in the environment
  #yvar: outcome variable
  #xvar: predictor variable
  #n: number of bagged linear models user wants
  #Output: mean percentage of error
  
  #create a null vector of errors for each of the models
  beta1 <- NULL
  beta0 <- NULL
  
  #save data as a dataframe
  dataset <- data.frame(data)
  
  #get rid of observations in which the xvar and/or yvar are missing
  #save it to dataset2
  notNA <- which(is.na(dataset[[xvar]]) == FALSE & is.na(dataset[[yvar]]) == FALSE, arr.ind=TRUE) 
  dataset2 <- dataset[notNA, ]
  
  #forloop to repeat model n number of times
  for(i in 1:n){
    
    #create index for bagging
    indexes <- sample(1:nrow(dataset2), 
                      size = nrow(dataset2),
                      replace = TRUE)
    
    #create training data with the indexes created above
    train <- dataset2[indexes, ]
    
    #create testing datta with the indexes not included in training data
    test <- dataset2[-indexes, ]
    
    #mean x and y from the training data
    meanx <- mean(train[[xvar]])
    meany <- mean(train[[yvar]])
    
    #vectors of x and y variables from the training data
    x <- train[[xvar]]
    y <- train[[yvar]]
    
    #use training data to model yvar on xvar
    #calculate beta1, the slope
    beta1[i] <- sum((x - meanx)*(y - meany)) / sum((x - meanx)^2)
    
    #calculate beta0, the intercept
    beta0[i] <- meany - beta1[i]*meanx
  }
  
  #vectors of x and y variables from the testing data
  testx <- test[[xvar]]
  testy <- test[[yvar]]
  
  mean.beta1 <- mean(beta1)
  mean.beta0 <- mean(beta0)
  
  #predict yhat, the predicted outcome, for testing data
  yhat <- round(mean.beta0 + mean.beta1*testx)
  
  #compare yhat (predicted y) and testy (the actual y) for accuracy
  accurate <- ifelse(yhat == testy, 
                     1,
                     0)
  
  #add to the error vector the error rate of this model
  error <- 1 - sum(accurate)/length(accurate)
  
  #calculate and return the mean error rate of all bagged linear models
  return(mean(error))
}


# LM Function -------------------------------------------------------------


lm.function <- function(data, yvar, xvar){
  #Inputs:
  #data: dataset to use, either built-in on R or in the environment
  #yvar: outcome variable
  #xvar: predictor variable
  #Output: percentage of error
  
  #save data as a dataframe
  dataset <- data.frame(data)
  
  #get rid of observations in which the xvar and/or yvar are missing
  #save it to dataset2
  notNA <- which(is.na(dataset[[xvar]]) == FALSE & is.na(dataset[[yvar]]) == FALSE, arr.ind=TRUE) 
  dataset2 <- dataset[notNA, ]
  
  #create index for bagging
  indexes <- sample(1:nrow(dataset2), 
                    size = nrow(dataset2),
                    replace = TRUE)
  
  #create training data with the indexes created above
  train <- dataset2[indexes, ]
  
  #create testing datta with the indexes not included in training data
  test <- dataset2[-indexes, ]
  
  #vectors of x and y variables from the training data
  x <- train[[xvar]]
  y <- train[[yvar]]
  
  #vectors of x and y variables from the testing data
  testx <- test[[xvar]]
  testy <- test[[yvar]]
  
  model.data <- train
  
  #create lm function
  model1 <- lm(model.data[[yvar]] ~ model.data[[xvar]],
               data = model.data)
  
  model.data <- test
  
  #predict yhat, the predicted outcome, for testing data
  yhat <- round(predict.lm(model1,
                           model.data,
                           type = 'response'))
  
  #compare yhat (predicted y) and testy (the actual y) for accuracy
  accurate <- ifelse(yhat == testy, 
                     1,
                     0)
  
  #calculate and return the error
  error <- 1 - sum(accurate)/length(accurate)
  
  return(error)
}



# Testing our Functions on Titanic and Mtcars Data ------------------------
# We chose two different variable inputs to test with each dataset.

#using titanic data
lm.function(titanic, "Survived", "Pclass")
lm.function(titanic, "Survived", "Age")

#try with mtcars data
lm.function(mtcars, "am", "mpg")
lm.function(mtcars, "am", "cyl")



# Comparison between bagged linear modeling function and lm functions --------

#Getting the ranges and variances of both functions to compare

#bagging function
bag <- NULL
for (i in 1:1000){
  bag[i] <- bagging.function(titanic, "Survived", "Pclass", 25)
}
summary(bag)


# bag100 <- NULL
# for (i in 1:1000){
#   bag[i] <- bagging.function(titanic, "Survived", "Pclass", 100)
# }
# summary(bag100)

#lm function
lm <- NULL
for (i in 1:1000){
  lm[i] <- lm.function(titanic, "Survived", "Pclass")}
summary(lm)




#Similarities: 


#Differences:

#If one runs the bagged function with the same input multiple times, 
#the error rate remains similar each time the function runs.

#For example, the error rate for examining the effects of passenger class on survival rate
#in the titanic dataset with 25 bagged linear models is relatively stable around 0.32.
#The code is below:
bagging.function(titanic, "Survived", "Pclass", 25)

#However, the lm function returns a wider range of error rates. 

#For the same example as above, the lm function sometimes returns an error rate of 0.29
#while sometimes it returns an error rate as high as 0.36. The code is below:
lm.function(titanic, "Survived", "Pclass")

# However, looking at the ranges of the two functions when bootstrapped,
# we see that the difference between the bagged range and lm range are small.

# The improvement in variance from using bagging on a linear regression when compared to 
# a regular linear regression model is this small because although bagging reduces variance,
# linear regression has low variance already.




# Works Cited:
# https://towardsdatascience.com/understanding-the-effect-of-bagging-on-variance-and-bias-visually-6131e6ff1385
