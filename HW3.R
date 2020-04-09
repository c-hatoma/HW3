# Libraries and data ------------------------------------------------------

library(tidyverse)
library(rpart)
library(rattle)
library(caret)

#begin with the titanic dataset
titanic <- read_csv(file.choose())



# Bagging function --------------------------------------------------------

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
  
  #calculate error rate of this model
  error <- 1 - sum(accurate)/length(accurate)
  
  #add to the error vector the error rate of this model
  return(mean(error))
  
}

#using titanic data
bagging.function(titanic, "Survived", "Pclass", 25)
bagging.function(titanic, "Survived", "Age", 25)


#try with mtcars data
bagging.function(mtcars, "am", "mpg", 25)
bagging.function(mtcars, "am", "cyl", 25)



# LM Function -------------------------------------------------------------

#LM Function
lm.function <- function(data, yvar, xvar){
  #Inputs:
  #data: dataset to use, either built-in on R or in the environment
  #yvar: outcome variable
  #xvar: predictor variable
  #Output: percentage of error
  
  #save data as a dataframe
  dataset <- data.frame(data)
  
  #get rid of observations in which the xvar and/or yvar.lm are missing
  #save it to dataset2
  notNA <- which(is.na(dataset[[xvar]]) == FALSE & is.na(dataset[[yvar]]) == FALSE, arr.ind=TRUE) 
  dataset2 <- dataset[notNA, ]
  
  #create index to separate training and testing data
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
  yhat.lm <- round(predict(model1,
                           model.data,
                           type = 'response'))
  
  #compare yhat (predicted y) and testy (the actual y) for accuracy
  accurate <- ifelse(yhat.lm == testy, 
                     1,
                     0)
  
  #calculate and return the error
  error <- 1 - sum(accurate)/length(accurate)
  
  return(error)
  
  #create a confusion matrix
}


#using titanic data
lm.function(titanic, "Survived", "Pclass")
lm.function(titanic, "Survived", "Age") 

#try with mtcars data
lm.function(mtcars, "am", "mpg")
lm.function(mtcars, "am", "cyl")



# Comparing the bagged and lm functions based on spread -----------------------------------

#If one runs the bagged function with the same input multiple times, 
#the error rate remains similar each time the function runs.

#If we run the bagging function a 1000 times to examine the error rate 
#the effects of passenger class on survival rate in the titanic dataset 
#with 25 bagged linear models, the average error rates is around 0.32.
#The code is below:
bagging.function(titanic, "Survived", "Pclass", 25)

bag <- NULL
for (i in 1:1000){
  bag[i] <- bagging.function(titanic, "Survived", "Pclass", 25)
}

summary(bag)
var(bag)

#When we look at the summary of the vector bag, which contains the error rates
#of running the bagging function a 1000 times, it shows us that the minimum is around
#0.25 and the maximum is around 0.39. The variance of the error rate is quite small, 
#of around 0.0004.


#We find that the error rates for the lm function is quite similar to the 
#bagging function. For the same example as above, when we run the lm function 
#a 1000 times, the average error rate is also around 0.32.
lm.function(titanic, "Survived", "Pclass")

lm <- NULL
for (i in 1:1000){
  lm[i] <- lm.function(titanic, "Survived", "Pclass")}

summary(lm)
var(lm)

#When we look at the summary of the vector lm, it shows us that the minimum is around
#0.24 and the maximum is around 0.39, which is very similar to the range of error rates
#of the bagging function. The variance of the error rate is also around 0.0004, 
#which is very similar to the bagging function.

# The improvement in variance from using bagging on a linear regression when compared to 
# a regular linear regression model is this small because although bagging reduces variance,
# linear regression has low variance already.


# Works Cited:
# https://towardsdatascience.com/understanding-the-effect-of-bagging-on-variance-and-bias-visually-6131e6ff1385




# Replicating functions to produce confusion matrices (CM) ---------------------

# Bagging and LM CM list ---------------------------------------------------------
# To gain futher insight on the differences between our bagging and linear models, we create a function that
# generates a dataframe of bagging and linear predicted values, as well as the "true" values for a given dataset.
# This is a modified version of both of our previous models.

cm.function <- function(data, yvar, xvar, n){
  #Inputs:
  #data: dataset to use, either built-in on R or in the environment
  #yvar: outcome variable
  #xvar: predictor variable
  #n: number of bagged linear models user wants
  #Output: list of preditions and true results (to be used to build a confusion matrix)
  
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
  
  #now, create an LM model
  model.data <- train
  
  model1 <- lm(model.data[[yvar]] ~ model.data[[xvar]],
               data = model.data)
  
  #predict yhat.lm, the predicted outcome, for testing data
  model.data <- test
  
  yhat.lm <- round(predict(model1,
                           model.data,
                           type = 'response'))
  
  output <- data.frame(yhat, yhat.lm, testy)
  
  return(output)
  
}

#run cm.function with the "titanic" dataset, 
results <- cm.function(titanic, "Survived", "Pclass", 25)

#isolate each column
yhat <- results[,1]
yhat.lm <- results[,2]
testy <- results[,3]

#create BAGGING confusion matrix
bagging.confusion <- table(yhat,testy)
confusionMatrix(bagging.confusion, mode = "everything")

#create LM confusion matrix
lm.confusion <- table(yhat.lm,testy)
confusionMatrix(lm.confusion, mode = "everything")

#Plot These Results
model.data <- data.frame(yhat,
                         yhat.lm,
                         obs = 1:length(yhat.lm))

gathered.model.data <- model.data %>%
  gather(key = "model",
         value = "pred",
         -obs)

gathered.model.data %>%
  filter(obs <= 50) %>%
  ggplot(aes(x = model,
             y = obs)) +
  geom_tile(aes(fill = factor(pred)),
            color = "black")


#Now, we can make accuracy plots
model.data.2 <- data.frame(bag = yhat == testy,
                           lm = yhat.lm == testy,
                           obs = 1:length(yhat.lm))

gathered.model.data.2 <- model.data.2 %>%
  gather(key = "model",
         value = "pred",
         -obs)

gathered.model.data.2 %>%
  filter(obs <= 50) %>%
  ggplot(aes(x = model,
             y = obs)) +
  geom_tile(aes(fill = factor(pred)),
            color = "black")


# Comparison based on CM --------------------------------------------------


#Creating the confusion matrices for both models allows us to compare what factors
# may be underlying any differences in accuracy.  In the case of our "titanic" dataset,
# we find that both models exhibit identical accuracies; they predicted exactly the same
# outcomes for all observations in the sample.  To confirm that this is not due to an error 
# in our coding, we re-ran our cm.function WITHOUT rounding either model's predictions.
# The unrounded predictions differ between algorithms, but this difference is very small.
# Most importantly, both algorithms always predict on the same side of the 0.5 threshold.
# As long as both algorithms round their predictions in the same direction for each observation,
# their accuracies will remain identical. Both algorithms are correct approximately 67% of the time.
# They correctly predict the "positive" class (An individual dying) 83% of the time. 


# Additionally, we visualize these results in two plots.  The first confirms our findings that both models
# predict the same outcome in all cases.  As expected, we see a higher frequency of predicted "positive" 
# outcomes (Survived = 0).  We generate a second graph, which compares the accuracies of the two models.
# As our confusion matricies showed, the predicted values between both models with be true or false for
# the same observation.  Given the identical accuracies of our two models, these graphics do not add
# much additional information to our understanding.  However, for a different dataset, we might use these
# graphics to compare differences in sensitivity bewtween our models.  

