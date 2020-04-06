# Let's get started on this homework, eh?

library(tidyverse)
library(rpart)
library(rattle)

#begin with the titanic dataset
titanic <- read_csv(file.choose())

#Bagging Function
bagging.function <- function(data, yvar, xvar, n){
#Inputs:
  #data: dataset to use, either built-in on R or in the environment
  #yvar: outcome variable
  #xvar: predictor variable
  #n: number of bagged linear models user wants
#Output: mean percentage of error
  
  #create a null vector of errors for each of the models
  error <- NULL
  
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
      
      #vectors of x and y variables from the testing data
      testx <- test[[xvar]]
      testy <- test[[yvar]]
      
      #use training data to model yvar on xvar
      #calculate beta1, the slope
      beta1 <- sum((x - meanx)*(y - meany)) / sum((x - meanx)^2)
      
      #calculate beta0, the intercept
      beta0 <- meany - beta1*meanx
      
      #predict yhat, the predicted outcome, for testing data
      yhat <- round(beta0 + beta1*testx)
      
      #compare yhat (predicted y) and testy (the actual y) for accuracy
      accurate <- ifelse(yhat == testy, 
                         1,
                         0)
      
      #add to the error vector the error rate of this model
      error[i] <- 1 - sum(accurate)/length(accurate)
      }
  
  #calculate and return the mean error rate of all bagged linear models
  return(mean(error))
}


#using titanic data
bagging.function(titanic, "Survived", "Pclass", 25)
bagging.function(titanic, "Survived", "Age", 25)


#try with mtcars data
bagging.function(mtcars, "am", "mpg", 25)
bagging.function(mtcars, "am", "cyl", 25)


#LM FUNCTION COMPARISON
lm.function <- function(data, yvar, xvar){

model1 <- lm(yvar ~ xvar,
   data = data)

model1predictions <- predict(model1,
                             data)

model1predictions.rounded <- round(model1predictions)


comparison <- data.frame(data$Survived, model1predictions.rounded) %>%
  mutate(correct = ifelse(Survived == model1predictions.rounded, 1, 0))

error.lm <- 1 - mean(comparison$correct) 
















