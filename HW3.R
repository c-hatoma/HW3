# Let's get started on this homework, eh?

library(tidyverse)
library(rpart)
library(rattle)
library(caret)

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
  #return(mean(error))
  
  #create a confusion matrix
  bagging.cm <- table(yhat, testy)
  return(confusionMatrix(bagging.cm, mode = "everything"))

}

#using titanic data
bagging.function(titanic, "Survived", "Pclass", 25)
bagging.function(titanic, "Survived", "Age", 25)


#try with mtcars data
bagging.function(mtcars, "am", "mpg", 25)
bagging.function(mtcars, "am", "cyl", 25)

#_____________________________________________________________________


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
        yhat.lm <- round(predict(model1,
                        model.data,
                        type = 'response'))
        
        #compare yhat (predicted y) and testy (the actual y) for accuracy
        accurate <- ifelse(yhat.lm == testy, 
                           1,
                           0)
        
        #calculate and return the error
        #error <- 1 - sum(accurate)/length(accurate)
  
  #return(error)
        
        #create a confusion matrix
        linear.cm <- table(yhat.lm, testy)
        return(confusionMatrix(linear.cm, mode = "everything"))
        
}


#using titanic data
lm.function(titanic, "Survived", "Pclass")
lm.function(titanic, "Survived", "Age") 

#try with mtcars data
lm.function(mtcars, "am", "mpg")
lm.function(mtcars, "am", "cyl")




#Similarities: 



#Differences:
#One of the differences was that the error rate for the bagged function is quite stable, 
#while the error rate for the lm function has a lot of variations.
#If one runs the bagged function with the same input various times, 
#the error rate remains similar each of the time that the function is run.
#For example, the error rate for examining the effects of passenger class on survival rate
#in the titanic dataset with 25 bagged linear models, the error rate is stable around 0.32.
#The code is below:
bagging.function(titanic, "Survived", "Pclass", 25)
#However, the lm function returns a wider range of error rates. 
#For the same example as above, the lm function sometimes returns an error rate of 0.29
#while sometimes it returns an error rate as high as 0.36. The code is below:
lm.function(titanic, "Survived", "Pclass")




#Bagging Function
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
  
  #predict yhat.lm, the predicted outcome, for testing data
  yhat.lm <- round(mean.beta0 + mean.beta1*testx)
  
  #compare yhat.lm (predicted y) and testy (the actual y) for accuracy
  accurate <- ifelse(yhat.lm == testy, 
                     1,
                     0)
  
  #add to the error vector the error rate of this model
  error <- 1 - sum(accurate)/length(accurate)
  
  #calculate and return the mean error rate of all bagged linear models
  return(mean(error))
}

bag <- NULL
for (i in 1:1000){
  bag[i] <- bagging.function(titanic, "Survived", "Pclass", 25)
}

bag100 <- NULL
for (i in 1:1000){
  bag[i] <- bagging.function(titanic, "Survived", "Pclass", 100)
}

lm <- NULL
for (i in 1:1000){
  lm[i] <- lm.function(titanic, "Survived", "Pclass")}








##--------------------------------------------------TEST for a yhat function

#YHAT FUNCTION (BAGGING)
yhat.function <- function(data, yvar, xvar, n){
  #Inputs:
  #data: dataset to use, either built-in on R or in the environment
  #yvar: outcome variable
  #xvar: predictor variable
  #n: number of bagged linear models user wants
  #Output: yhat for the bagged model 
  
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
  }
  
  #return bagging.cm
  return(yhat)
  
}

#TEST w titanic data
yhat <- yhat.function(titanic, "Survived", "Pclass", 25)

#HOW would i create a confusion matrix with this?? 
  linear.cm <- table(yhat.lm, testy) #<- how could i get "testy"
return(confusionMatrix(linear.cm, mode = "everything"))

 
   
#YHAT.LM.FUNCTION
yhat.lm.function <- function(data, yvar, xvar){
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
    yhat.lm <- round(predict(model1,
                             model.data,
                             type = 'response'))
    list <- list(yhat.lm, testy)

    return(list)
    
  }
  
  #using titanic data
test <- yhat.lm.function(titanic, "Survived", "Pclass")

test.2 <- data.frame(test)

#Now, build a confusion matrix from this 
yhat <- test.2[,1]
testy <- test.2[,2]

lm.confusion <- table(yhat,testy)

confusionMatrix(lm.confusion, mode = "everything")














