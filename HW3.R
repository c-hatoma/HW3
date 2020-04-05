# Let's get started on this homework, eh?

library(tidyverse)
library(rpart)
library(rattle)

#begin with the titanic dataset
titanic <- read_csv(file.choose())


# have a dataset ready to use


bagging.function <- function(data, yvar, xvar, n){

error <- NULL
dataset <- data.frame(data)

notNA <- which(is.na(dataset[[xvar]]) == FALSE & is.na(dataset[[yvar]]) == FALSE, arr.ind=TRUE) 
dataset2 <- dataset[notNA, ]


  for(i in 1:n){
    
    indexes <- sample(1:nrow(dataset2), 
                      size = nrow(dataset2),
                      replace = TRUE)
    
    train <- dataset2[indexes, ]
    test <- dataset2[-indexes, ]
    
    meanx <- mean(train[[xvar]])
    meany <- mean(train[[yvar]])
    
    x <- train[[xvar]]
    y <- train[[yvar]]
    
    testx <- test[[xvar]]
    testy <- test[[yvar]]
    
    beta1 <- sum((x - meanx)*(y - meany)) / sum((x - meanx)^2)
    
    beta0 <- meany - beta1*meanx
    
    yhat <- round(beta0 + beta1*testx)
    
    compare.y <- data.frame(yhat, testy)
    
    accurate <- ifelse(compare.y$yhat == compare.y$testy, 
                       1,
                       0)
    
    error[i] <- 1 - sum(accurate)/length(accurate)
  }
    
    return(mean(error))}



bagging.function(titanic, "Survived", "Pclass", 25)
bagging.function(titanic, "Survived", "Age", 25)


#try with mtcars data
bagging.function(mtcars, "am", "mpg", 25)
bagging.function(mtcars, "am", "cyl", 25)












