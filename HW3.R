# Let's get started on this homework, eh?

library(tidyverse)
library(rpart)
library(rattle)

#begin with the titanic dataset
titanic <- read_csv(file.choose())

#create indexes 
indexes <- sample(1:nrow(titanic), 
                  size = nrow(titanic),
                  replace = TRUE)

training.sample.1 <- titanic[indexes, ]
testing.sample.1 <- titanic[-indexes, ]


mean.x <- mean(training.sample.1$Pclass)
mean.y <- mean(training.sample.1$Survived)
x <- training.sample.1$Pclass
y <- training.sample.1$Survived

# mean.testx <- mean(testing.sample.1$Pclass)
# mean.testy <- mean(testing.sample.1$Survived)
testx <- testing.sample.1$Pclass
testy <- testing.sample.1$Survived

#Beta1 (slope)
beta1 <- sum((x - mean.x)*(y - mean.y)) / sum((x - mean.x)^2)

beta0 <- mean.y - beta1*mean.x

yhat <- round(beta0 + beta1*testx)

compare.y <- data.frame(yhat, testy)

accurate <- ifelse(compare.y$yhat == compare.y$testy, 
       1,
       0)

accuracy <- sum(accurate)/length(accurate)


rounded.predict <- round(prediction[,0])



bagging.m1 <- bagging(factor(Survived) ~ Age + factor(Pclass) + Sex + SibSp,
                      data = titanic,
                      coob = TRUE)