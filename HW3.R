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


training.sample.1.Y <- training.sample.1[,2] #survived
training.sample.1.X <- training.sample.1[,3] #pclass

training.s1.matrix <- cbind(training.sample.1.Y, training.sample.1.X)






tree.test <- rpart(factor(Survived) ~ factor(Pclass),
                   data = training.sample.1)

fancyRpartPlot(tree.test)

prediction <- predict(tree.test,
        data.frame(Pclass = "3"))

rounded.predict <- round(prediction[,0])



bagging.m1 <- bagging(factor(Survived) ~ Age + factor(Pclass) + Sex + SibSp,
                      data = titanic,
                      coob = TRUE)