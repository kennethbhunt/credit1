#Data set: credit.csv
#Your task is to predict the customers credit score (rating) using classification trees. The
#independent variables are: age, income, cars, education and carloans.

Credit <- read.csv('credit.csv', stringsAsFactors = F)

####Boosting Classification trees 

library(gbm)

n <-sample(2464,1200)
credit_train <- Credit[n, 1:6]
credit_test <-Credit[-n, 1:6]

Boost_fit <-gbm(rating~., data = credit_train, distribution = "bernoulli", 
                interaction.depth = 3, n.trees = 2000, shrinkage = 0.1)
summary(Boost_fit)

##Compute prediction accuracy in the Test set 

Boost_pred <- predict(Boost_fit, credit_test, n.trees = 2000, "response")
head(Boost_pred)
## Round the probabilities 
Boost_pred <- round(predict(Boost_fit, credit_test, n.trees = 2000, "response"))
head(Boost_pred)

mean(Boost_pred==credit_test$rating)
#81%

##Goodness of fit training set 
Boost_pred2 <- round(predict(Boost_fit, credit_train, n.trees = 2000, "response"))
head(Boost_pred)

mean(Boost_pred2==credit_train$rating)
#83.5%

##Find optimal shrinkage parameter 
k <- -10:-1
shrink <- 10 ^ k
vect_acc <- c()

for (j in shrink){
  Boost_fit <-gbm(rating~., data = credit_test, distribution = "bernoulli", 
                  interaction.depth = 3, n.trees = 2000, shrinkage = j)
  
  Boost_pred <- round(predict(Boost_fit, credit_test, n.trees = 2000, "response"))
  
  acc <- mean(Boost_pred==credit_test$rating)
  
  vect_acc <- c(vect_acc, acc)
}

which.max(vect_acc)
max(vect_acc)
shrink[which.max(vect_acc)]

####RF Regression Tree

library(randomForest)
RF_fit <-randomForest(factor(rating)~., data = credit_train, mtry=2)
str(RF_fit)

##Compute prediction accuracy in the test set 
RF_pred <-predict(RF_fit, credit_test)
head(RF_pred)

mean(RF_pred==credit_test$rating)
#80% accuracy 

##Compute prediction accuracy in the training set 
RF_pred2 <-predict(RF_fit, credit_train)
head(RF_pred2)

mean(RF_pred2==credit_train$rating)
#82% accuracy 

## Grow the classification tree with rpart()
##rpart() has build it cross validation, 10 fold cv 

fit <- rpart(rating~., data=credit_train, method="class")

#Plot the tree
prp(fit)

rpart.plot(fit)

## Print the complexity paremeter table 
printcp(fit)

RF_pred2 <-predict(RF_fit, credit_train, type="class")
head(RF_pred2)
mean(RF_pred2==credit_train$rating)
#82%

#Accuracy in the test set 
RF_pred3 <-predict(RF_fit, credit_test, type="class")
head(RF_pred3)
mean(RF_pred3==credit_test$rating)
# 80%









