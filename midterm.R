# Brett Waugh
# 21 February 2019
# midterm.questions.R
# Midterm project. 

#1. Set working directory.
setwd("/home/brett")

#2. Download the College.csv data in your working directory. Read the data using read.csv function, and save it as data.
require(readr)
data <-College <- read_csv("~/Downloads/College.csv")

#3. Find out the details about College data. Understand each variables.
?College

#4. Print the first ten rows of the data. Explore the data.
head(College, 10)

#5. Require libraries: "dplyr", "ISLR", "boot", "caret".
require(dplyr)
require(ISLR)
require(boot)
require(caret)

#6. Drop private schools, and save it as "public". how many rows does public include?
public <- data[College$Private == "No",]
nrow(public)

#7. Drop the first two columns because they are character values and we do not need them for linear modeling.
public <-public[ , 3:19]
public

#8. Set seed to "1"
set.seed(1)

#9. Create train and test set. First, create set object by spliting 2/3 of the data based on Grad.Rate.

set <-createDataPartition(public$Grad.Rate, p=2/3, list=F)
nrow(set)

# 10. Create train and test set.
train <-public[set,]
test <-public[-set,]

#11. Fit regression model using the train set.
model <-lm(train, data=public)

#12. Five fold cross validation because our sample size is small. First, create ControlParameters. set method="repeatedcv", use five repeats.

ControlParameters <-trainControl(method="repeatedcv", 
                                 number=5,
                                 repeats=5)

#13. Use train function for five fold cross validation.
modellm <-train(Grad.Rate~., 
                data=train,
                method="lm",
                trControl= ControlParameters
                )
#14. Print the model on the console. What is the value of RMSE?
modellm
trainRMSE <- 13.03708

#15. Predict on the new data (test data).
p <- predict(modellm, test)
p

#16. Compute errors: error.
error <- p - test$Grad.Rate

#17. Calculate RMSE.
testRMSE <- sqrt(mean(error^2))

#18. Compare modellm and RMSE on test set.
trainRMSE
testRMSE

#19. Why is the test set RMSE higher than the training set RMSE?


# 20. Predict on full College dataset. Is the full data RMSE lower than the test set result?
p_full <- predict(modellm, College)
error_full <- p_full - College$Grad.Rate
summary(modellm)
fullRSME <- sqrt(mean(error_full^2))
fullRSME