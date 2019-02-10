# Brett Waugh
# 10 February 2019
# Module 4 Lab 4: Logistic Regression
# from "Data Science and Big Data Analytics" 

###################################################
# Step 3: Set the Working Directory
###################################################
#set working directory

###################################################
# Step 4: Read in and Examine the Data
###################################################
Mydata <- read.csv("~/Downloads/survey.csv",header=TRUE,sep=",")

#Explore data
#MyDEPV means whether or not a customer buy the product.
table(Mydata$MYDEPV)
table(Mydata$Price,Mydata$MYDEPV)
summary(Mydata$Age)
cor.mat <- cor(Mydata[,-1])
cor.mat

###################################################
#Step 5: Build and Review the Logistic Regression Model
###################################################
mylogit <- glm(MYDEPV ~ Income + Age + as.factor(Price),
           data =Mydata, family=binomial(link="logit"),
           na.action=na.pass)
summary(mylogit)
# One unit change of Income, the log odds of Purchase increases by 0.12876 
# Compared with a Price of 10, Purchase decision at a Price 20 decreases the log odds of Purchase by 0.74418

#log odds ratio of purchase = (-6.02116)   + 0.12876*Income +0.03506*Age +  (-0.74418)*Price20 + (-2.21028)*Price30 
#logA = ax + by + c
# A = exp(ax + by +c)
###################################################
#Step 6: Review the Results and Interpret the Coefficients
###################################################
confint(mylogit)
#2.	To determine how much the odds-ratio changes per unit change of the variable, 
#raise e to the power of the coefficient of the variable.  
#The object you want to exponentiate is called coefficients and it is part of mylogit. 

exp(mylogit$coefficients)
##Odds ratio of purchase =  0.002426851 + 1.137416400*Income  + 1.035685762*Age + 0.475124932*Price20 + 0.109669885*Price30 

# You can observe that for every unit change in income, 
#the odd-ratio of Purchase increases by a multiplicative factor of 1.13
#This is actually a bit more intuitive than the log odds explanation you reviewed 
#in the previous step. Observe that that Age does not appear to be a very strong factor 
#in this model, 
#and the price factor of 30 has a stronger effect than a price factor of 20.

# Compute Pseudo R-squared
attributes(mylogit)  # get me the names of the 'class members'

#When analyzing data with a logistic regression, an equivalent statistic to R-squared does not exist.  
#The model estimates from a logistic regression are maximum likelihood estimates arrived at through an iterative process.  
#They are not calculated to minimize variance, so the OLS approach to goodness-of-fit does not apply.  
#However, to evaluate the goodness-of-fit of logistic models, we can use pseudo R-squareds. 

1- with(mylogit, deviance/null.deviance)
#Psedou R squred values from 0.2-0.4 indicate excellent model fit

###################################################
#Step 7: Visualize the Model
###################################################
plot(mylogit)



###################################################
#Step 8: Use relevel Function to re-level the Price
#   factor with value 30 as the base reference
###################################################
#We will now change the reference level at price point 30
# 
Mydata$pricefactor = relevel(as.factor(Mydata$Price), "30")
 
mylogit2 = glm(MYDEPV ~ Income + Age + pricefactor  ,
             data= Mydata,family=binomial(link="logit"), na.action=na.pass)
summary(mylogit2)



###################################################
#Step 9: Plot the ROC Curve
###################################################
install.packages("bitops")
library(bitops)
install.packages("caTools")
library(caTools)
install.packages("ROCR")
library(ROCR)
 
pred = predict(mylogit, type="response") # this returns the probability scores on the training data
predObj = prediction(pred, Mydata$MYDEPV) # prediction object needed by ROCR
 
?performance
 
rocObj = performance(predObj, measure="tpr", x.measure="fpr")
aucObj = performance(predObj, measure="auc") 
auc = aucObj@y.values[[1]]  
auc  
 
# # plot the roc curve
plot(rocObj, main = paste("Area under the curve:", auc))

# ###################################################
# #Step 10: Predict Outcome Given Age and Income
# ###################################################
Price <- c(10,20,30)
Age <- c(mean(Mydata$Age))
Income <- c(mean(Mydata$Income))
newdata1 <- data.frame(Income,Age,Price)
newdata1
 
newdata1$PurchaseP <- predict (mylogit,newdata=newdata1,type="response")
newdata1

###################################################
# #Step 11: Predict Outcome for a Sequence of Age Values 
# #         at price30 and Mean Income
# ###################################################
newdata2 <- data.frame(Age=seq(min(Mydata$Age),max(Mydata$Age),2),
                        Income=mean(Mydata$Income),Price=30)
newdata2$AgeP<-predict(mylogit,newdata=newdata2,type="response")
cbind(newdata2$Age,newdata2$AgeP)
plot(newdata2$Age,newdata2$AgeP)
 
# ###################################################
# #Step 12: Predict Outcome for a Sequence of Income 
# #         at price30 and Mean Age 
# ###################################################
newdata3 <- data.frame(Income= seq(20,90,10),Age=mean(Mydata$Age),Price=30)
newdata3$IncomeP<-predict(mylogit,newdata=newdata3,type="response")
cbind(newdata3$Income,newdata3$IncomeP)
plot(newdata3$Income,newdata3$IncomeP)
 
# ###################################################
# #Step 13: Use Logistic Regression as a Classifier
# ###################################################
newdata4 <- data.frame(Age= round(runif(10,min(Mydata$Age),max(Mydata$Age))), 
                         Income= round(runif(10,min(Mydata$Income),max(Mydata$Income))),
                         Price = round((runif(10,10,30)/10))*10)
newdata4$Prob <- predict(mylogit,newdata=newdata4,type="response")
newdata4