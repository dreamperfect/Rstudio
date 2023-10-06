#BUS2 194A, Chapter 15: Multiple Linear Regression - RStudio Lab Session

#Install the following packages
#install.packages("readxl") #This should be done once and then delete it
#install.packages("gclus") 

#Load the packages into R
library(ggpubr) #helps us to create results that are publication ready for your report
library(tidyverse) #for data manipulation and visualization
library(broom) #tidy model output
library("readxl") #Will help us to read .xlsx files
library(gclus) #Allows us to have our scatterplot with correlation embedded in it
theme_set(theme_pubr())

#Step 1: Import data into RStudio

library(readxl)
Sal_df<- read_excel("MLR_Example.xlsx")



#we use the read_excel to call excel files directly into R
head(Sal_df) #Gives us a snapshot of the excel file 
glimpse(Sal_df) #Gives us the structure of the data. E.g., number of rows and column. Also the type of variables (categorical or integer)

#Step 2: Descriptive statistics & data visualization
summary(Sal_df) #This gives the descriptive statistics of the data

#Scatterplot, Correlation coefficient & visualization
pairs(~Experience + Testscore + Salary, data = Sal_df) #Simple correlation plot

corr <- abs(cor(Sal_df)) # Correlation in absolute value
corr #Outputs the correlation results
colors <- dmat.color(corr) #Gives different color coding to the correlation result
order <- order.single(corr) #orders the result

cpairs(Sal_df, order, panel.colors = colors, gap = 0.5, main = "Sorted and colored variables by correlation") 
#Visualize the new correlation plot

#Step 3: Building the model
#Regression equation: Salary = b0 + b1*Experience + b2*Testscore
model <- lm(Salary ~ Experience + Testscore, data = Sal_df)
model

#Step 4: Model Summary
summary(model)

#Model Accuracy
#Adjusted R-square: Our example gives an adj R2 of 81% which is very good.
#Let's look at only the coefficients
summary(model)$coefficient #outputs the coefficients
confint(model) #outputs the 95% CI of the coefficients

#Making result tidy using the "tidy()" function from the broom package
tidy(model) #gives us a nice and tidy output

#Let's look at the analysis of variance within the data
model_aov <- anova(model) #extract the anova results from regression
model_aov



#Step 5: Predicting values for our test set


Sal_df2<- read_excel("MLR_test.xlsx")

Sal_df2


#This is the new test data we want to predict salary with our estimate MLR model
prediction <- predict(model, newdata = Sal_df2) 
#This is how you input the new data into the estimate MLR model
prediction #Outputs the new prediction results

#Step 6: Checking the model assumptions
#Residual Analysis
#Get the predicted values of y
Sal_fit <- round(fitted(model), 2) 

#gives the predicted y values based on our data. The "round" function helps us to put it at 2 decimal places
Salfit_df <- data.frame(Sal_fit) #Create a dataframe for the results (i.e., put the results in a table)
Salfit_df #Outputs the fitted results based on our estimated MLR model

#Get the residual values (y - y_hat) (i.e., observed values - predicted values)
Sal_res <- round(resid(model), 2) #"resid" is the function that extracts the residuals from the model
Salres_df <- data.frame(Sal_res) #create a dataframe for the results (i.e., put the results in a table)
Salres_df #gives the residual values

#Get the standardized residual values (see Chapter 15.8 for the equation)
Sal_stdres <- round(rstandard(model), 2) #the "rstandard" function gives us the standardized residual 
Salstdres <- data.frame(Sal_stdres) #and we put it in a table
Salstdres #output of standard residuals

#Put all the calculated values into a residual plot table
Res_table <- data.frame(c(Salfit_df, Salres_df, Salstdres))
Res_table

#Get all the residual plots (i.e., diagnostic plots)
#layout(matrix(c(1,2,3,4), 2,2)) #Helps us to get all the plots in a 2 X 2 single page
layout(matrix(c(1,2,3,4), 1,1)) #Helps to get only 1 plot per page
plot(model) #plots the residual vs fitted plot




########################################       New Section       ####################################################
#Chapter 15.7 - Categorical Independent Variables

#Step 1: Import data into RStudio


Sal_dfsheet2 <-read_excel("MLR_Example.xlsx", 
                     sheet = "Sheet2")



#the sub-function "sheet" helps us to call a second excel sheet
head(Sal_dfsheet2) #Gives us a snapshot of the excel file 
glimpse(Sal_dfsheet2) 

#Gives us the structure of the data. E.g., number of rows and column. Also the type of variables (categorical or integer)

#Step 2: Descriptive statistics & data visualization
summary(Sal_dfsheet2) #This gives the descriptive statistics of the data

#Step 3: Convert the "Degree" variable into dummy variables (0=No degree, 1=degree)
Sal_dfsheet2$Degree <- ifelse(Sal_dfsheet2$Degree == 'Yes', 1, 0) 

#the ifelse function enables us to convert the Degree column into dummy variables
head(Sal_dfsheet2) #Outputs a snapshot of our new columns

#Step 4: Scatterplot, Correlation coefficient & visualization
pairs(~Experience + Testscore + Degree + Salary, data = Sal_dfsheet2) #Simple correlation plot

corr <- abs(cor(Sal_dfsheet2)) # Correlation in absolute value
corr #Outputs the correlation results

#Step 5: Building the model
#Regression equation: Salary = b0 + b1*Experience + b2*Testscore
model <- lm(Salary ~ Experience + Testscore + Degree, data = Sal_dfsheet2)
model

#Step 6: Model Summary
summary(model)


