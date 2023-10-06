
library(readxl)
#Import data set under environment. For xlsx its "from excel". 
#For CSV ITS "From text (Readr)

Butler_Trucking <- read_excel("Butler_Trucking_Multiple_Regression.xlsx")

head(Butler_Trucking)


library(ggpubr) #helps us to create results that are publication ready for your report
library(tidyverse) #for data manipulation and visualization
library(broom) #tidy model output
library(gclus) #Allows us to have our scatterplot with correlation embedded in it
theme_set(theme_pubr())

summary(Butler_Trucking)


Butler_Trucking <- rename(Butler_Trucking, "Travel Hours"="Travel hours")

Butler_Trucking

#See multiple scatter plots
pairs(~`Travel Hours`+ `Miles Traveled` + `Number of Deliveries`, data=Butler_Trucking)

#See just one of the plots bigger
plot(Butler_Trucking$`Travel Hours`,Butler_Trucking$`Miles Traveled`,main="Scatter Diagram")

#If I want to remove assignment column use this code:
Butler_Trucking<-subset(Butler_Trucking,select=-c(Assignment))


correlation <- abs(cor(Butler_Trucking)) # Correlation in absolute value
correlation #Outputs the correlation results
colors <- dmat.color(correlation) #Gives different color coding to the correlation result
order <- order.single(correlation) #orders the result

#Visualize the new correlation plot

cpairs(Butler_Trucking, order, panel.colors = colors, gap = 0.5, main = "Sorted and colored variables by correlation") 



#Step 3: Building the model
#Regression equation: Travel Hours = b0 + b1*Miles Traveled + b2*Number of Deliveries
model <- lm(`Travel Hours` ~ `Miles Traveled` + `Number of Deliveries`, data = Butler_Trucking)
model

#Step 4: Model Summary
summary(model)



#Model Accuracy
#Adjusted R-square: Our example gives an adj R2 of 0.8763 (87.63 %) which is very good. 

#Let's look at only the coefficients
summary(model)$coefficient #outputs the coefficients
confint(model) #outputs the 97.5% CI of the coefficients

#Making result tidy using the "tidy()" function from the broom package
tidy(model) #gives us a nice and tidy output


#Predictions
model$fitted.values

#One way of seeing residuals 
model$residuals

# Calculate ANOVA components

#Sum Square Regression 

ssregression <- sum((model$fitted.values-mean(Butler_Trucking$`Travel Hours`))^2)  # Explained Sum of Squares
ssregression

#Sum Square Residual Error
ssresidualerror <- sum(model$residuals^2) 
ssresidualerror

#Total Summ of Square (or just ssregression+ssresidual)
total_ss <- sum((Butler_Trucking$`Travel Hours` - mean(Butler_Trucking$`Travel Hours`))^2)  
total_ss


# Calculate Mean Squares and F value. 
#Assuming you have an intercept, the number of predictors (p) is length(model$coefficients)-1
mse_regression <- ssregression / (length(model$coefficients)-1)
mse_residual <- ssresidualerror / model$df.residual
f_value <- (mse_regression / mse_residual)

#Calculate p-value pf the f distribution with 1-p value(f value, degrees of freedom numerator (number of predictors),
#degrees of freedom denominator (n-number of predictors-1)).This equation will work for the f distribution p value of any
#model, so long as you have an intercept (bcuz I calculate number of predictors with coeffiecients -1). Well that is assuming
# its not diffrent for more complex categorical variables for like 3 categories where a category is x1 and x2 being 0. 
p_value <- 1 - pf(f_value, length(model$coefficients)-1, length(model$fitted.values)-(length(model$coefficients)-1)-1)



# Create the ANOVA table
anova_table <- data.frame(
  Source = c("Regression", "Residual Error", "Total"),
  df = c(length(model$coefficients)-1, model$df.residual,length(model$coefficients)-1+model$df.residual),
  `Sum Sq` = c(ssregression, ssresidualerror, total_ss),
  `Mean Sq` = c(mse_regression, mse_residual, NA),
  `F value` = c(f_value, NA, NA),
  `Pr(>F)` = c(p_value, NA, NA)
)
# Print the ANOVA table
print(anova_table)

#This is the real anova table

#4.007687e-05 is 0.00004007687





#Step 5: Predicting values for our test set


Butler_Trucking2 <- read_excel("Butler_Trucking_Multiple_Regression.xlsx", 
                                                  sheet = "Sheet2")
head(Butler_Trucking2)

#Rename columns to the same 
Butler_Trucking2<-rename(Butler_Trucking2, "Miles Traveled"=Miles_traveled,"Number of Deliveries"=Number_of_deliveries)

#This is the new test data we want to predict Travel Hours with our estimate Multiple LR model

prediction <- predict(model, newdata = Butler_Trucking2) 

#This is how you input the new data into the estimate MLR model
prediction #Outputs the new prediction results, prediction 1 then prediction 2



#Step 6: Checking the model assumptions

#6a: Residual Analysis
#Get the predicted values of y rounded to 3 decimal places

truckingmodel_pred <- round(fitted(model), 3) 

#Create a dataframe for the results (i.e., put the results in a table)

truckingmodel_pred

travelhours_predictions <- data.frame(truckingmodel_pred) 

travelhours_predictions #Outputs the fitted results based on our estimated MLR model

#Get the residual values (y - y_hat) (i.e., observed values - predicted values)
#"resid" is the function that extracts the residuals from the model

trucking_res <- round(resid(model), 3) 

#create a dataframe for the results (i.e., put the results in a table)
truckingres_df <- data.frame(trucking_res) 

#Output the residual values

truckingres_df

#Get the standardized residual values with textbook formula (see Chapter 15.8 for the equation) 
#the "rstandard" function gives us the standardized residual
trucking_stdres <- round(rstandard(model), 3)  


#and we put it in a table
truckstandardizedres_table <- data.frame(trucking_stdres) 
truckstandardizedres_table #output of standard residuals

#Make travel hours into a dataframe and rename the column
travel_hours<-data.frame(Butler_Trucking$`Travel Hours`)
travel_hours
travel_hours<-rename(travel_hours,`Actual Travel Hours`=Butler_Trucking..Travel.Hours.)
travel_hours

#Put all the calculated values into a residual plot table
Res_table <- data.frame(c(travel_hours,  travelhours_predictions, truckingres_df, truckstandardizedres_table))
Res_table



#Get the studentized deleted residuals

trucking_studentized_deleted_res <- round(rstudent(model), 3)

trucking_studentized_deleted_res


#For the ith observation deleted, the data set is n-1, so in the case the error sum of squares
# has (n-1)-p-1 degress of freedom. So for this butler trucking that would be 6 d.f., and 
#at a 0.05 level of significance the t distrubtion of t0.025 with the d.f. is 2.447.
#SO make sure that the studentized deleted residuals is not less than -2.447 or more than 2.447
#Because if so it is an outlier.

#This would have been a faster way of updated the table together, 
#just df$columnname<- round(rstudent(model),3) 

Res_table$Studentized_Deleted_Res<-trucking_studentized_deleted_res

Res_table

#Check the leverage of the observations, and make sure none is greater than 
#3(p+1)/n which in this case 3(3)/10 equals 0.9

leverage_values <- hatvalues(model)

leverage_values

# Check if any leverage value is greater than 0.9 with code instead of visually:
any_leverage_gt_0.9 <- any(leverage_values > 0.9)

any_leverage_gt_0.9

#Displays true if there are any, false if not

#Although there are none, if there were to filter for it when leverage_values is a vector: 
leverage_values

filteredleverage <- leverage_values[leverage_values > 0.9]

#But it will say named numeric(0) if there is none
filteredleverage

#To filter it as a dataframe
leverage_values<-data.frame(leverage_values)

leverage_values

library(dplyr)

anyatall<-filter(leverage_values, leverage_values>0.9)

anyatall

# Make sure no cooks distance is greater than 1, or else it is an influential observation.
cooks_distance_values <- cooks.distance(model)

cooks_distance_values

#Get all the residual plots (i.e., diagnostic plots)
#To get all the plots in a 2 X 2 single page run this code (erase the #) and then the plot(model) line

#layout(matrix(c(1,2,3,4), 2,2)) 

#To get only 1 plot per page run this code
layout(matrix(c(1,2,3,4), 1,1)) 
plot(model) #plots the residual vs fitted plot


#Now click inside of the console next to where it says "Hit return to see next plot" 
#and then click enter four times to see the four plots

#You can manually make the normal probability plot from the textbook (standardized residuals vs normal scores)
#by using the code from Armands pizza, but you can also just see from the Normal Q-Q that no standardized residual
#is less than -2 or more than +2 so no outlier is detected that way.


