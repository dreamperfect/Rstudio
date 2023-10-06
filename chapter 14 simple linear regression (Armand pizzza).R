#Chapter 14: Simple linear regression

library(readr)
library(ggplot2)

Arm_df <- read_csv("Armand_Pizza_sheet1.csv")
View(Arm_df)

library(ggpubr)
library(tidyverse)

theme_set(theme_pubr())

head(Arm_df) #gives snapshot of file


#Descriptive statistics

summary(Arm_df)

plot(Arm_df$Population, Arm_df$Sales,main="Scatter Diagram (thousands)", xlab="Student Population",
     ylab="Quarterly Sales",pch=19)


#The mean of students on campus is 14k with a max of 26k and min of 2k.
#The graph above suggests a linearly increasing relationship between the sales and the
#student population variables. This is a good thing, because one important assumption of
#the linear regression is that the relationship between the outcome and predictor variables 
#is linear and additive. 

#We can compute the correlation coefficient between the two variables using R function cor()

cor(Arm_df$Population,Arm_df$Sales)

#cor(Arm_df$Sales,Arm_df$Population)
#I guess order doesnt matter between dependent and independent for this ^



#Theres a strong correlation between population and sales (95.01%)

#Build regression model

model<-lm(Sales~Population,data=Arm_df)

model

#Interpretation: sales (y hat) = 60 +5*population (x). ( yhat=b0 + b1(x) ) This is the 
#estimated regression equation.The intercept (b0) is 60. It can be interpreted as the predicted 
#sales unit for zero students on campus. THe slope (b1) is +5, implying that as a student pop 
#increases sales increases. 

# Make predictions using the model, with this data set, or another dataset with the same column names
predictions <- predict(model, newdata = Arm_df)

# Print the predictions
print(predictions)

#Or see the predictions from this dataset.
model$fitted.values

#We visualize the regression line. 

ggplot(Arm_df, aes(Population, Sales))+
  geom_point()+ 
  stat_smooth(method=lm, se=FALSE)


#SE is used to put of the confidence bands of the regression line.

#Step 4: Model assessment (Ensures that our regression model is statistically significant)

#Step 4a: Model summary

summary(model)

#Multiple R Squared is the coefficient of determination/goodness of fit. 90.27% of variability in sales can be explained by the
#linear relationship between the size of the student population and sales. That is assuming the model assumptions are true. 

#Model Asssumptions: 1. The error term Epsilon is a random variable with a mean or expected value of 0.
#2. The varieance of epsilon, denoted by variance is the same for all values of x. Implication: the variance of y about
#the regresson line is the same for a ll values of x.
#3. The values of epsilon are independent.Implication: the value of epsilon for a particular value of x is not related to the value
#of epsilon for any other value of x, thus the value of y for a particular value of x is not related to the value of y for any other 
#value of x. #4. The error term epsilon is a normally distributed random variable. Implication: Because y is a linear function of Epsilon, y 
#is also a normally distributed random variable.


#The estimated standard deviation of b1 is the  "residual standard error"
#or "standard deviation"/ Sqrt of ( ( sum of Xi-sample mean)^2)
# Or 13.83/ sqrt of (568). Which equals 0.5803.
#The test statistic is the one next to population, 8.617. It is b1/estimated standard deviation of b1. 
#That is 5/0.5803 = 8.62 or 8.617. 
#Since that test statistic on the distribution table with n-2 (10-2)=8 Degrees of freedom, is 0.005 and this is a two tailed test
#so we double it by 2=0.01 (meaning its less than that) because the 0.005 is test statistic 3.355 and on excel it shows that the p value is 0.000
#Or in Rstudio it shows its 2.55e-05 which is 0.0000255. Since that is less than a=0.01, we reject Ho and conclude B1 is not equal to zero. This evidence is
#sufficient to conclude that a significant relationship exists between student population and quarterly sales. 



#Step 4b: Coefficient significance 
#Hypothesis testing for the coefficients.
#Ho: the coefficients are equal to zero (i.e there is no relationship between x and y)
#Ha: The coefficients are not equal to zero (i.e there is some relationship between x and y)

#Coefficients: We can see that b0 and b1 (population) coefficients are statistically 
#significant (p-value<0.05).

#In our example, both the p-value for the intercept and the predictor variables are highly
#significant, so we an reject the null hypothesis and accept the alternative hypothesis, which
#means that there is a significant association between the predictor and the outcome variables.

#Standard error: measures the variability or accuracy of the beta coefficients and can be used to 
#compute the confidence interval.


#Extract the anova table from the model. Only works for linear regression. 

model_aov<-anova(model)
model_aov

# Calculate Total Sum of Squares

total_ss <- sum(model_aov$`Sum Sq`)
total_ss

#Calculate total Mean squares

total_ms<-sum(model_aov$`Mean Sq`)
total_ms<-data.frame(`D.F`=total_ms)
total_ms

#Calculate total Degrees of Freedom

total_df<-sum(model_aov$Df)
total_df<-data.frame(`D.F`=total_df)

total_df

#Combining with other degrees of freedom

degfree<-model_aov$Df

degfree<-data.frame(D.F=degfree)

degfree

degfreewithtotal <- bind_rows(degfree, total_df)

degfreewithtotal

#Make a complete anova table


Response <- c('Population', 'Residuals', 'Total')
Response


complete_anova_table<-data.frame(Response)

complete_anova_table
complete_anova_table$D.F<-degfreewithtotal

complete_anova_table

# Add other columns to the data frame

complete_anova_table$Sum.Sq <- c(model_aov$`Sum Sq`, total_ss)  # Use NA to match the length
complete_anova_table$Mean.Sq <- c(model_aov$`Mean Sq`, total_ms)  # Use NA to match the length
complete_anova_table$F.value <- c(model_aov$`F value`, NA)  # Use NA to match the length
complete_anova_table$Pr.F <- c(model_aov$`Pr(>F)`, NA)

complete_anova_table

# Just for further use, how to add a totals row when the number of columns are not the same

Arm_df

library(dplyr)

totals_row <- Arm_df %>%
  summarize(Population = sum(Population), Sales = sum(Sales)) %>%
  mutate(Restaurant = NA) # Use NA for the Restaurant column

totals_row

# Add the totals row to the original tibble
Arm_df_with_totals <- bind_rows(Arm_df, totals_row)

# Label the "NA" value in Restaurant as "Total"
Arm_df_with_totals$Restaurant[is.na(Arm_df_with_totals$Restaurant)] <- "Total"

# Print the tibble with the totals row
print(Arm_df_with_totals)

# Or another way to do this is: 
#Create a totals row
totals_row <- data.frame(
  `Restaurant` = 'Totals',
  `Population` = sum(Arm_df$Population),
  `Sales`=sum(Arm_df$Sales)
)
totals_row

# Combine ANOVA table and totals row
arm_df_with_totals <- rbind(Arm_df, totals_row)

arm_df_with_totals


# Residuals:provide a quick view of the distribution of the residuals, which by definition 
#should have a mean=zero. 
#Therefore the median should not be far from zero.
#In Rstudio the "Multiple R-Squared" is the excel "Multiple R", Residual Standard Error is the excel standard error.
#Multiple R from excel is not on here (not the same as Multiple R Squared)
 
residuals<-residuals(model)
print(residuals)

#Or another way

model$residuals

#Combine observed sales, predicted sales, residuals (difference) to one table

difference<-data.frame(Arm_df$Sales,predictions,residuals)


#Rename the columns in the difference table

difference<-rename(difference, Sales=Arm_df.Sales, Predictions=predictions, Residuals=residuals)

print(difference)


confint(model)  #95% confidence interval

#Interpretation: there is approximately a 95% chance that the interval [3.66,6.33] will contain
# the true value of b1.Where sales (y hat) = 60 +5*population (x). ( yhat=b0 + b1(x) )
#We use 5 as b1 but 95 percent chance that it is between 3.66 and 6.33 and 95
#percent chance bo is between 38.72 and 81.27
#Also since the confidence interval does not contain zero, we can reject Ho.


#Step 4c: Model Accuracy
#R square: The Rsquare ranges from 0 to 1 and represents the proportion of information 
#(i.e variation) in the data that can be explained by the model. Our example gives an Rsquare 
#of 90.3% which is very good. 


#Residuals against independent variable x (population)
plot(Arm_df$Population,residuals, main='Plot of Residuals Against Independent Variable (Poplulation)',xlab="Population (Thousands)",pch=19)

print(Arm_df$Population)

#Residualsl against predicted values 

plot(predictions,residuals, main='Plot of Residuals Against Predicted Values',xlab="Predicted Sales",pch=19)

#Getting standardized residuals (the same as in excel, which is residual/stdev of residuals)
#Although it is calculated differently in the textbook, you get the same shape of plot basically since its not much different.


residual_sd <- sd(residuals)
print(residual_sd)

# Calculate standardized residuals
standardized_residuals <- residuals / residual_sd

# Print the standardized residuals
print(standardized_residuals)

#Plotting standardized residuals vs Predicted values
plot(predictions,standardized_residuals, main='Plot of Standardized Residuals Against Predicted Values',xlab="Predicted Sales",pch=19)



library(dplyr)
print(standardized_residuals)

# Sort the standardized_residuals vector in ascending order
sorted_standardized_residuals <- standardized_residuals[order(+standardized_residuals)]

print(sorted_standardized_residuals)

library(stats)

#get the normal scores of a normal distribution, sample size 10, mean 0, stdev1
# Create a vector of 10 standard normal deviates
z <- rnorm(10)
# Calculate the normal scores
normal_scores <- qnorm(ppoints(10))
 
print(ppoints(10))

# Print the normal scores
print(normal_scores)


#Normal Probability Plot with Excel Formula Standardized Residuals (Residual/stdev of Residual)

plot(normal_scores, sorted_standardized_residuals, pch=19, xlab="Normal Scores", ylab="Standardized Residuals", main="Normal Probability Plot")
abline(a=0, b=1, lty=2)

help(abline)

help(plot)
#Another way to do this.

df1<-data.frame(normal_scores,sorted_standardized_residuals)

print(df1)

df2 <- data.frame(x=c(-2,0,2), y=c(-2,0,2))


ggplot(df1, aes(x=normal_scores, y=sorted_standardized_residuals)) + geom_point() + 
  geom_line(data=df2, aes(x=x, y=y), col="red") + labs(x="Normal Scores", y="Standardized Residuals", title="Normal Probability Plot")+ 
  theme(plot.title=element_text(hjust=0.5)) 

#You can break up long lines of code by leaving the + sign (assuming its part of the code) and 
#hitting enter to the next line.



# A third way of adding a line with a specific slope.
#If the intercept is set to 2, the line will pass through the points (0,2) and (1,3) (since slope is 1) 
#Intercept 0 is passing through the point (0,0)
ggplot(df1, aes(x = normal_scores, y = sorted_standardized_residuals)) +
  geom_point(pch = 19) +
  geom_abline(aes(slope = 1, intercept = 0)) + 
  labs(x="Normal Scores", y="Standardized Residuals", title="Normal Probability Plot")

#Since no standardized residual is less than -2 or more than +2, no outlier is detected that way.
#The other way to detect outliers is with the studentized deleted residuals, you can use the code from Ch 15 butler 
#trucking multiple regression if you want to do that. 

#Calculate the Standardized Residuals the way the textbook calculates it
textbook_residuals <- rstandard(model)

textbook_residuals
textbook_residuals_sorted<-textbook_residuals[order(+textbook_residuals)]
print(textbook_residuals_sorted)

df3<-data.frame(normal_scores,textbook_residuals_sorted)


ggplot(df3, aes(x=normal_scores, y=textbook_residuals_sorted)) + geom_point() + 
  geom_line(data=df2, aes(x=x, y=y), col="blue") + labs(x="Normal Scores", y="Standardized Residuals", title="Normal Probability Plot")+ 
  theme(plot.title=element_text(hjust=0.5)) 


#You only see a slight difference in the way the textbook calculates standardized residuals
#and the way excel does it. 

plot(model)

#Do the cooks distance, studentized deleted residuals, and leverage/influential observations from ch 15 butler trucking
#code to check for outliers, etc.