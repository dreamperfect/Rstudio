library(readxl)
johnson_filtration<- read_excel("Johnson Filtration Multiple Regression with Categorical Independent Variables.xlsx")

johnson_filtration

#Load the packages into R
library(ggpubr) #helps us to create results that are publication ready for your report
library(tidyverse) #for data manipulation and visualization
library(broom) #tidy model output
library("readxl") #Will help us to read .xlsx files
library(gclus) #Allows us to have our scatterplot with correlation embedded in it
theme_set(theme_pubr())
library("dplyr") #For filter() function

library(ggplot2)

glimpse(johnson_filtration)

#Convert type of repair to dummy variable, 1 if electrical, 0 if mechanical

johnson_filtration$`Type of Repair`<- ifelse(johnson_filtration$`Type of Repair` == 'electrical', 1, 0) 

johnson_filtration


#If I want to remove Service Call column use this code:
johnson_filtration<-subset(johnson_filtration,select=-c(`Service Call`))

johnson_filtration

johnson_filtration
#Step 4: Scatterplot, Correlation coefficient & visualization
pairs(~`Repair Time in Hours` + `Months Since Last Service` + `Type of Repair`, data = johnson_filtration) #Simple correlation plot

corr <- abs(cor(johnson_filtration)) # Correlation in absolute value
corr #Outputs the correlation results

colors <- dmat.color(corr) #Gives different color coding to the correlation result
order <- order.single(corr) #orders the result
#Visualize the new correlation plot

cpairs(johnson_filtration, order, panel.colors = colors, gap = 0.5, main = "Sorted and colored variables by correlation") 


#Step 5: Building the model
#Regression equation: Salary = b0 + b1*Experience + b2*Testscore
model <- lm(`Repair Time in Hours` ~ `Months Since Last Service` + `Type of Repair`, data = johnson_filtration)
model

#Step 6: Model Summary
summary(model)

confint(model) #outputs the 97.5% CI of the coefficients

#Making result tidy using the "tidy()" function from the broom package
tidy(model) #gives us a nice and tidy output

#Sum Square Regression 

ssregression <- sum((model$fitted.values-mean(johnson_filtration$`Repair Time in Hours`))^2)  # Explained Sum of Squares
ssregression

#Sum Square Residual Error
ssresidualerror <- sum(model$residuals^2) 
ssresidualerror

#Total Summ of Square (or just ssregression+ssresidual)
total_ss <- sum((johnson_filtration$`Repair Time in Hours` - mean(johnson_filtration$`Repair Time in Hours`))^2)  
total_ss


# Calculate Mean Squares and F value. 
#Assuming you have an intercept, the number of predictors (p) is length(model$coefficients)-1
mse_regression <- ssregression / (length(model$coefficients)-1)
mse_residual <- ssresidualerror / model$df.residual
f_value <- (mse_regression / mse_residual)

# Calculate p-value pf the f distribution with 1-p value(f value, degrees of freedom numerator (number of predictors), degrees of freedom denominator (n-number of predictors-1))
#This equation will work for the f distribution p value of any model, so long as you have an intercept (because I calculate number of predictors with coeffiecients -1)
#Well that is assuming its not diffrent for more complex categorical variables for like 3 categories where a category is x1 and x2 being 0. 
p_value <- 1 - pf(f_value, length(model$coefficients)-1, length(model$fitted.values)-(length(model$coefficients)-1)-1)

p_value


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


#Predictions
model$fitted.values

#Make one table for this:

Res_table <- data.frame(johnson_filtration$`Months Since Last Service`, johnson_filtration$`Type of Repair`,  johnson_filtration$`Repair Time in Hours`, round(model$fitted.values,3),round(model$residuals,3))
Res_table

Res_table<-rename(Res_table, `Months_Since_Last_Service`=johnson_filtration..Months.Since.Last.Service.,`Type_of_Repair`=johnson_filtration..Type.of.Repair.,`Repair_Time(hours)`=johnson_filtration..Repair.Time.in.Hours.,`Predicted_Repair_Time`=round.model.fitted.values..3., 'Residuals'=round.model.residuals..3.)

Res_table


#Line and Scatter Plot with prediction lines 

#Seperate data by category
library("dplyr") #For filter() function


#Just an example of how to filter 

electrical=filter(johnson_filtration, `Type of Repair`==1)

electrical

# An example of how to get predictions manually of just electrical. 
#But you can also just make a table of the predictions and sort or filter by repair type.


electricalprediction=model$coefficients[1]+model$coefficients[2]*electrical$`Months Since Last Service`+
  model$coefficients[3]*electrical$`Type of Repair`

electricalprediction

electricalprediction=data.frame(electrical$`Months Since Last Service`,electricalprediction,electrical$`Type of Repair`)

electricalprediction=rename(electricalprediction,`x`=electrical..Months.Since.Last.Service.,`y`=electricalprediction,Electrical=electrical..Type.of.Repair.)

electricalprediction



johnson_filtration

johnsonwithfactor<-johnson_filtration

#Convert 0 and 1 back to mechanical or electrical for the legend on the graph

johnsonwithfactor$`Type of Repair`<- ifelse(johnson_filtration$`Type of Repair` == 1, 'electrical', 'mechanical') 


johnsonwithfactor$`Type of Repair`<-factor(johnsonwithfactor$`Type of Repair`)

johnsonwithfactor$`Predictions`<-model$fitted.values

johnsonwithfactor
#Shorter version



ggplot(data=johnsonwithfactor,aes(x=`Months Since Last Service`,y=`Repair Time in Hours`,colour=`Type of Repair`))+
  geom_point()+
  geom_line(data=johnsonwithfactor, aes(x=`Months Since Last Service`,y=`Predictions`,colour=`Type of Repair`))+
  scale_x_continuous(limits=c(0,10),breaks=seq(0, 10, 1))+
  scale_y_continuous(limits=c(0,7),breaks=seq(0,5,1))

johnsonwithfactor


#Like in textbook (Making the line reach zero)

zerototen<-seq(0,10,1)
zerototen

zerototen<-data.frame(zerototen)

print(zerototen)

zerototen$`Mechanical Predictions`<-model$coefficients[1]+model$coefficients[2]*zerototen$zerototen
zerototen$`Electrical Predictions`<-model$coefficients[1]+model$coefficients[2]*zerototen$zerototen+
  model$coefficients[3]


johnsonwithfactor

ggplot(data=johnsonwithfactor,aes(x=`Months Since Last Service`,y=`Repair Time in Hours`))+
  geom_point()+
  geom_line(data=zerototen,aes(x=`zerototen`,y=`Mechanical Predictions`,colour="Mechanical Predictions"))+
  geom_line(data=zerototen,aes(x=`zerototen`,y=`Electrical Predictions`,colour="Electrical Predictions"))+
  scale_x_continuous(breaks=seq(0, 10, 2))+
  scale_y_continuous(limits=c(0,7),breaks=seq(0,7,1))+
  guides(colour=guide_legend(title="Prediction Type"))

zerototen

#So the graphs are the same as in excel it just looks different because the x label intervals are different, 
#and this second one includes more points in the line but is the same slope.

#More plots:
layout(matrix(c(1,2,3,4),1,1))
plot(model)


#Do the cooks distance, studentized deleted residuals, and leverage/influential observations from ch 15 butler trucking
#code to check for outliers, etc.

#You can manually make the normal probability plot from the textbook (standardized residuals vs normal scores)
#by using the code from Armands pizza, but you can also just see from the Normal Q-Q that no standardized residual
#is less than -2 or more than +2 so no outlier is detected that way.
