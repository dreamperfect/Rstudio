library(readxl)
library(ggpubr)
library(tidyverse)
library(broom)
library(gclus)


CravensData<- read_excel("16.2 CravensData determining when to add or delete variables.xlsx")

CravensData

summary(CravensData)

glimpse(CravensData)


correlation<-cor(CravensData)

correlation


#Correlation plot
colors<-dmat.color(correlation)

order<-order.single(correlation)

# Too many variables to be a good visualization
cpairs(CravensData, order, panel.colors = colors,gap=0.5, main="Sorted and Colored by Correlation")

#Form Model

model<-lm(`Sales`~.,data=CravensData)

summary(model)


ssregression<-sum((model$fitted.values-mean(CravensData$Sales))^2)

ssregression

#To show decimals
formatted_ssregression <- format(ssregression, nsmall = 3)

formatted_ssregression

#sum square residual error
ssresidualerror<-sum(model$residuals^2)

ssresidualerror

#total sum of square

total_ss<-ssresidualerror+ssregression

#Mean squares and F value

mse_regression<-ssregression/(length(model$coefficients)-1)

mse_residual<-ssresidualerror/model$df.residual

f_value<-(mse_regression/mse_residual)


#p value

p_value<-1-pf(f_value, length(model$coefficients)-1, length(model$fitted.values-length(model$coefficients)-1)-1)

#join it into anova table

anova_table<-data.frame(Source=c("Regression","Residual Error","Total"), 
                        df= c(length(model$coefficients)-1,model$df.residual,length(model$coefficients)-1+model$df.residual),
                              `Sum Sq`=c(ssregression,ssresidualerror,total_ss),
                              `Mean Sq`=c(mse_regression,mse_residual,NA),
                              `F value`=c(f_value,NA,NA),
                              `Pr(>F)`=c(p_value,NA,NA)
                              )


anova_table

######Backwards Elimination: 

#default is back if left blank, and scope is not included.

backwardselimination<-step(model)



backmodel<-lm(formula = `Sales`~ `Time`+`Poten`+`AdvExp`+`Share`+`Change`, data=CravensData)

summary(backmodel)

#This model is the same as textbooks backwards selection but includes Change because it's p value is at 0.05 and maybe the cutoff is at 0.1
#The model actually performs better with the independent variable Change, as seen in the excel. 


ssregression<-sum((backmodel$fitted.values-mean(CravensData$Sales))^2)

ssregression


#To show decimals
formatted_ssregression <- format(ssregression, nsmall = 3)

formatted_ssregression

#sum square residual error
ssresidualerror<-sum(backmodel$residuals^2)

ssresidualerror

#total sum of square

total_ss<-ssresidualerror+ssregression

#Mean squares and F value

mse_regression<-ssregression/(length(backmodel$coefficients)-1)

mse_residual<-ssresidualerror/backmodel$df.residual

f_value<-(mse_regression/mse_residual)


#p value



p_value<-1-pf(f_value, length(backmodel$coefficients)-1, length(backmodel$fitted.values)-(length(backmodel$coefficients)-1)-1)

#join it into anova table

anova_table_backwardselim<-data.frame(Source=c("Regression","Residual Error","Total"), 
                        df= c(length(backmodel$coefficients)-1,backmodel$df.residual,length(backmodel$coefficients)-1+backmodel$df.residual),
                        `Sum Sq`=c(ssregression,ssresidualerror,total_ss),
                        `Mean Sq`=c(mse_regression,mse_residual,NA),
                        `F value`=c(f_value,NA,NA),
                        `Pr(>F)`=c(p_value,NA,NA)
)

anova_table_backwardselim

#with decimals

anova_table_backwarselim_decimals<-format(anova_table_backwardselim, nsmall = 2)

anova_table_backwarselim_decimals


####Stepwise regression ####
stepwise<-step(model, direction=c("both"))

#Got the same as backward selection, like the textbook, but not the same as the textbook. 
#(Textbook also got the same as backwards but both are different as this one)
#Wont repeat anova table as I know it is the same in Excel if I run it there.


#######forward selection 

#Start with the intercept 

Fitstart<-lm(Sales~1, data=CravensData)

Fitstart

#should be the mean of Sales

summary(CravensData)

#Scope is mandatory for it to work
step(Fitstart, direction="forward", scope=formula(model))

forward_elim_model<-lm(formula = Sales ~ Accounts + AdvExp + Poten + Share + Change + 
                       Time, data = CravensData)

summary(forward_elim_model)

# Can put code for anova here again or instead I did it in Excel. 

