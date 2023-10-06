#BUS2 194A, Chapter 15.9: Logistic Regression - RStudio Lab Session

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
Sim_df <- read_excel("Simmons_Logistic_Regression(Categorical Independent & Dependent Variables).xlsx") 

head(Sim_df) #Gives us a snapshot of the excel file 

#Step 2: Descriptive statistics
summary(Sim_df) #This gives the descriptive statistics of the data

sapply(Sim_df, sd) #This gives us the standard deviation using the "sd" function


xtabs(~Coupon + Card, data = Sim_df) #Creates a two-way contingency table of categorical outcome and predictors


#Step 3: Building the model
#The "glm" function enables us to call the logistic regression model. 
#The family is binomial as we are dealing with binary outcome variables

Sim_logit <- glm(Coupon ~ Spending + Card, data = Sim_df, family = "binomial")
summary(Sim_logit)

#Probabilities of using coupon
Sim_logit$fitted.values

#Step 4: Interpreting the Logistic Regression Equation using odds ratios only

##This gives us the odds ratio for each predictor variable
exp(coef(Sim_logit)) 



#Odds ratios and 95% CI
exp(cbind(O.R. = coef(Sim_logit), confint(Sim_logit))) 


#Beginning of calculating pseudo R2:
Sim_logit$null.deviance

#Here's a simplified explanation of why "-2" is used: The likelihood ratio test statistic is based on the difference 
#in log-likelihoods between the two models. The difference in log-likelihoods is multiplied by "-2" to approximate a 
#chi-squared distribution. For LL(Bad fit model):

ll.null<-Sim_logit$null.deviance/-2

ll.null #


#Sum of log likelihood/ll(good fit):
ll.proposed<-Sim_logit$deviance/-2


ll.proposed

#Pseudo R Squared:

(ll.null-ll.proposed)/ll.null

1-pchisq(2*(ll.proposed-ll.null), df=(length(Sim_logit$coefficients)-1))

#0.00109 is rounded as 0.0011 in the "model.lrm" method I do later on in this code.
#Its p value is less than 0.05 so that is good

#Another way of running model that outputs the pseudo Rsquared automatically
#install.packages("rms")

library(rms)

model.lrm<- lrm(Coupon ~ Spending + Card, data=Sim_df,y=TRUE, x=TRUE)


model.lrm

# Pr(> chi2) = 0.0011 our model is statistically signficant for being better than no model (random guess)


#Create a data frame of probabilities and actual observed values to make a plot

##First convert categorical variables to factors since rstudio thinks its a number (so color in plot will be 0 or 1, not a range)

Sim_df$Card<-as.factor(Sim_df$Card)

Sim_df$Coupon<-as.factor(Sim_df$Coupon)


predicted.data<-data.frame(probability.of.coupon=Sim_logit$fitted.values,observedcoupon=Sim_df$Coupon)

#Name of dataframe/variable is "predicted.data"

predicted.data

#Sort data from low to high probability

predicted.data<-predicted.data[+order(predicted.data$probability.of.coupon,decreasing=FALSE),]

#Add a new column with the rank of each probability from low to high

predicted.data$rank<-1:nrow(predicted.data)

predicted.data

library(ggplot2)
library(cowplot) #nice looking defaults for ggplot


#plot

ggplot(data=predicted.data, aes(x=rank, y=probability.of.coupon,))+
  geom_point(aes(color=observedcoupon), alpha=1, shape=4, stroke=2)+
  xlab("Probability Ranked Low to High")+
  ylab("Predicted Probability")



#Save as pdf

#ggsave("ObservedvsPredicted.pdf")


#Calculate leverage

hats <- as.data.frame(hatvalues(Sim_logit))

#display leverage stats for each observation
hats

#Confidence matrix 

observed_coupons<-Sim_df$Coupon

observed_coupons

predicted.data
probabilities<-Sim_logit$fitted.values

probabilities

# Convert predicted probabilities to class labels (0 or 1) based on a threshold (e.g., 0.5)
classified_predictions <- ifelse(probabilities >= 0.5, 1, 0)


conf_matrix <- table(Reference = observed_coupons, Predicted = classified_predictions)

conf_matrix

# Print the confusion matrix
print(conf_matrix)

#Also do studentized deleted residuals I think, and other checks of model assumptions?
