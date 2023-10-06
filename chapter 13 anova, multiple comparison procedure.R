

install.packages(c("ggplot2","ggpubr","tidyverse","broom"))

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)

#ho:u1=u2=u3
#ha:not all pop. means are equal 

library(readr)
Chemitech_Stacked <- read_csv("Chemitech_Stacked.csv", 
                              col_types = cols(Method = col_factor(levels = c("Method A", 
                                                                              "Method B", "Method C"))))
View(Chemitech_Stacked)

#assumptions of anova that we check: for each population, the response variable 
#is normally distributed. The variance of the response variable is the same for all pops.
#The observation must be independent. 

summary(Chemitech_Stacked)
summary(Chemitech_Stacked$Method)
#The categorical variable (Method A,B,C) has five observations.
#You can view this in summary only if you import as a csv and check character as factor
#Quantitative variable "NumberAssembled" has a numeric summary with mean=60 and median =59.
#The number of filtration systems assembled per week on average by employeees is 60.

boxplot(NumberAssembled ~ Method, data=Chemitech_Stacked, main="Filtration System Analysis",
          xlab="Method",ylab="Number of Filtration System")

#The boxplot gives us a visual summary of the data. It shows the median, outliers, quartiles, 
#maximum and minimum values. It is observed from the plot that the median values of all three assembly
#methods are different (64,68,48). We can visually see the variation in the data from the boxplot, which
#shows high within group variance and high among group variance. We see that not all notchces in the boxplot
#overlap and we can conclude with 95% confidence that the true medians do differ. 


chem_anova<-aov(NumberAssembled~Method,data=Chemitech_Stacked)
summary(chem_anova)

#The anova test shows us the variance MSE in the data as 28.33. The pvalue is less then 0.01,
#so we reject the null hypothesis.  
#Ho: u1=u2=u3

#Conclusion: We can conclude that there are significant differences between the three filtration assembly
#methods being used by Chemitech employees. 

#Assumption test, homogenity of variance: graphical analysis  to check for variance using Residual vs 
#Fitted plot.The 1 says to do residual vs fitted plot. 

plot(chem_anova,1)

#Interpretation: Points 9, 7, and 2 are detected as outliers, which can severely affect the normality and
#variance assumptions. It can be useful to remove outliers from the data to meet test assumptions. 

#We can use a test called a levenes test to check for variance. 

library(car)
leveneTest(NumberAssembled~Method,data=Chemitech_Stacked) 

#From the output we can see that the p value is not less then significance level of 0.05. This means that 
#there is no evidence to suggest that the variance across methods is statistically significantly different.
#Therefore, we can assume the homogeneity of variances is in the different treatment groups. 

#The second assumption to check for is normality(normally distributed dataset) The normal probability plot of
#residuals(Normal Quantile plot) is used to check for normality. The points on the plot should approximately
#follow a straight line. 

plot(chem_anova,2)

#The points on a plot does not really follow a straight line so we cannot assume normality.
# We may have to perform another test to confirm.

#The shapiro Wilk test on the ANOVA residuals is used to confirm normality. 
#Extract the residuals

chem_residuals<-residuals(object=chem_anova)

#run shapiro wilk test

shapiro.test(x=chem_residuals)

#W=0.92, p=0.18 which indicates that the normality assumption is not violated. 

#Multiple Comparison Procedure using t test
#The pairwise.t.test function helps us to perform the F 

pairwise.t.test(Chemitech_Stacked$NumberAssembled, Chemitech_Stacked$Method, p.adj="bonf")

#"bonf" means bonferroni 
#P values indicate that Method A and B have similar means? 



#Another method for multiple comparison is Tukey Honest Significant Differences (HSD)

TukeyHSD(chem_anova, p.adj="bonf")

#Interpretation A: With an alpha value of 0.05 (p-value=0.03<0.05  we can reject the
#null hypothesis that the pop mean number of units produced per week with method A or Method B is equal to pop 
#mean for method C. 


#Overall interpretation: Our conclusion is that the pop means for Method A differs from pop mean of Method C, Method B
#differs from pop mean for Method C. However Method A and B do not differ.








