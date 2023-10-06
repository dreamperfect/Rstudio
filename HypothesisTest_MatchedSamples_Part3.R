# Practice Exercise: Chapter 10 - Hypothesis Test for Two Population Means with Matched Samples

# Step 0 - we state the null and alternate hypothesis tests
# H0: ud is equal to 0
# Ha: ud is not equal to 0

#I dont have access to this video anymore as I did not record it. 

# Step 1 -  we import the dataset, go to environment, import data set, 
#from text (Readr) when its a csv file

matched_df<- read_csv("D:/194A Statistical Analysis/(5) R files/Matched.csv")
View(matched_df)

 #Save the data into a dataframe called matched_df



matched_df #Outputs the dataframe here helps us to view the imported data

# Step 2 - we need to create a vector that stores the individual features 

times_1 <- matched_df$`Method 1` 
# The dollar sign helps us to call a particular feature in the data
times_1 

#Outputs a vector to enable us do some calculations. Vector is just an output of the data structure
times_2 <- matched_df$'Method 2'
times_2 #Outputs only the method 2 data

# Step 3 - Compute the test statistics
#Note - the "t.test" function helps us to run the t statistics on the given data
#Note2 - the "paired" is set to TRUE because we are performing a matched samples test 
#for a similar type of work




test <- t.test(times_1, times_2, paired = TRUE, alternative = "two.sided", conf.level = 0.95)
# Computes the t statistics
list(test) # Outputs a summary result of the t statistics

# The summary result shows us the test statistics t and p-value.
# t = 2.1958 and p-value = 0.0795
# Interpretation: Do not reject H0 as the p-value is greater than alpha = 0.05

# Note
# When performing a two-sided hypothesis test, set the alternative to "two.sided"
# When performing a lower tail hypothesis test, set the alternative to "less"
# When performing an upper tail hypothesis test, set the alternative to "greater"

# Bonus explanations that will help you in answering Question 2 of Assignment 1
# To compute the difference, we subtract times_1 from times_2
d = times_1 - times_2 #Computes the difference
d #Outputs the difference

# Proceed to find the mean of d
d_mean = mean(d) #use the "mean" function
d_mean #Outputs the mean of the difference between times_1 and times_2

# Proceed to find the sample standard deviation of d
d_std = sd(d) #Computes the sample standard deviation
d_std #Outputs the standard deviation of the difference


