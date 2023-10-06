# Practice Exercise: Chapter 10 - Hypothesis Test for Two Population Means: Sigma1 and Sigma2 Unknown

# Step 0 - we state the null and alternative hypothesis tests
# H0: u1 - u2 is equal to 0
# Ha: u1 - u2 is not equal to 0


# Step 1 - we import the dataset, go to environment, import data set, from text 
#from text(readr) when its a csv file


library(readxl)
bank_df <- read_excel("CheckAcct.xlsx")
View(CheckAcct)


bank_df #Outputs the dataframe here helps us to view the imported data

# Step 2 - data cleaning. 
#After viewing our data in step 1, we see there's some missing points
#We've to clean and address those missing points in Center A
beechmont_df <- subset(bank_df, select = "Beechmont") #This enables us to focus only on Beechmont feature
beechmont_df #Outputs only Beechmont

beechmont_df <- na.omit(beechmont_df) #na.omit helps us to delete those rows that contain NA as a value
beechmont_df #Outputs only the rows with data in Beechmont branch

# Setp 3 - we need to create a vector that stores the individual features 
beechmont <- beechmont_df$Beechmont # The dollar sign helps us to call a particular feature in the data
beechmont #Outputs a vector to enable us do some calculations. Vector is just an output of the data structure



cherry <- bank_df$`Cherry Grove`
cherry #Outputs only the cherry branch location data

# Step 4 - Compute the test statistics
#Note - the "t.test" function helps us to run the t statistics on the given data
test <- t.test(cherry, beechmont, paired = FALSE, alternative = "two.sided", conf.level = 0.95) # Computes the t statistics
list(test) # Outputs a summary result of the t statistics






# The summary result shows us the test statistics t and p-value.
# t = 2.9546 and p-value = 0.004847
# Interpretation: We reject H0 as p-value is less than alpha = 0.05

# Note
# When performing a two-sided hypothesis test, set the alternative to "two.sided"
# When performing a lower tail hypothesis test, set the alternative to "less"
# When performing an upper tail hypothesis test, set the alternative to "greater"

