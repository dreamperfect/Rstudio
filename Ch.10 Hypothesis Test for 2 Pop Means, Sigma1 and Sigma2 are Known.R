# Practice Exercise: Chapter 10 part 1 - Hypothesis Test for Two Population Means: Sigma1 and Sigma2 are Known

# Step 1 - we import the dataset, go to environment, import data set, from text (Readr) 
#when its a csv file

library(readr)
exam_df <- read_csv("ExamScores.csv")
View(exam_df)


#Make sure the data is saved as a csv on your desktop or else it won't read
exam_df #Outputs the dataframe here helps us to view the imported data




# Step 2 - data cleaning. 
#After viewing our data in step 1, we see there's some missing points
# we've to clean and address those missing points in Center A
centerA_df <- subset(exam_df, select = "Center A") #This enables us to focus only on the Center A feature
centerA_df #Outputs only Center A

centerA_df <- na.omit(centerA_df) #na.omit helps us to delete those rows that contain NA as a value
centerA_df #Outputs only the rows with data in Center A

samp_size_A <- nrow(centerA_df) #We calculate the total sample size of Center A. it counts number of rows
samp_size_A #Outputs the sample size (n) of Center A

A_scores <- centerA_df$"Center A" #Creates a vector
A_scores #Outputs a vector to enable us do some calculations

#We can also perform the same data cleaning for Center B
# we've to clean and address those missing points in Center A
centerB_df <- subset(exam_df, select = "Center B") 

#This enables us to focus only on the Center B feature
centerB_df #Outputs only Center B

centerB_df <- na.omit(centerB_df) #na.omit helps us to delete those rows that contain NA as a value
centerB_df #Outputs only the rows with data in Center B

samp_size_B <- nrow(centerB_df) #We calculate the total sample size of Center B
samp_size_B #Outputs the sample size (n) of Center B

B_scores <- centerB_df$"Center B" #Creates a vector
B_scores #Outputs a vector to enable us do some calculations

# Step 3 - Descriptive Statistics.
# You can analyze the mean, median, mode, quartiles, histogram, and other plots for Center A
samp_meanA <- mean(A_scores) #Calculate the mean of Center A
samp_meanA #Outputs the mean of Center A

samp_medA <- median(A_scores) #Calculate the median of Center A
samp_medA #Outputs the median of Center A

hist_A <- hist(A_scores) #Plots a histogram of the data and we can see if it follows a normal dist.


summary(A_scores) #This will give you the entire summary statistics of the data
summary(exam_df) #We can look at the summary for the entire dataframe

# Descriptives for Center B
samp_meanB <- mean(B_scores) #Calculate the mean of Center B
samp_meanB #Outputs the mean of Center B

samp_medB <- median(B_scores) #Calculate the median of Center B
samp_medB #Outputs the median of Center B

hist_B <- hist(B_scores) #Plots a histogram of the data and we can see if it follows a normal dist.

# Step 4 - Compute the Standard Error of the two population.
std_error <- sqrt(10^2/samp_size_A+10^2/samp_size_B) #Here we use the standard error equations. Check your notes
std_error

# Step 5 - Compute the test statistics.
test_z <- (samp_meanA-samp_meanB)/std_error 
#We use z test because pop. std dev is known. Check your notes
test_z

# Step 6 - Find the p-value
p_value1 <- 2*pnorm(abs(test_z), lower.tail = FALSE)
#This helps to find the two-sided p-value. Abs means absolute
p_value1 #Outputs the two-side p-value

p_value2 <- pnorm(test_z, lower.tail = TRUE) #This helps to find the p-value of the left-tail (i.e., lower tail)
p_value2 #Outputs the one-sided left tail p-value

p_value3 <- pnorm(test_z, lower.tail = FALSE) 
#This helps to find the p-value of the right-tail (i.e., upper tail)
p_value3 #Outputs the one-sided right tail p-value

# Step 7 - view the results based on the interested p-value
print(c(test_z, p_value1)) #outputs the test statistics and p-value
