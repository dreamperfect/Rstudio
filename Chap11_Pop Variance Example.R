# Chapter 11 - Population Variance
# Develop the null and alternate hypothesis
# H0: variance <= 4
# Ha: variance > 4

# Import data
library(readxl)
BusTimes <- read_excel("BusTimes.xlsx")
View(BusTimes)


bus_df

# Count the number of samples
samp_size <- nrow(bus_df)
samp_size

# Create the vector 
times <- bus_df$Times
times

# Calculate the sample variance using the function "var"
var_time <- var(times) 
var_time

# Compute the chi-squared statistics. Remember the equation from the lecture slides
test_chi <- (samp_size-1)*var_time/4 #Note: 4 is the arrival time variance given in the question
test_chi

# Compute the p-value using the function "pchisq"
p_value <- pchisq(test_chi, samp_size-1, lower.tail = FALSE)
list(test_chi, p_value)

# Interpretation: Do not reject H0 as p-value (0.21) > alpha (0.05)
# The sample does not support the conclusion that the population variance of the arrival times is excessive

# To compute the lower tail we change it to TRUE
# To compute the two tail hypothesis, we multiply line 29 by 2, leaving lower.tail as FALSE