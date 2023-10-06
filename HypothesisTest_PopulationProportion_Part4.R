# Difference between two population proportions
# H0: p1 - p2 is equal to 0
# Ha: p1 - p2 is not equal to 0

# Import data. I did it with import text base and string as factor, checkmark headers


tax_df<-TaxPrep

tax_df
# Clean the data for Office 1
office1_df<- subset(tax_df, select = Office.1)
office1_df

office1_df <- na.omit(office1_df)
office1_df

# Look at the number of Yes in the data
errors1 <- subset(office1_df, Office.1=="Yes")
errors1

num_errors1 <- nrow(errors1)
num_errors1

# Look at the number of No in the data
not_errors1 <- subset(office1_df, Office.1=="No")
not_errors1

num_not_errors1 <- nrow(not_errors1)
num_not_errors1

# Count the samples of Yes and No
samp_size1 <- num_errors1 + num_not_errors1
samp_size1

# We look at analyzing office 2 the same way we analyze office 1
# Clean the data for Office 2
office2_df<- subset(tax_df, select = Office.2)
office2_df

office2_df <- na.omit(office2_df)
office2_df

# Look at the number of Yes in the data
errors2 <- subset(office2_df, Office.2=="Yes")
errors2

num_errors2 <- nrow(errors2)
num_errors2

# Look at the number of No in the data
not_errors2 <- subset(office2_df, Office.2=="No")
not_errors2

num_not_errors2 <- nrow(not_errors2)
num_not_errors2

# Count the samples of Yes and No
samp_size2 <- num_errors2 + num_not_errors2
samp_size2

# Perform the hypothesis test using the "prop.test" function, which is for population proportion
test <- prop.test(c(num_errors1, num_errors2), c(samp_size1, samp_size2), alternative = "two.sided", 
                  conf.level = 0.95, correct = FALSE)
list(test)
# We do not reject H0 as the p-value is greater that alpha (0.05)
