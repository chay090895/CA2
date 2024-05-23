# Install necessary packages if not already installed
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
if (!requireNamespace("lmtest", quietly = TRUE)) install.packages("lmtest")
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")

# Load necessary libraries
library(readxl)
library(dplyr)
library(car)
library(lmtest)
library(MASS)

# Load the dataset
file_path <- "/Users/kalki/Downloads/Dataset_2024.xlsx" 
dataset <- read_excel(file_path)

# Display the first few rows and structure
head(dataset)
str(dataset)
summary(dataset)

# Fit the initial multiple regression model
initial_model <- lm(`Body fat (%)` ~ `Age (years)` + `Chest circumference (cm)` + `Density (g/cm³)` + 
                      `Knee circumference (cm)` + `Weight (lbs)`, data=dataset)

# Summary of the initial model
summary(initial_model)

# Diagnostic plots
par(mfrow=c(2,2))
plot(initial_model)

# Check for multicollinearity
vif_values <- vif(initial_model)
print(vif_values)

# Normality of residuals
shapiro_test <- shapiro.test(residuals(initial_model))
print(shapiro_test)

# Homoscedasticity
bptest_result <- bptest(initial_model)
print(bptest_result)

# Autocorrelation
dwtest_result <- dwtest(initial_model)
print(dwtest_result)


# Stepwise regression for model selection
final_model <- stepAIC(initial_model, direction="both")

# Summary of the final model
summary(final_model)

# Diagnostic plots for the final model
par(mfrow=c(2,2))
plot(final_model)

# VIF for the final model
vif_final <- vif(final_model)
print(vif_final)

# Normality of residuals for the final model
shapiro_test_final <- shapiro.test(residuals(final_model))
print(shapiro_test_final)

# Homoscedasticity for the final model
bptest_result_final <- bptest(final_model)
print(bptest_result_final)

# Autocorrelation for the final model
dwtest_result_final <- dwtest(final_model)
print(dwtest_result_final)
