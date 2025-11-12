# ------------------------------------------------------------
# Project: Regression Modelling with the ISLR2 College Dataset in R
# Author: Chok Zu Bing
# Repository: https://github.com/ChokZB/college-regression-analysis
# Description: Multiple linear regression, model diagnostics, transformations, subset selection, and cross-validation.
# ------------------------------------------------------------


# Load the necessary library
library(ISLR2)
library(leaps)
library(boot)

# Load the dataset
data("College")

# Check for missing values in the `Outstate` column
sum(is.na(College$Outstate)) 

# Fit the original multiple linear regression model
model = lm(Outstate ~ Apps + Accept + Enroll + Top10perc + Top25perc, data = College)

# Summary of the original model
summary(model)


# ------------------------------------------------------------
# Diagnostic plots for the original model
# ------------------------------------------------------------

# Create a folder for figures if it doesn’t exist
if (!dir.exists("figures")) dir.create("figures")

png("figures/model_diagnostics_original.png", width = 1200, height = 800)
par(mfrow = c(2, 2))
plot(model)
dev.off()

# Studentised residual plot for the original model
png("figures/studentized_residuals_original.png", width = 800, height = 600)
par(mfrow = c(1, 1))
plot(predict(model), rstudent(model), main = "Studentised Residuals - Original Model",
     xlab = "Predicted Values", ylab = "Studentised Residuals")
dev.off()


# ------------------------------------------------------------
# Log transformation of the response variable `Outstate`
# ------------------------------------------------------------
model_log = lm(log(Outstate) ~ Apps + Accept + Enroll + Top10perc + Top25perc, data = College)

# Summary of the log-transformed model
summary(model_log)

# Diagnostic plots for the log-transformed model
png("figures/model_diagnostics_log.png", width = 1200, height = 800)
par(mfrow = c(2, 2))
plot(model_log)
dev.off()

# Studentised residual plot for the log-transformed model
png("figures/studentized_residuals_log.png", width = 800, height = 600)
par(mfrow = c(1, 1))
plot(predict(model_log), rstudent(model_log), main = "Studentised Residuals - Log Model",
     xlab = "Predicted Values", ylab = "Studentised Residuals")
dev.off()


# ------------------------------------------------------------
# Square root transformation of the predictors `Apps`, `Accept`, and `Enroll`
# ------------------------------------------------------------
model_sqrt = lm(Outstate ~ sqrt(Apps) + sqrt(Accept) + sqrt(Enroll) + Top10perc + Top25perc, data = College)

# Summary of the square root-transformed model
summary(model_sqrt)

# Diagnostic plots for the square root-transformed model
png("figures/model_diagnostics_sqrt.png", width = 1200, height = 800)
par(mfrow = c(2, 2))
plot(model_sqrt)
dev.off()

# Studentised residual plot for the square root-transformed model
png("figures/studentized_residuals_sqrt.png", width = 800, height = 600)
par(mfrow = c(1, 1))
plot(predict(model_sqrt), rstudent(model_sqrt), main = "Studentised Residuals - Sqrt Model",
     xlab = "Predicted Values", ylab = "Studentised Residuals")
dev.off()


# ------------------------------------------------------------
# Best subset selection
# ------------------------------------------------------------
regfit.full = regsubsets(Outstate ~ ., data = College, nvmax = 17)

# Summary of the best subset selection
reg.summary = summary(regfit.full)
reg.summary

# Plotting Adjusted R-squared, Cp, and BIC
png("figures/best_subset_selection_metrics.png", width = 1800, height = 600)
par(mfrow = c(1, 3))


# Adjusted R-squared
plot(reg.summary$adjr2, xlab = "Number of Predictors", ylab = "Adjusted R-Squared", main = "Adjusted R-Squared", type = "b")

# Add dotted grid lines for reference
grid()

# Highlight the best model based on Adjusted R-Squared
points(which.max(reg.summary$adjr2), max(reg.summary$adjr2), col = "red", cex = 2, pch = 20)


# Cp
plot(reg.summary$cp, xlab = "Number of Predictors", ylab = "Cp", main = "Cp", type = "b")

# Add dotted grid lines for reference
grid()

# Highlight the best model based on Cp
points(which.min(reg.summary$cp), min(reg.summary$cp), col = "red", cex = 2, pch = 20)


# BIC
plot(reg.summary$bic, xlab = "Number of Predictors", ylab = "BIC", main = "BIC", type = "b")

# Add dotted grid lines for reference
grid()

# Highlight the best model based on BIC
points(which.min(reg.summary$bic), min(reg.summary$bic), col = "red", cex = 2, pch = 20)

dev.off()


# Print the best model based on Adjusted R-squared and display the coefficients
cat("Best model by Adjusted R-squared: ", which.max(reg.summary$adjr2), "\n")
print(coef(regfit.full, which.max(reg.summary$adjr2)))

# Print the best model based on Cp and display the coefficients
cat("Best model by Cp: ", which.min(reg.summary$cp), "\n")
print(coef(regfit.full, which.min(reg.summary$cp)))

# Print the best model based on BIC and display the coefficients
cat("Best model by BIC: ", which.min(reg.summary$bic), "\n")
print(coef(regfit.full, which.min(reg.summary$bic)))


# ------------------------------------------------------------
# Cross-validation MSE plots
# ------------------------------------------------------------ 

# Set the seed for reproducibility
set.seed(1)

# Function to perform cross-validation and calculate MSE for different polynomial degrees
calculate_mse = function(data, response, predictor, method = "holdout", k = 10) {
    # Define the range of polynomial degrees to evaluate
    degrees = 1:10
    
    # Initialise a numeric vector to store MSE values for each polynomial degree
    mse_values = numeric(length(degrees))
    
    # Calculate MSE using Holdout Method
    if (method == "holdout") {
        # Split the data into training (70%) and testing (30%) sets
        train_index = sample(1:nrow(data), 0.7 * nrow(data))
        train_data = data[train_index, ]
        test_data = data[-train_index, ]
        
        # Loop through polynomial degrees from 1 to 10
        for (degree in degrees) {
            # Fit the model using polynomial regression of the specified degree
            model = lm(as.formula(paste(response, "~ poly(", predictor, ",", degree, ")")), data = train_data)
            
            # Predict the response variable on the test data
            predictions = predict(model, newdata = test_data)
            
            # Calculate MSE for the current degree and store it in `mse_values`
            mse_values[degree] = mean((test_data[[response]] - predictions)^2)
        }
    
    # Calculate MSE using Leave-One-Out Cross-Validation (LOOCV) or k-Fold Cross-Validation
    } else if (method == "loocv" || method == "kfold") {
        for (degree in degrees) {
            # Fit the model using polynomial regression of the specified degree
            model = glm(as.formula(paste(response, "~ poly(", predictor, ",", degree, ")")), data = data)
            
            # Calculate MSE using Leave-One-Out Cross-Validation (LOOCV)
            if (method == "loocv") {
                mse_values[degree] = cv.glm(data, model)$delta[1]
                
            # Calculate MSE using k-Fold Cross-Validation
            } else if (method == "kfold") {
                set.seed(1)
                mse_values[degree] = cv.glm(data, model, K = k)$delta[1]
            }
        }
    }
    
    # Return the vector of MSE values for each polynomial degree
    return(mse_values)
}

# Function to create a results data frame
create_results_df = function(mse_values) {
    return(data.frame(
        Degree = 1:10,
        Model = c("linear", paste0("polyn", 2:10)),
        MSE = format(round(mse_values, 4), scientific = FALSE)
    ))
}

# Apply the function for each cross-validation approach
holdout_mse = calculate_mse(College, "Top10perc", "Apps", method = "holdout")
loocv_mse = calculate_mse(College, "Top10perc", "Apps", method = "loocv")
kfold_mse = calculate_mse(College, "Top10perc", "Apps", method = "kfold", k = 10)

# Create separate data frames for each method
holdout_results = create_results_df(holdout_mse)
loocv_results = create_results_df(loocv_mse)
kfold_results = create_results_df(kfold_mse)

# Export MSE plots
png("figures/cv_mse_comparison.png", width = 900, height = 600)
plot(1:10, holdout_mse, type = "b", col = "blue", ylim = range(c(holdout_mse, loocv_mse, kfold_mse)),
     xlab = "Polynomial Degree", ylab = "Mean Squared Error", main = "Cross-Validation Comparison")
lines(1:10, loocv_mse, type = "b", col = "red")
lines(1:10, kfold_mse, type = "b", col = "green")
legend("topright", legend = c("Holdout", "LOOCV", "K-Fold (10)"),
       col = c("blue", "red", "green"), lty = 1, pch = 19)
dev.off()

# Print the results
print("Cross-Validation Approach 1 - Holdout Method Results:")
print(holdout_results, row.names = FALSE)

print("Cross-Validation Approach 2 - Leave-One-Out Cross-Validation Results:")
print(loocv_results, row.names = FALSE)

print("Cross-Validation Approach 3 - k-Fold Cross-Validation Results:")
print(kfold_results, row.names = FALSE)



###########################################################
# Perform different cross-validation approach sequentially
###########################################################

# # Load the necessary library for cross-validation
# library(boot)
# 
# # Cross-Validation Approach 1: Holdout Method
# 
# # Set the seed for reproducibility
# set.seed(1)
# 
# # Function to calculate Mean Squared Error (MSE)
# mse = function(actual, predicted) {
#     mean((actual - predicted)^2)
# }
# 
# # Split the data into training and testing sets
# train_index = sample(1:nrow(College), 0.7 * nrow(College))
# train_data = College[train_index, ]
# test_data = College[-train_index, ]
# 
# # Initialise vector to store MSE values
# holdout_mse = numeric(10)
# 
# # Loop through polynomial degrees 1 to 10
# for (degree in 1:10) {
#     # Fit a polynomial regression model
#     model = lm(Top10perc ~ poly(Apps, degree), data = train_data)
# 
#     # Make predictions on the test data
#     predictions = predict(model, newdata = test_data)
# 
#     # Calculate and store the MSE
#     holdout_mse[degree] = mse(test_data$Top10perc, predictions)
# }
# 
# # Create a data frame to display the results
# mse_results = data.frame(Degree = 1:10,
#                          Model = c("linear", paste0("polyn", 2:10)),
#                          Holdout_MSE = format(round(holdout_mse, 4), scientific = FALSE))
# 
# # Print the results
# print(mse_results, row.names = FALSE)
# 
# 
# # Cross-Validation Approach 2: Leave-One-Out Cross-Validation (LOOCV)
# 
# # Set a seed for reproducibility
# set.seed(1)
# 
# # Initialise a numeric vector to store LOOCV MSE values for each degree
# loocv_mse = numeric(10)
# 
# # Loop over polynomial degrees from 1 to 10
# for (degree in 1:10) {
#     # Fit a generalised linear model with polynomial terms
#     model = glm(Top10perc ~ poly(Apps, degree), data = College)
# 
#     # Calculate LOOCV MSE for the current model
#     loocv_mse[degree] = cv.glm(College, model)$delta[1]
# }
# 
# # Create a data frame to display the results
# mse_results = data.frame(Degree = 1:10,
#                          Model = c("linear", paste0("polyn", 2:10)),
#                          LOOCV_MSE = format(round(loocv_mse, 4), scientific = FALSE))
# 
# # Print the results
# print(mse_results, row.names = FALSE)
# 
# 
# 
# # Cross-Validation Approach 3: k-Fold Cross-Validation (k=10)
# 
# # Set a seed for reproducibility
# set.seed(1)
# 
# # Initialise a numeric vector to store LOOCV MSE values for each degree
# k_fold_mse = numeric(10)
# 
# # Loop over polynomial degrees from 1 to 10
# for (degree in 1:10) {
#     # Fit a generalised linear model with polynomial terms
#     model = glm(Top10perc ~ poly(Apps, degree), data = College)
# 
#     # Calculate k-Fold MSE for the current model
#     k_fold_mse[degree] = cv.glm(College, model, K = 10)$delta[1]
# 
# }
# 
# # Create a data frame to display the results
# mse_results = data.frame(Degree = 1:10,
#                          Model = c("linear", paste0("polyn", 2:10)),
#                          K_Fold_MSE = format(round(k_fold_mse, 4), scientific = FALSE))
# 
# # Print the results
# print(mse_results, row.names = FALSE)