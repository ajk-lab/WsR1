# %%
34 + 23 * 10
27 + 1 / 7
2^10
exp(2)
# %%


# %%
sqrt(100)
log(10)
factorial(10)
log(factorial(10))
choose(15, 5)
factorial(15) / (factorial(5) * factorial(15 - 5))
factorial(1500)
factorial(1500) / (factorial(2) * factorial(1500 - 2))
choose(1500, 2)
# %%

# Variables & Functions
# %%
x <- 2
y <- 10
x^y
y^x
factorial(y)
log(factorial(y))
choose(x, y)
choose(y, x)
x <- 10
factorial(x)
x + y
x * y
"+"(x, y)
choose
str(choose)
str(log)
log(100)
log(100, 10)
log(100, 2)
log(x = 100, base = 10)
log(base = 10, x = 100)
# %%


# Vectors
# %%
1:10
5:20
20:5
seq(1, 10)
seq(1, 10, 0.1)

sample(1:10, 5)
sample(1:10, 20)
sample(1:10, 20, replace = TRUE)

rnorm(10)
runif(10)
rbinom(10, size = 15,prob = 0.25)
rbinom(10, size = 15,prob = 0.25)

x <- rnorm(100)
x

mean(x)
sum(x)
length(x)
sum(x) / length(x)
prod(x)
prod(1:10)
factorial(10)

N <- 15
0:N
r <- 0:N
choose(N,r)

1:10 * 2:11

(1:10)^3

(1:10)^(2:3)
# %%

2024-12-28-11-44-17.png

#Binomial Ditribution
#%%
N <- 15
x -< seq(0, N)
p <- 0.25
choose(N, x) * p^x * (1-p)^(N-x)
dbinom(x, size = N, prob = p)

seq(0,15,2)
dbinom(seq(0,15,2), size = N, prob = p)
sum(dbinom(seq(0,15,2), size = N, prob = p))
sum(dbinom(seq(0,7), size = N, prob = p))
pbinom(7, size = N, prob = p)
x <- 0:N
px <- dbinom(x , size = N, prob = p)
plot(x, px)

plot(x, px, pch = 16, ylab = "Probability")
abline(h=0, col="grey")

plot(x, px, pch = 16, ylab = "Probability", abline(h=0, col="grey"))

p <- 0.25
z <- rbinom(100, size = 1, prob = p)
z
cumsum(z)
cumsum(z) / 1:100

plot(1:100, type = "n", ylim = c(0,1), ylab = "Sample proportion")
for (i in 1:50) {
    z <- rbinom(100, size = 1, prob = p)
    lines(1:100, cumsum(z) / 1:100, col = sample(colors(),1))
}
#%%

#%%
help(Distributions)

ppois(14, lambda=10)

pbinom(10, size=1000, prob=0.01)  # exact binomial
ppois(10, lambda=10)              # Poisson approx
pnorm(10, mean=10, sd=3.146)      # Normal approx

N <- 12
m <- 30

p_12_30 <- 1
for (k in 1:11) {
  p_12_30 <- p_12_30 + (-1)^(k+1) * choose(N, k) * ((N - k)/N)^m
}

p_12_30


#%%

N <- 12
m <- 30

p_12_30 <- 1
for (k in 1:11) {
  # note the sign: (-1)^(k+1) 
  p_12_30 <- p_12_30 - (-1)^(k+1) * choose(N, k) * ((N - k)/N)^m
}

p_12_30

#%%
N <- 12
m <- 30

p_12_30 <- 1
for (k in 1:N) {
  # Notice (-1)^k (not ^(k+1)) so that k=1 => sign is negative, k=2 => sign is positive, etc.
  p_12_30 <- p_12_30 + (-1)^k * choose(N, k) * ((N - k)/N)^m
}

p_12_30


#%%

#%%
N <- 12
m <- 30

p_12_30 <- 1
for (k in 1:N) {
  # (-1)^(k+1) ensures the correct alternating signs for union
  p_12_30 <- p_12_30 - (-1)^(k+1) * choose(N, k) * ((N - k)/N)^m
}

p_12_30
#%%

#%%
N <- 12
m <- 50

p_12_50 <- 1
for (k in 1:N) {
  p_12_50 <- p_12_50 + (-1)^k * choose(N, k) * ((N - k)/N)^m
}

p_12_50

#%%

#%%
install.packages("lmreg")

library(lmreg)

data(lifelength)

# Mean life length of musicians
mean_musician <- with(lifelength, mean(DurationOfLife[Category == 9], na.rm = TRUE))

# Mean life length of mathematicians/astronomers
mean_mathastr <- with(lifelength, mean(DurationOfLife[Category == 10], na.rm = TRUE))

difference <- mean_musician - mean_mathastr
difference

#%%


#%%
# Load the required package
library(alr4)

# Load the MinnWater dataset
data("MinnWater")

# View the first few rows to understand the structure of the dataset
head(MinnWater)

# Plot `allUse` against `statePop`
plot(MinnWater$statePop, MinnWater$allUse,
     xlab = "State Population (in millions)",
     ylab = "Annual Ground Water Consumption (in billions of gallons)",
     main = "Ground Water Consumption vs State Population",
     pch = 19, col = "blue")

# Fit a least squares regression line
model <- lm(allUse ~ statePop, data = MinnWater)
abline(model, col = "red", lwd = 2)

# Check the residuals to identify patterns
residuals <- resid(model)
plot(MinnWater$statePop, residuals,
     xlab = "State Population (in millions)",
     ylab = "Residuals",
     main = "Residual Plot",
     pch = 19, col = "purple")
abline(h = 0, col = "red", lwd = 2)

# Check for outliers, heteroscedasticity, or systematic patterns
summary(model)

#%%

#%%
# Load the required packages
library(HistData)

# Load the Wheat dataset
data("Wheat")

# Create the ratio of Wages to Wheat
Wheat$Wages_Wheat <- Wheat$Wages / Wheat$Wheat

# Perform linear regression of Wages/Wheat on Year
model <- lm(Wages_Wheat ~ Year, data = Wheat)

# Calculate leverage
leverage <- hatvalues(model)

# Calculate studentized residuals
studentized_residuals <- rstudent(model)

# Thresholds
high_leverage_threshold <- 2 * (2 / nrow(Wheat)) # 2 predictors (including intercept)
high_residual_threshold <- 3

# Find points with high leverage and high studentized residuals
high_leverage_points <- which(leverage > high_leverage_threshold)
high_residual_points <- which(abs(studentized_residuals) > high_residual_threshold)

# Intersection of points with both high leverage and high residuals
high_leverage_and_residuals <- intersect(high_leverage_points, high_residual_points)

# Output the number of points
length(high_leverage_and_residuals)

#%%

#%%
# Load the required packages
library(alr4)
library(car)

# Load the dwaste dataset
data("dwaste")

# Fit the regression model for log(O2UP) on pollution measures
model_dwaste <- lm(log(O2UP) ~ BOD + TKN + TS + TVS + COD, data = dwaste)

# Calculate VIF for each predictor
vif_values <- vif(model_dwaste)

# Find the number of predictors with VIF > 10
vif_high_count <- sum(vif_values > 10)

# Calculate the condition number
design_matrix <- model.matrix(model_dwaste)
eigen_values <- eigen(t(design_matrix) %*% design_matrix)$values
condition_number <- sqrt(max(eigen_values) / min(eigen_values))

# Output results
list(
  vif_count = vif_high_count,
  condition_number = condition_number
)

#%%


#%%
# Install the package if not already installed
#install.packages("alr4")
library(alr4)

# Load the dataset
data(MinnWater)

# Scatter plot with regression line
plot(MinnWater$statePop, MinnWater$allUse, 
     main = "Scatter Plot of allUse vs statePop",
     xlab = "State Population (statePop)",
     ylab = "Annual Ground Water Consumption (allUse)",
     pch = 16, col = "blue")

# Add least squares regression line
abline(lm(allUse ~ statePop, data = MinnWater), col = "red", lwd = 2)
# Fit the model
model <- lm(allUse ~ statePop, data = MinnWater)

# Plot residuals
plot(MinnWater$statePop, residuals(model), 
     main = "Residual Plot",
     xlab = "State Population (statePop)",
     ylab = "Residuals",
     pch = 16, col = "purple")
abline(h = 0, col = "black", lwd = 1.5)

#%%

#%%

# Install and load necessary packages
if (!require("alr4")) install.packages("alr4")
if (!require("car")) install.packages("car")
library(alr4)
library(car)

# Load the data
data("dwaste")
head(dwaste)
write.csv(dwaste,"dwaste.csv")
# Fit the linear regression model
model <- lm(log(O2UP) ~ BOD + TKN + TS + TVS + COD, data = dwaste)

# Calculate Variance Inflation Factors (VIF)
vif_values <- vif(model)

# Identify the number of predictors with VIF > 10
vif_greater_than_10 <- sum(vif_values > 10)
cat("Number of predictors with VIF > 10:", vif_greater_than_10, "\n")

# Compute the condition number of the model
condition_number <- kappa(model, exact = TRUE)
cat("Condition number of the model:", condition_number, "\n")

#%%


#%%
# Install and load required packages
required_packages <- c("alr4", "leaps", "DAAG")


# Load the dataset
data(wblake, package = "alr4")

# Create the squared term for Length
wblake$Length_Squared <- wblake$Length^2

# Perform subset selection using regsubsets()
subset_selection <- regsubsets(Age ~ Length + Length_Squared + Scale, data = wblake, nvmax = 3)

# Extract subset selection summary
subset_summary <- summary(subset_selection)

# Identify the best subset based on the lowest Mallows' Cp
best_model_index <- which.min(subset_summary$cp)

# Display the selected predictors in the best model
best_subset <- subset_summary$which[best_model_index, ]
print("Best subset based on Cp criterion:")
print(best_subset)

# Construct formula for best subset
selected_predictors <- names(best_subset[best_subset == TRUE])[-1] # Remove intercept
best_model_formula <- as.formula(paste("Age ~", paste(selected_predictors, collapse = " + ")))

# Fit the best model
best_model <- lm(best_model_formula, data = wblake)

# Compute PRESS for the best model
press_value <- sum((resid(best_model) / (1 - hatvalues(best_model)))^2)

# Display the PRESS value
print(paste("PRESS value for the best subset:", press_value))

#%%

#%%
# Install and load required packages
required_packages <- c("alr4", "leaps")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Load the dataset
data(MinnWater, package = "alr4")

# Fit all subsets regression using regsubsets()
subset_selection <- regsubsets(allUse ~ muniUse + agPrecip + muniPrecip + statePop, 
                               data = MinnWater, nvmax = 4)

# Extract the summary of subset selection
subset_summary <- summary(subset_selection)

# Find the best subset based on Adjusted R^2
best_adj_r2_index <- which.max(subset_summary$adjr2)
best_adj_r2_subset <- subset_summary$which[best_adj_r2_index, ]

# Find the best subset based on Mallows' Cp
best_cp_index <- which.min(subset_summary$cp)
best_cp_subset <- subset_summary$which[best_cp_index, ]

# Count the number of selected predictors for each criterion
num_adj_r2 <- sum(best_adj_r2_subset) - 1  # Subtracting 1 to exclude intercept
num_cp <- sum(best_cp_subset) - 1  # Subtracting 1 to exclude intercept

# Display the results
print(paste("Best subset size based on Adjusted R^2:", num_adj_r2))
print(paste("Best subset size based on Mallows' Cp:", num_cp))

#%%

#%%

setwd("/Users/ajk/Library/CloudStorage/OneDrive-Personal/ajkTech/CODE-LAB-REPO/WsR1/ISIpgdas")
data <- read.csv("data/wblake.csv")

# Fit the linear regression model
model <- lm(Age ~ sqrt(Length), data = data)

# New data for prediction
new_data <- data.frame(Length = 200)
new_data$sqrt_Length <- sqrt(new_data$Length)

# Predict age with confidence and prediction intervals
prediction <- predict(model, newdata = new_data, interval = "prediction", level = 0.95)

# Compute the length of the prediction interval
interval_length <- prediction[ , "upr"] - prediction[ , "lwr"]

# Print the result rounded to two decimal places
print(round(interval_length, 2))

#%%

#%%
# Load necessary package
#install.packages("alr4")  # If not installed
library(alr4)

# Load dataset
data(wblake)

# Transform Length variable
wblake$sqrt_Length <- sqrt(wblake$Length)

# Fit linear regression model
model <- lm(Age ~ sqrt_Length, data = wblake)

# New data point for prediction
new_data <- data.frame(sqrt_Length = sqrt(200))

# Predict age and compute 95% prediction interval
pred <- predict(model, newdata = new_data, interval = "prediction", level = 0.95)

# Print results
print(pred)

# Compute and round the length of the prediction interval
interval_length <- round(pred[3] - pred[2], 2)
print(interval_length)

#%%


#%%
# Step 1: Install and Load the Required Package
if (!require(lmreg)) {
  install.packages("lmreg", dependencies = TRUE)
}
library(lmreg)

# Step 2: Load the LAcrime Dataset
data("LAcrime", package = "lmreg")

# Step 3: Data Preparation
# Define the rape rate (rapes per 100,000 population)
LAcrime$raperate <- (LAcrime$Rape / LAcrime$Population) * 100000

# Step 4: Fit the Linear Model (raperate ~ Year + TempCelsius)
model <- lm(raperate ~ Year + TempCelsius, data = LAcrime)

# Step 5: Model Summary to Check Coefficients
summary(model)

# Step 6: Extract Coefficient for TempCelsius
temp_coef <- coef(model)["TempCelsius"]

# Step 7: Compute Expected Change for 9°C Temperature Difference
expected_change <- temp_coef * 9

# Step 8: Print the Final Result (Rounded to Two Decimal Places)
cat("Estimated change in rape rate for a 9°C temperature increase:", round(expected_change, 2), "\n")


#%%

#%%
# Load necessary libraries
#install.packages("lmreg")  # Install if not already installed
library(lmreg)

# Load the dataset
data(LAcrime)

# Create the response variable: HomicideRate per 100,000 population
LAcrime$HomicideRate <- (LAcrime$Homicide / LAcrime$Population) * 100000

# Fit the multiple linear regression model
model <- lm(HomicideRate ~ Year + factor(Month) + Fahrenheit, data = LAcrime)

# Summary of the model
summary(model)

# Checking R-squared and Adjusted R-squared
summary(model)$r.squared
summary(model)$adj.r.squared

# Checking the coefficient difference for March and February
coefficients(model)
diff_march_feb <- coefficients(model)["factor(Month)3"] - coefficients(model)["factor(Month)2"]
diff_march_feb

#%%


#%%
# Install and load necessary package
#install.packages("alr4")  # Install if not already installed
library(alr4)

# Load dataset
data(dwaste)

# Set the seed using last 5 digits of student ID (replace XXXXX)
n <- 22088  
set.seed(n)

# Construct response variable y
y <- log(dwaste$O2UP) + 0.01 * rnorm(length(dwaste$O2UP))

# Display first few rows to check dataset
head(dwaste)

# 2. Plot y against each pollution measure
pollutants <- c("BOD", "TKN", "TS", "TVS", "COD")

par(mfrow=c(2,3))  # Arrange plots in 2 rows and 3 columns
for (var in pollutants) {
  plot(dwaste[[var]], y, main=paste("y vs", var), xlab=var, ylab="y", pch=19, col="blue")
}

# 3. Multiple linear regression: Regress y on all pollutants
model_full <- lm(y ~ BOD + TKN + TS + TVS + COD, data=dwaste)
summary(model_full)  # Check model summary

# Extract significant coefficients (p < 0.05)
significant_vars <- summary(model_full)$coefficients[summary(model_full)$coefficients[,4] < 0.05, ]
print(significant_vars)

# 4. P-value of F-statistic for model significance
f_stat_p_value <- summary(model_full)$fstatistic
p_value <- pf(f_stat_p_value[1], f_stat_p_value[2], f_stat_p_value[3], lower.tail=FALSE)
print(p_value)

# 5. Check for contradiction between Q3 and Q4
# (Answer in report: If overall model is significant but individual predictors are not, discuss multicollinearity.)

# 6. Standardized Residuals vs Leverage plot
plot(hatvalues(model_full), rstandard(model_full), main="Standardized Residuals vs Leverage",
     xlab="Leverage", ylab="Standardized Residuals", pch=19, col="red")
abline(h=c(-2,2), col="blue", lty=2)  # Add threshold lines

# Identify high leverage points
high_leverage <- which(hatvalues(model_full) > (2*mean(hatvalues(model_full))))
print(high_leverage)

# 7. Check residual pattern with scatter plots
par(mfrow=c(2,3))
for (var in pollutants) {
  plot(dwaste[[var]], rstandard(model_full), main=paste("Residuals vs", var), xlab=var, ylab="Residuals", pch=19, col="green")
  abline(h=0, col="red", lty=2)
}

# 8. Best subset selection using Mallows' Cp

library(leaps)

subset_model <- regsubsets(y ~ BOD + TKN + TS + TVS + COD, data=dwaste, nbest=1, method="exhaustive")
subset_summary <- summary(subset_model)
print(subset_summary$cp)

# Identify best model (minimum Cp value)
best_subset <- which.min(subset_summary$cp)
selected_vars <- names(subset_summary$which[best_subset,])[subset_summary$which[best_subset,] == TRUE]
print(selected_vars)

# 9. Repeat regression with best subset
model_best <- lm(as.formula(paste("y ~", paste(selected_vars, collapse=" + "))), data=dwaste)
summary(model_best)

# 10. Root Mean Squared Error (RMSE) for best model
rmse <- sqrt(mean(model_best$residuals^2))
print(rmse)

#%%


#%%
# Install and load necessary packages
install.packages("alr4")  # Only if not already installed
#install.packages("leaps")  # Only if not already installed
library(alr4)
library(leaps)

# Load dataset
data(dwaste)

# 1. Set the seed using the last 5 numeric characters of your student ID (replace XXXXX)
n <- 22088
set.seed(n)

# 2. Construct response variable y
y <- log(dwaste$O2UP) + 0.01 * rnorm(length(dwaste$O2UP))

# 3. Display first few rows to check dataset
head(dwaste)

# 4. Plot y against each pollution measure
pollutants <- c("BOD", "TKN", "TS", "TVS", "COD")

par(mfrow=c(2,3))  # Arrange plots in 2 rows and 3 columns
for (var in pollutants) {
  plot(dwaste[[var]], y, main=paste("y vs", var), xlab=var, ylab="y", pch=19, col="blue")
}

# 5. Multiple linear regression: Regress y on all pollutants
model_full <- lm(y ~ BOD + TKN + TS + TVS + COD, data=dwaste)
summary(model_full)  # Check model summary

# 6. Extract significant coefficients (p < 0.05)
significant_vars <- summary(model_full)$coefficients[summary(model_full)$coefficients[,4] < 0.05, ]
print(significant_vars)

# 7. P-value of the F-statistic for overall model significance
f_stat_p_value <- summary(model_full)$fstatistic
p_value <- pf(f_stat_p_value[1], f_stat_p_value[2], f_stat_p_value[3], lower.tail=FALSE)
print(p_value)

# 8. Standardized Residuals vs Leverage plot
plot(hatvalues(model_full), rstandard(model_full), main="Standardized Residuals vs Leverage",
     xlab="Leverage", ylab="Standardized Residuals", pch=19, col="red")
abline(h=c(-2,2), col="blue", lty=2)  # Add threshold lines

# Identify high leverage points
high_leverage <- which(hatvalues(model_full) > (2*mean(hatvalues(model_full))))
print(high_leverage)

# 9. Residuals vs. each predictor variable
par(mfrow=c(2,3))
for (var in pollutants) {
  plot(dwaste[[var]], rstandard(model_full), main=paste("Residuals vs", var), xlab=var, ylab="Residuals", pch=19, col="green")
  abline(h=0, col="red", lty=2)
}

# 10. Best subset selection using Mallows' Cp
subset_model <- regsubsets(y ~ BOD + TKN + TS + TVS + COD, data=dwaste, nbest=1, method="exhaustive")
subset_summary <- summary(subset_model)

# Identify best model based on minimum Cp
best_subset <- which.min(subset_summary$cp)

# Remove "Intercept" from selected variable names
selected_vars <- names(subset_summary$which[best_subset,])[subset_summary$which[best_subset,] == TRUE]
selected_vars <- selected_vars[selected_vars != "(Intercept)"]

# Print best subset of variables
print(selected_vars)

# 11. Re-run regression with best subset model
formula_best <- as.formula(paste("y ~", paste(selected_vars, collapse=" + ")))
model_best <- lm(formula_best, data=dwaste)
summary(model_best)

# 12. Compute RMSE for prediction model
rmse <- sqrt(mean(model_best$residuals^2))
print(rmse)

#%%


#%%
# Install and load required packages
if (!require("lmreg")) install.packages("lmreg", repos = "http://cran.us.r-project.org")

if (!require("boot")) install.packages("boot", repos = "http://cran.us.r-project.org")

library(lmreg)
library(boot)

# Load the dataset
data("imf2015", package = "lmreg")

# Fit the regression model
model <- lm(UNMP ~ EXP + INFL + INV, data = imf2015)

# Define new observation
new_data <- data.frame(EXP = 42, INFL = 0.12, INV = 22)

# Define bootstrapping function
boot_pred <- function(data, indices) {
  d <- data[indices, ]  # Resample data
  fit <- lm(UNMP ~ EXP + INFL + INV, data = d)  # Refit model
  predict(fit, newdata = new_data)  # Predict value
}

# Perform parametric bootstrap with 1000 replicates
set.seed(123)  # Set seed for reproducibility
boot_results <- boot(imf2015, boot_pred, R = 1000)

# Extract 95% lower confidence bound
lower_ci <- quantile(boot_results$t, probs = 0.025)

# Round to the nearest whole number
rounded_lower_ci <- round(lower_ci)

# Print the result
cat("The 95% lower confidence limit for UNMP is:", rounded_lower_ci, "\n")

#%%

# Load necessary package
if (!require("lmreg")) install.packages("lmreg", repos = "http://cran.us.r-project.org")
library(lmreg)

# Load the dataset
data("imf2015", package = "lmreg")

getwd()

setwd("/Users/ajk/Library/CloudStorage/OneDrive-Personal/ajkTech/CODE-LAB-REPO/WsPy1/ISIpgdas") 
# Export dataset to CSV
write.csv(imf2015, file = "/Users/ajk/Downloads/imf2015_export.csv", row.names = TRUE)

# Confirm the file has been saved
data("MinnWater", package = "alr4")
write.csv(MinnWater, file = "/Users/ajk/Downloads/MinnWater.csv", row.names = TRUE)

