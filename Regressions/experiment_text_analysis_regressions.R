# Clear environment
rm(list=ls())

# Install the necessary packages for robust standard errors
library(sandwich)
library(lmtest)
library(readxl)
library(stargazer)
library(dplyr)

# Load the data
mydata <- read_excel("add_to_path/data.xlsx", col_names = TRUE)
head(mydata)

# Assuming Birth_Year is in the data frame `df`
mydata$Age <- 2024 - mydata$Birth_Year_cleaned  # Replace 2024 with the current year

# Get the top 3 most common categories for Education
top3_education <- names(sort(table(mydata$Education), decreasing = TRUE))[1:3]

# Get the top 3 most common categories for Income_group
top3_income <- names(sort(table(mydata$Income_group), decreasing = TRUE))[1:3]

# Calculate robust standard errors and display using stargazer for each model
robust_se <- function(model) {
  coeftest(model, vcov = vcovHC(model, type = "HC1")) # "HC1" is commonly used
}

mydata$retention <- ifelse(mydata$Q121 == "0.006" & mydata$Q122 == "0.042" & mydata$Q123 == "0.76" & mydata$Q124 == "£1 million" & mydata$Q127 == "0.51" & mydata$Q126 == "Germany" & mydata$Q214 == "30" , 1, 0)
head(mydata$retention)

################################################################################################################
# Model 1 with robust SE
model1 <- lm(Balance_combined_score ~ Treatment + Gender + Age + Education + Income_group, data = mydata) #add/remove + Religiosity + statistics_exposure_pca
robust_model1 <- robust_se(model1)

################################################################################################################
# Model 2 with robust SE
model2 <- lm(Empathy_combined_score ~ Treatment + Gender + Age + Education + Income_group, data = mydata) #add/remove + Religiosity + statistics_exposure_pca
robust_model2 <- robust_se(model2)
summary(model1)
################################################################################################################
# Model 3 with robust SE
model3 <- lm(Favorability_combined_score ~ Treatment + Gender + Age + Education + Income_group, data = mydata)  #add/remove + Religiosity + statistics_exposure_pca
robust_model3 <- robust_se(model3)

################################################################################################################
# Model 4 with robust SE
model4 <- lm(Shocks_combined_score ~ Treatment + Gender + Age + Education + Income_group, data = mydata)  #add/remove + Religiosity + statistics_exposure_pca
robust_model4 <- robust_se(model4)

################################################################################################################
# Model 5 with robust SE
model5 <- lm(learning_pca ~ Treatment + Balance_combined_score + Empathy_combined_score + 
               Favorability_combined_score + Shocks_combined_score, data = mydata)
library(sandwich)
library(lmtest)

# HC1 is a common robust SE estimator
robust_se <- sqrt(diag(vcovHC(model5, type = "HC1")))

library(stargazer)

stargazer(model5,
          se = list(robust_se),  # provide the robust SE vector here
          type = "latex",
          title = "Model 5: Learning Outcome Regression with Robust Standard Errors",
          label = "tab:robust_model5",
          dep.var.labels = "Learning (PCA Score)",
          omit.stat = c("f", "ser"),
          no.space = TRUE)



# Display the models with robust standard errors in stargazer
stargazer(model1, model2, model3, model4,
          type = "latex",
          se = list(robust_model1[, 2], robust_model2[, 2], robust_model3[, 2], robust_model4[, 2]),
          p = list(robust_model1[, 4], robust_model2[, 4], robust_model3[, 4], robust_model4[, 4]),
          title = "Regression Results with Robust Standard Errors",
          keep = c("Constant", "Treatment", "Gender", "Age", top3_education, top3_income),
          intercept.bottom = TRUE,
          notes = "Note: Robust standard errors in parentheses",
          notes.append = FALSE)



