# Clear environment
rm(list=ls())

library(sandwich)
library(lmtest)
library(readxl)
library(stargazer)
library(dplyr)
library(broom)
library(ggplot2)
library(zoo)
library(patchwork)
library(robustbase) 

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

mydata$T1 <- ifelse(mydata$Treatment == "Short paper", 1, 0)
mydata$T2 <- ifelse(mydata$Treatment == "Short paper and Video", 1, 0)
mydata$T3 <- ifelse(mydata$Treatment == "Video", 1, 0)
mydata$T4 <-ifelse(mydata$Treatment == "Video and Short paper", 1, 0)

mydata$Male <- ifelse(mydata$Gender == "Male", 1, 0)
mydata$SSAfrica <- ifelse(mydata$Q15 == "Yes", 1, 0)
mydata$Race <- mydata$Ethnic_group
mydata$father_immigrant <- ifelse(mydata$Q190 == "Yes", 1, 0)
mydata$mother_immigrant <- ifelse(mydata$Q192 == "Yes", 1, 0)


# Model 1: Without any controls
model1 <- lm(learning_pca ~ T2 + T3 + T4, data = mydata)
robust_model1 <- robust_se(model1)
# Model 2: With demographic controls
model2 <- lm(learning_pca ~ T2 + T3 + T4 + Gender + Age + Race, data = mydata)
robust_model2 <- robust_se(model2)
# Model 3: With all controls
model3 <- lm(learning_pca ~ T2 + T3 + T4 + Gender + Age + Race + Education + Income_group + Weekend, data = mydata)
robust_model3 <- robust_se(model3)


# Extract coefficients and confidence intervals
results1 <- tidy(robust_model1, conf.int = TRUE) %>% filter(term %in% c("T2", "T3", "T4"))
results2 <- tidy(robust_model2, conf.int = TRUE) %>% filter(term %in% c("T2", "T3", "T4"))
results3 <- tidy(robust_model3, conf.int = TRUE) %>% filter(term %in% c("T2", "T3", "T4"))

# Add a column to identify the model
results1$model <- "No Controls"
results2$model <- "Demographic Controls"
results3$model <- "Full Controls"

# Combine results
results <- bind_rows(results1, results2, results3)

ggplot(results, aes(x = term, y = estimate, color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +  # Dots for estimates
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, position = position_dodge(width = 0.5)) +  # Error bars
  labs(
       x = "Treatment",
       y = "Coefficient Estimate",
       color = "Model",
       shape = "Model") +
  ylim(-1, 1) +  # Set y-axis limits
  theme_minimal() +
  theme(legend.position = "bottom")


###############################################################################################################################
# Heterogeneity in Learning plots
###############################################################################################################################

model4 <- lm(learning_pca ~ Pooled_Treatment + perspectives_pca + Pooled_Treatment*perspectives_pca, data = mydata)
robust_model4 <- robust_se(model4)
summary(model4)
model4full <- lm(learning_pca ~ Pooled_Treatment + perspectives_pca + Pooled_Treatment*perspectives_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model4full <- robust_se(model4full)

model5 <- lm(learning_pca ~ Pooled_Treatment + vividness_pca + Pooled_Treatment*vividness_pca, data = mydata)
robust_model5 <- robust_se(model5)
model5full <- lm(learning_pca ~ Pooled_Treatment + vividness_pca + Pooled_Treatment*vividness_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model5full <- robust_se(model5full)

model6 <- lm(learning_pca ~ Pooled_Treatment + parent + Pooled_Treatment*parent, data = mydata)
robust_model6 <- robust_se(model6)
model6full <- lm(learning_pca ~ Pooled_Treatment + parent + Pooled_Treatment*parent + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model6full <- robust_se(model6full)

model7 <- lm(learning_pca ~ Pooled_Treatment + Developing_lived + Pooled_Treatment*Developing_lived, data = mydata)
robust_model7 <- robust_se(model7)
model7full <- lm(learning_pca ~ Pooled_Treatment + Developing_lived + Pooled_Treatment*Developing_lived + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model7full <- robust_se(model7full)

model8 <- lm(learning_pca ~ Pooled_Treatment + self_vs_others_pca + Pooled_Treatment*self_vs_others_pca, data = mydata)
robust_model8 <- robust_se(model8)
model8full <- lm(learning_pca ~ Pooled_Treatment + self_vs_others_pca + Pooled_Treatment*self_vs_others_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model8full <- robust_se(model8full)

model9 <- lm(learning_pca ~ Pooled_Treatment + Employment_status + Pooled_Treatment*Employment_status, data = mydata)
robust_model9 <- robust_se(model9)
model9full <- lm(learning_pca ~ Pooled_Treatment + Employment_status + Pooled_Treatment*Employment_status + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model9full <- robust_se(model9full)

model10 <- lm(learning_pca ~ Pooled_Treatment + Liberal + Pooled_Treatment*Liberal, data = mydata)
robust_model10 <- robust_se(model10)
model10full <- lm(learning_pca ~ Pooled_Treatment + Liberal + Pooled_Treatment*Liberal + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model10full <- robust_se(model10full)

model11 <- lm(learning_pca ~ Pooled_Treatment + Religiosity + Pooled_Treatment*Religiosity, data = mydata)
robust_model11 <- robust_se(model11)
model11full <- lm(learning_pca ~ Pooled_Treatment + Religiosity + Pooled_Treatment*Religiosity + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model11full <- robust_se(model11full)

model12 <- lm(learning_pca ~ Pooled_Treatment + Immigrants_share + Pooled_Treatment*Immigrants_share, data = mydata)
robust_model12 <- robust_se(model12)
model12full <- lm(learning_pca ~ Pooled_Treatment + Immigrants_share + Pooled_Treatment*Immigrants_share + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model12full <- robust_se(model12full)

model13 <- lm(learning_pca ~ Pooled_Treatment + zero_sum_pca + Pooled_Treatment*zero_sum_pca, data = mydata)
robust_model13 <- robust_se(model13)
model13full <- lm(learning_pca ~ Pooled_Treatment + zero_sum_pca + Pooled_Treatment*zero_sum_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model13full <- robust_se(model13full)

model14 <- lm(learning_pca ~ Pooled_Treatment + moral_universalism_pca + Pooled_Treatment*moral_universalism_pca, data = mydata)
robust_model14 <- robust_se(model14)
model14full <- lm(learning_pca ~ Pooled_Treatment + moral_universalism_pca + Pooled_Treatment*moral_universalism_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model14full <- robust_se(model14full)

model15 <- lm(learning_pca ~ Pooled_Treatment + shoes_pca + Pooled_Treatment*shoes_pca, data = mydata)
robust_model15 <- robust_se(model15)
model15full <- lm(learning_pca ~ Pooled_Treatment + shoes_pca + Pooled_Treatment*shoes_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model15full <- robust_se(model15full)

model16 <- lm(learning_pca ~ Pooled_Treatment + father_immigrant + Pooled_Treatment*father_immigrant, data = mydata)
robust_model16 <- robust_se(model16)
model16full <- lm(learning_pca ~ Pooled_Treatment + father_immigrant + Pooled_Treatment*father_immigrant + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model16full <- robust_se(model16full)

model17 <- lm(learning_pca ~ Pooled_Treatment + mother_immigrant + Pooled_Treatment*mother_immigrant, data = mydata)
robust_model17 <- robust_se(model17)
model17full <- lm(learning_pca ~ Pooled_Treatment + mother_immigrant + Pooled_Treatment*mother_immigrant + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model17full <- robust_se(model17full)

model18 <- lm(learning_pca ~ Pooled_Treatment + statistics_exposure_pca + Pooled_Treatment*statistics_exposure_pca, data = mydata)
robust_model18 <- robust_se(model18)
model18full <- lm(learning_pca ~ Pooled_Treatment + statistics_exposure_pca + Pooled_Treatment*statistics_exposure_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model18full <- robust_se(model18full)




# Function to prepare data for plotting
prepare_plot_data <- function(model1, model2, model1_name, model2_name) {
  # Replace colons with asterisks in model names
  model1_name <- gsub(":", "*", model1_name)
  model2_name <- gsub(":", "*", model2_name)
  
  # Extract coefficients and confidence intervals
  coef1 <- coef(model1)
  conf1 <- confint(model1)
  coef2 <- coef(model2)
  conf2 <- confint(model2)
  
  # Create data frames for each model
  data1 <- data.frame(
    Variable = names(coef1),
    Estimate = coef1,
    CI_low = conf1[, 1],
    CI_high = conf1[, 2],
    Model = model1_name
  )
  
  data2 <- data.frame(
    Variable = names(coef2),
    Estimate = coef2,
    CI_low = conf2[, 1],
    CI_high = conf2[, 2],
    Model = model2_name
  )
  
  # Combine data and filter common variables
  combined_data <- rbind(data1, data2)
  common_vars <- intersect(data1$Variable, data2$Variable)
  combined_data <- subset(combined_data, Variable %in% common_vars)
  
  # Use dplyr to preprocess Variable names
  combined_data <- combined_data %>%
    mutate(
      Variable = gsub("_", " ", Variable) # Replace underscores with spaces
    )
  
  return(combined_data)
}

# Function to plot coefficients and confidence intervals
plot_coefficients <- function(data, model_name) {
  # Replace colons with asterisks in the model name
  model_name <- gsub(":", "*", model_name)
  
  # Filter out the intercept
  data <- subset(data, Variable != "(Intercept)")
  
  ggplot(data, aes(x = Variable, y = Estimate, color = Model)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high), 
                  width = 0.2, 
                  position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + # Dashed line at 0
    coord_flip() + # Flip for better readability
    labs(
      x = "Variable",
      y = "Estimate") +
    theme_minimal() +
    ylim(-0.6, 0.6) +   # Set y-axis limits
    theme(
      axis.text.x = element_text(color = "black"),  # Black color for x-axis variable names
      axis.text.y = element_text(color = "black"),  # Black color for y-axis variable names
      axis.title.x = element_text(color = "black"), # Black color for x-axis title
      axis.title.y = element_text(color = "black")  # Black color for y-axis title
    )
}

# List of models and their names
models <- list(
  list(robust_model4, robust_model4full, "No Controls", "Full Controls"),
  list(robust_model5, robust_model5full, "No Controls", "Full Controls"),
  list(robust_model6, robust_model6full, "No Controls", "Full Controls"),
  list(robust_model7, robust_model7full, "No Controls", "Full Controls"),
  list(robust_model8, robust_model8full, "No Controls", "Full Controls"),
  list(robust_model9, robust_model9full, "No Controls", "Full Controls"),
  list(robust_model10, robust_model10full, "No Controls", "Full Controls"),
  list(robust_model11, robust_model11full, "No Controls", "Full Controls"),
  list(robust_model12, robust_model12full, "No Controls", "Full Controls"),
  list(robust_model13, robust_model13full, "No Controls", "Full Controls"),
  list(robust_model14, robust_model14full, "No Controls", "Full Controls"),
  list(robust_model15, robust_model15full, "No Controls", "Full Controls"),
  list(robust_model16, robust_model16full, "No Controls", "Full Controls"),
  list(robust_model17, robust_model17full, "No Controls", "Full Controls"),
  list(robust_model18, robust_model18full, "No Controls", "Full Controls")
)

# Directory to save the plots
output_dir <- "C:/Users/BENDAJ/Regressions/Plots/Learning" # Change to your desired directory

# Create the directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Generate and save plots for each pair of models
for (i in 1:length(models)) {
  # Prepare data
  data_to_plot <- prepare_plot_data(models[[i]][[1]], models[[i]][[2]], models[[i]][[3]], models[[i]][[4]])
  
  # Create the plot
  plot <- plot_coefficients(data_to_plot, paste(models[[i]][[3]], "vs", models[[i]][[4]]))
  
  # Display the plot in the viewer
  print(plot)
  
  # Save the plot with sequential file names (1.png, 2.png, ..., 18.png)
  plot_filename <- paste0(output_dir, "/", i, ".png")
  
  # Save the plot
  ggsave(filename = plot_filename, plot = plot, width = 8, height = 4)
}




#Combined plot for Traits
# Function to extract and combine data for specific variables across models
prepare_combined_plot_data <- function(models, variables) {
  combined_data <- data.frame()
  
  for (i in seq_along(models)) {
    model1 <- models[[i]][[1]]  # No Controls
    model2 <- models[[i]][[2]]  # Full Controls
    model1_name <- models[[i]][[3]]
    model2_name <- models[[i]][[4]]
    
    coef1 <- coef(model1)
    conf1 <- confint(model1)
    coef2 <- coef(model2)
    conf2 <- confint(model2)
    
    # Create data frames for model 1 and model 2
    data1 <- data.frame(
      Variable = names(coef1),
      Estimate = coef1,
      CI_low = conf1[, 1],
      CI_high = conf1[, 2],
      Model = model1_name
    )
    
    data2 <- data.frame(
      Variable = names(coef2),
      Estimate = coef2,
      CI_low = conf2[, 1],
      CI_high = conf2[, 2],
      Model = model2_name
    )
    
    # Combine and filter by specified variables and their interactions
    combined_data <- rbind(
      combined_data,
      subset(data1, Variable %in% variables | grepl("Pooled_Treatment", Variable)),
      subset(data2, Variable %in% variables | grepl("Pooled_Treatment", Variable))
    )
  }
  
  # Use dplyr to clean variable names
  combined_data <- combined_data %>%
    mutate(Variable = gsub("_", " ", Variable))  # Replace underscores with spaces
  
  return(combined_data)
}

# Specify the variables of interest
variables_of_interest <- c("vividness_pca", "moral_universalism_pca", 
                           "zero_sum_pca", "perspectives_pca")

# Specify the models related to these variables
models_of_interest <- list(
  list(robust_model5, robust_model5full, "Vividness (No Controls)", "Vividness (Full Controls)"),
  list(robust_model14, robust_model14full, "Moral Universalism (No Controls)", "Moral Universalism (Full Controls)"),
  list(robust_model13, robust_model13full, "Zero Sum (No Controls)", "Zero Sum (Full Controls)"),
  list(robust_model4, robust_model4full, "Perspectives (No Controls)", "Perspectives (Full Controls)")
)

# Prepare data for plotting
combined_plot_data <- prepare_combined_plot_data(models_of_interest, variables_of_interest)


# Adjust the order of the variables for plotting
combined_plot_data$Variable <- factor(combined_plot_data$Variable, levels = c(
  "perspectives pca",
  "zero sum pca",
  "moral universalism pca",
  "vividness pca",
  "Pooled Treatment:perspectives pca",
  "Pooled Treatment:zero sum pca",
  "Pooled Treatment:moral universalism pca",
  "Pooled Treatment:vividness pca",
  "Pooled Treatment"
))


# Plotting the coefficients and confidence intervals
traits_plot <- ggplot(combined_plot_data, aes(x = Variable, y = Estimate, color = Model)) +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  coord_flip() +
  labs(title = NULL,
       x = "Variable",
       y = "Estimate") +
  theme_minimal() +
  ylim(-0.6, 0.6) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )


# Plot for Experiences in Learning
variables_of_interest <- c("parent", "Developing_lived", "statistics_exposure_pca")

# Update the models of interest to include only the models for the new variables
models_of_interest <- list(
  list(robust_model6, robust_model6full, "Parent (No Controls)", "Parent (Full Controls)"),
  list(robust_model7, robust_model7full, "Developing Lived (No Controls)", "Developing Lived (Full Controls)"),
  list(robust_model18, robust_model18full, "Statistics Exposure (No Controls)", "Statistics Exposure (Full Controls)")
)

# Prepare data for plotting using the function defined earlier
combined_plot_data <- prepare_combined_plot_data(models_of_interest, variables_of_interest)

# Adjust the order of the variables for plotting (only the new variables now)
combined_plot_data$Variable <- factor(combined_plot_data$Variable, levels = c(
  "parent",
  "Developing lived",
  "statistics exposure pca",
  "Pooled Treatment:parent",
  "Pooled Treatment:Developing lived",
  "Pooled Treatment:statistics exposure pca",
  "Pooled Treatment"
))

# Plotting the coefficients and confidence intervals
exp_plot <- ggplot(combined_plot_data, aes(x = Variable, y = Estimate, color = Model)) +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  coord_flip() +
  labs(title = NULL,
       x = "Variable",
       y = "Estimate") +
  theme_minimal() +
  ylim(-0.6, 0.6) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )



output_dir <- "C:/Users/BENDAJ/Regressions/Plots/Learning"

# Create the directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE) # Use 'recursive = TRUE' for nested directories
}

# Save the Traits Plot
traits_filename <- file.path(output_dir, "traits_learning.png")
ggsave(filename = traits_filename, plot = traits_plot, width = 8, height = 4)

# Save the Experiences in Learning Plot
exp_filename <- file.path(output_dir, "experiences_learning.png")
ggsave(filename = exp_filename, plot = exp_plot, width = 8, height = 4)










###############################################################################################################################
# Heterogeneity in Policy Attitudes plots - Immigrants
###############################################################################################################################

model4 <- lm(immigrants_pca ~ Pooled_Treatment + perspectives_pca + Pooled_Treatment*perspectives_pca, data = mydata)
robust_model4 <- robust_se(model4)
model4full <- lm(immigrants_pca ~ Pooled_Treatment + perspectives_pca + Pooled_Treatment*perspectives_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model4full <- robust_se(model4full)

model5 <- lm(immigrants_pca ~ Pooled_Treatment + vividness_pca + Pooled_Treatment*vividness_pca, data = mydata)
robust_model5 <- robust_se(model5)
model5full <- lm(immigrants_pca ~ Pooled_Treatment + vividness_pca + Pooled_Treatment*vividness_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model5full <- robust_se(model5full)

model6 <- lm(immigrants_pca ~ Pooled_Treatment + parent + Pooled_Treatment*parent, data = mydata)
robust_model6 <- robust_se(model6)
model6full <- lm(immigrants_pca ~ Pooled_Treatment + parent + Pooled_Treatment*parent + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model6full <- robust_se(model6full)

model7 <- lm(immigrants_pca ~ Pooled_Treatment + Developing_lived + Pooled_Treatment*Developing_lived, data = mydata)
robust_model7 <- robust_se(model7)
model7full <- lm(immigrants_pca ~ Pooled_Treatment + Developing_lived + Pooled_Treatment*Developing_lived + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model7full <- robust_se(model7full)

model8 <- lm(immigrants_pca ~ Pooled_Treatment + self_vs_others_pca + Pooled_Treatment*self_vs_others_pca, data = mydata)
robust_model8 <- robust_se(model8)
model8full <- lm(immigrants_pca ~ Pooled_Treatment + self_vs_others_pca + Pooled_Treatment*self_vs_others_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model8full <- robust_se(model8full)

model9 <- lm(immigrants_pca ~ Pooled_Treatment + Employment_status + Pooled_Treatment*Employment_status, data = mydata)
robust_model9 <- robust_se(model9)
model9full <- lm(immigrants_pca ~ Pooled_Treatment + Employment_status + Pooled_Treatment*Employment_status + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model9full <- robust_se(model9full)

model10 <- lm(immigrants_pca ~ Pooled_Treatment + Liberal + Pooled_Treatment*Liberal, data = mydata)
robust_model10 <- robust_se(model10)
model10full <- lm(immigrants_pca ~ Pooled_Treatment + Liberal + Pooled_Treatment*Liberal + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model10full <- robust_se(model10full)

model11 <- lm(immigrants_pca ~ Pooled_Treatment + Religiosity + Pooled_Treatment*Religiosity, data = mydata)
robust_model11 <- robust_se(model11)
model11full <- lm(immigrants_pca ~ Pooled_Treatment + Religiosity + Pooled_Treatment*Religiosity + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model11full <- robust_se(model11full)

model12 <- lm(immigrants_pca ~ Pooled_Treatment + Immigrants_share + Pooled_Treatment*Immigrants_share, data = mydata)
robust_model12 <- robust_se(model12)
model12full <- lm(immigrants_pca ~ Pooled_Treatment + Immigrants_share + Pooled_Treatment*Immigrants_share + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model12full <- robust_se(model12full)

model13 <- lm(immigrants_pca ~ Pooled_Treatment + zero_sum_pca + Pooled_Treatment*zero_sum_pca, data = mydata)
robust_model13 <- robust_se(model13)
model13full <- lm(immigrants_pca ~ Pooled_Treatment + zero_sum_pca + Pooled_Treatment*zero_sum_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model13full <- robust_se(model13full)

model14 <- lm(immigrants_pca ~ Pooled_Treatment + moral_universalism_pca + Pooled_Treatment*moral_universalism_pca, data = mydata)
robust_model14 <- robust_se(model14)
model14full <- lm(immigrants_pca ~ Pooled_Treatment + moral_universalism_pca + Pooled_Treatment*moral_universalism_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model14full <- robust_se(model14full)

model15 <- lm(immigrants_pca ~ Pooled_Treatment + shoes_pca + Pooled_Treatment*shoes_pca, data = mydata)
robust_model15 <- robust_se(model15)
model15full <- lm(immigrants_pca ~ Pooled_Treatment + shoes_pca + Pooled_Treatment*shoes_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model15full <- robust_se(model15full)

model16 <- lm(immigrants_pca ~ Pooled_Treatment + father_immigrant + Pooled_Treatment*father_immigrant, data = mydata)
robust_model16 <- robust_se(model16)
model16full <- lm(immigrants_pca ~ Pooled_Treatment + father_immigrant + Pooled_Treatment*father_immigrant + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model16full <- robust_se(model16full)

model17 <- lm(immigrants_pca ~ Pooled_Treatment + mother_immigrant + Pooled_Treatment*mother_immigrant, data = mydata)
robust_model17 <- robust_se(model17)
model17full <- lm(immigrants_pca ~ Pooled_Treatment + mother_immigrant + Pooled_Treatment*mother_immigrant + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model17full <- robust_se(model17full)

model18 <- lm(immigrants_pca ~ Pooled_Treatment + statistics_exposure_pca + Pooled_Treatment*statistics_exposure_pca, data = mydata)
robust_model18 <- robust_se(model18)
model18full <- lm(immigrants_pca ~ Pooled_Treatment + statistics_exposure_pca + Pooled_Treatment*statistics_exposure_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model18full <- robust_se(model18full)

# Function to prepare data for plotting
prepare_plot_data <- function(model1, model2, model1_name, model2_name) {
  # Replace colons with asterisks in model names
  model1_name <- gsub(":", "*", model1_name)
  model2_name <- gsub(":", "*", model2_name)
  
  # Extract coefficients and confidence intervals
  coef1 <- coef(model1)
  conf1 <- confint(model1)
  coef2 <- coef(model2)
  conf2 <- confint(model2)
  
  # Create data frames for each model
  data1 <- data.frame(
    Variable = names(coef1),
    Estimate = coef1,
    CI_low = conf1[, 1],
    CI_high = conf1[, 2],
    Model = model1_name
  )
  
  data2 <- data.frame(
    Variable = names(coef2),
    Estimate = coef2,
    CI_low = conf2[, 1],
    CI_high = conf2[, 2],
    Model = model2_name
  )
  
  # Combine data and filter common variables
  combined_data <- rbind(data1, data2)
  common_vars <- intersect(data1$Variable, data2$Variable)
  combined_data <- subset(combined_data, Variable %in% common_vars)
  
  # Use dplyr to preprocess Variable names
  combined_data <- combined_data %>%
    mutate(
      Variable = gsub("_", " ", Variable) # Replace underscores with spaces
    )
  
  return(combined_data)
}

# Function to plot coefficients and confidence intervals
plot_coefficients <- function(data, model_name) {
  # Replace colons with asterisks in the model name
  model_name <- gsub(":", "*", model_name)
  
  # Filter out the intercept
  data <- subset(data, Variable != "(Intercept)")
  
  ggplot(data, aes(x = Variable, y = Estimate, color = Model)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high), 
                  width = 0.2, 
                  position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + # Dashed line at 0
    coord_flip() + # Flip for better readability
    labs(title = NULL,
         x = "Variable",
         y = "Estimate") +
    theme_minimal() +
    ylim(-4, 4) +   # Set y-axis limits
    theme(
      axis.text.x = element_text(color = "black"),  # Black color for x-axis variable names
      axis.text.y = element_text(color = "black"),  # Black color for y-axis variable names
      axis.title.x = element_text(color = "black"), # Black color for x-axis title
      axis.title.y = element_text(color = "black")  # Black color for y-axis title
    )
}

# List of models and their names
models <- list(
  list(robust_model4, robust_model4full, "No Controls", "Full Controls"),
  list(robust_model5, robust_model5full, "No Controls", "Full Controls"),
  list(robust_model6, robust_model6full, "No Controls", "Full Controls"),
  list(robust_model7, robust_model7full, "No Controls", "Full Controls"),
  list(robust_model8, robust_model8full, "No Controls", "Full Controls"),
  list(robust_model9, robust_model9full, "No Controls", "Full Controls"),
  list(robust_model10, robust_model10full, "No Controls", "Full Controls"),
  list(robust_model11, robust_model11full, "No Controls", "Full Controls"),
  list(robust_model12, robust_model12full, "No Controls", "Full Controls"),
  list(robust_model13, robust_model13full, "No Controls", "Full Controls"),
  list(robust_model14, robust_model14full, "No Controls", "Full Controls"),
  list(robust_model15, robust_model15full, "No Controls", "Full Controls"),
  list(robust_model16, robust_model16full, "No Controls", "Full Controls"),
  list(robust_model17, robust_model17full, "No Controls", "Full Controls"),
  list(robust_model18, robust_model18full, "No Controls", "Full Controls")
)

# Directory to save the plots
output_dir <- "C:/Users/BENDAJ/Regressions/Plots/Immigrants" # Change to your desired directory

# Create the directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Generate and save plots for each pair of models
for (i in 1:length(models)) {
  # Prepare data
  data_to_plot <- prepare_plot_data(models[[i]][[1]], models[[i]][[2]], models[[i]][[3]], models[[i]][[4]])
  
  # Create the plot
  plot <- plot_coefficients(data_to_plot, paste(models[[i]][[3]], "vs", models[[i]][[4]]))
  
  # Display the plot in the viewer
  print(plot)
  
  # Save the plot with sequential file names (1.png, 2.png, ..., 18.png)
  plot_filename <- paste0(output_dir, "/", i, ".png")
  
  # Save the plot
  ggsave(filename = plot_filename, plot = plot, width = 8, height = 4)
}

#Combined plot for Traits
# Function to extract and combine data for specific variables across models
prepare_combined_plot_data <- function(models, variables) {
  combined_data <- data.frame()
  
  for (i in seq_along(models)) {
    model1 <- models[[i]][[1]]  # No Controls
    model2 <- models[[i]][[2]]  # Full Controls
    model1_name <- models[[i]][[3]]
    model2_name <- models[[i]][[4]]
    
    coef1 <- coef(model1)
    conf1 <- confint(model1)
    coef2 <- coef(model2)
    conf2 <- confint(model2)
    
    # Create data frames for model 1 and model 2
    data1 <- data.frame(
      Variable = names(coef1),
      Estimate = coef1,
      CI_low = conf1[, 1],
      CI_high = conf1[, 2],
      Model = model1_name
    )
    
    data2 <- data.frame(
      Variable = names(coef2),
      Estimate = coef2,
      CI_low = conf2[, 1],
      CI_high = conf2[, 2],
      Model = model2_name
    )
    
    # Combine and filter by specified variables and their interactions
    combined_data <- rbind(
      combined_data,
      subset(data1, Variable %in% variables | grepl("Pooled_Treatment", Variable)),
      subset(data2, Variable %in% variables | grepl("Pooled_Treatment", Variable))
    )
  }
  
  # Use dplyr to clean variable names
  combined_data <- combined_data %>%
    mutate(Variable = gsub("_", " ", Variable))  # Replace underscores with spaces
  
  return(combined_data)
}

# Specify the variables of interest
variables_of_interest <- c("vividness_pca", "moral_universalism_pca", 
                           "zero_sum_pca", "perspectives_pca")

# Specify the models related to these variables
models_of_interest <- list(
  list(robust_model5, robust_model5full, "Vividness (No Controls)", "Vividness (Full Controls)"),
  list(robust_model14, robust_model14full, "Moral Universalism (No Controls)", "Moral Universalism (Full Controls)"),
  list(robust_model13, robust_model13full, "Zero Sum (No Controls)", "Zero Sum (Full Controls)"),
  list(robust_model4, robust_model4full, "Perspectives (No Controls)", "Perspectives (Full Controls)")
)

# Prepare data for plotting
combined_plot_data <- prepare_combined_plot_data(models_of_interest, variables_of_interest)


# Adjust the order of the variables for plotting
combined_plot_data$Variable <- factor(combined_plot_data$Variable, levels = c(
  "perspectives pca",
  "zero sum pca",
  "moral universalism pca",
  "vividness pca",
  "Pooled Treatment:perspectives pca",
  "Pooled Treatment:zero sum pca",
  "Pooled Treatment:moral universalism pca",
  "Pooled Treatment:vividness pca",
  "Pooled Treatment"
))


# Plotting the coefficients and confidence intervals
traits_plot <- ggplot(combined_plot_data, aes(x = Variable, y = Estimate, color = Model)) +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  coord_flip() +
  labs(title = NULL,
       x = "Variable",
       y = "Estimate") +
  theme_minimal() +
  ylim(-4, 4) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )


# Plot for Experiences in Learning
variables_of_interest <- c("parent", "Developing_lived", "statistics_exposure_pca")

# Update the models of interest to include only the models for the new variables
models_of_interest <- list(
  list(robust_model6, robust_model6full, "Parent (No Controls)", "Parent (Full Controls)"),
  list(robust_model7, robust_model7full, "Developing Lived (No Controls)", "Developing Lived (Full Controls)"),
  list(robust_model18, robust_model18full, "Statistics Exposure (No Controls)", "Statistics Exposure (Full Controls)")
)

# Prepare data for plotting using the function defined earlier
combined_plot_data <- prepare_combined_plot_data(models_of_interest, variables_of_interest)

# Adjust the order of the variables for plotting (only the new variables now)
combined_plot_data$Variable <- factor(combined_plot_data$Variable, levels = c(
  "parent",
  "Developing lived",
  "statistics exposure pca",
  "Pooled Treatment:parent",
  "Pooled Treatment:Developing lived",
  "Pooled Treatment:statistics exposure pca",
  "Pooled Treatment"
))

# Plotting the coefficients and confidence intervals
exp_plot <- ggplot(combined_plot_data, aes(x = Variable, y = Estimate, color = Model)) +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  coord_flip() +
  labs(title = NULL,
       x = "Variable",
       y = "Estimate") +
  theme_minimal() +
  ylim(-4, 4) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )

output_dir <- "C:/Users/BENDAJ/Regressions/Plots/Immigrants"

# Create the directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE) # Use 'recursive = TRUE' for nested directories
}

# Save the Traits Plot
traits_filename <- file.path(output_dir, "traits_immigrants.png")
ggsave(filename = traits_filename, plot = traits_plot, width = 8, height = 4)

# Save the Experiences in Learning Plot
exp_filename <- file.path(output_dir, "experiences_immigrants.png")
ggsave(filename = exp_filename, plot = exp_plot, width = 8, height = 4)


###############################################################################################################################
# Heterogeneity in Policy Attitudes plots - Refugees
###############################################################################################################################

model4 <- lm(refugees_pca ~ Pooled_Treatment + perspectives_pca + Pooled_Treatment*perspectives_pca, data = mydata)
robust_model4 <- robust_se(model4)
model4full <- lm(refugees_pca ~ Pooled_Treatment + perspectives_pca + Pooled_Treatment*perspectives_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model4full <- robust_se(model4full)

model5 <- lm(refugees_pca ~ Pooled_Treatment + vividness_pca + Pooled_Treatment*vividness_pca, data = mydata)
robust_model5 <- robust_se(model5)
model5full <- lm(refugees_pca ~ Pooled_Treatment + vividness_pca + Pooled_Treatment*vividness_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model5full <- robust_se(model5full)

model6 <- lm(refugees_pca ~ Pooled_Treatment + parent + Pooled_Treatment*parent, data = mydata)
robust_model6 <- robust_se(model6)
model6full <- lm(refugees_pca ~ Pooled_Treatment + parent + Pooled_Treatment*parent + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model6full <- robust_se(model6full)

model7 <- lm(refugees_pca ~ Pooled_Treatment + Developing_lived + Pooled_Treatment*Developing_lived, data = mydata)
robust_model7 <- robust_se(model7)
model7full <- lm(refugees_pca ~ Pooled_Treatment + Developing_lived + Pooled_Treatment*Developing_lived + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model7full <- robust_se(model7full)

model8 <- lm(refugees_pca ~ Pooled_Treatment + self_vs_others_pca + Pooled_Treatment*self_vs_others_pca, data = mydata)
robust_model8 <- robust_se(model8)
model8full <- lm(refugees_pca ~ Pooled_Treatment + self_vs_others_pca + Pooled_Treatment*self_vs_others_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model8full <- robust_se(model8full)

model9 <- lm(refugees_pca ~ Pooled_Treatment + Employment_status + Pooled_Treatment*Employment_status, data = mydata)
robust_model9 <- robust_se(model9)
model9full <- lm(refugees_pca ~ Pooled_Treatment + Employment_status + Pooled_Treatment*Employment_status + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model9full <- robust_se(model9full)

model10 <- lm(refugees_pca ~ Pooled_Treatment + Liberal + Pooled_Treatment*Liberal, data = mydata)
robust_model10 <- robust_se(model10)
model10full <- lm(refugees_pca ~ Pooled_Treatment + Liberal + Pooled_Treatment*Liberal + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model10full <- robust_se(model10full)

model11 <- lm(refugees_pca ~ Pooled_Treatment + Religiosity + Pooled_Treatment*Religiosity, data = mydata)
robust_model11 <- robust_se(model11)
model11full <- lm(refugees_pca ~ Pooled_Treatment + Religiosity + Pooled_Treatment*Religiosity + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model11full <- robust_se(model11full)

model12 <- lm(refugees_pca ~ Pooled_Treatment + Immigrants_share + Pooled_Treatment*Immigrants_share, data = mydata)
robust_model12 <- robust_se(model12)
model12full <- lm(refugees_pca ~ Pooled_Treatment + Immigrants_share + Pooled_Treatment*Immigrants_share + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model12full <- robust_se(model12full)

model13 <- lm(refugees_pca ~ Pooled_Treatment + zero_sum_pca + Pooled_Treatment*zero_sum_pca, data = mydata)
robust_model13 <- robust_se(model13)
model13full <- lm(refugees_pca ~ Pooled_Treatment + zero_sum_pca + Pooled_Treatment*zero_sum_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model13full <- robust_se(model13full)

model14 <- lm(refugees_pca ~ Pooled_Treatment + moral_universalism_pca + Pooled_Treatment*moral_universalism_pca, data = mydata)
robust_model14 <- robust_se(model14)
model14full <- lm(refugees_pca ~ Pooled_Treatment + moral_universalism_pca + Pooled_Treatment*moral_universalism_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model14full <- robust_se(model14full)

model15 <- lm(refugees_pca ~ Pooled_Treatment + shoes_pca + Pooled_Treatment*shoes_pca, data = mydata)
robust_model15 <- robust_se(model15)
model15full <- lm(refugees_pca ~ Pooled_Treatment + shoes_pca + Pooled_Treatment*shoes_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model15full <- robust_se(model15full)

model16 <- lm(refugees_pca ~ Pooled_Treatment + father_immigrant + Pooled_Treatment*father_immigrant, data = mydata)
robust_model16 <- robust_se(model16)
model16full <- lm(refugees_pca ~ Pooled_Treatment + father_immigrant + Pooled_Treatment*father_immigrant + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model16full <- robust_se(model16full)

model17 <- lm(refugees_pca ~ Pooled_Treatment + mother_immigrant + Pooled_Treatment*mother_immigrant, data = mydata)
robust_model17 <- robust_se(model17)
model17full <- lm(refugees_pca ~ Pooled_Treatment + mother_immigrant + Pooled_Treatment*mother_immigrant + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model17full <- robust_se(model17full)

model18 <- lm(refugees_pca ~ Pooled_Treatment + statistics_exposure_pca + Pooled_Treatment*statistics_exposure_pca, data = mydata)
robust_model18 <- robust_se(model18)
model18full <- lm(refugees_pca ~ Pooled_Treatment + statistics_exposure_pca + Pooled_Treatment*statistics_exposure_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model18full <- robust_se(model18full)

# Function to prepare data for plotting
prepare_plot_data <- function(model1, model2, model1_name, model2_name) {
  # Replace colons with asterisks in model names
  model1_name <- gsub(":", "*", model1_name)
  model2_name <- gsub(":", "*", model2_name)
  
  # Extract coefficients and confidence intervals
  coef1 <- coef(model1)
  conf1 <- confint(model1)
  coef2 <- coef(model2)
  conf2 <- confint(model2)
  
  # Create data frames for each model
  data1 <- data.frame(
    Variable = names(coef1),
    Estimate = coef1,
    CI_low = conf1[, 1],
    CI_high = conf1[, 2],
    Model = model1_name
  )
  
  data2 <- data.frame(
    Variable = names(coef2),
    Estimate = coef2,
    CI_low = conf2[, 1],
    CI_high = conf2[, 2],
    Model = model2_name
  )
  
  # Combine data and filter common variables
  combined_data <- rbind(data1, data2)
  common_vars <- intersect(data1$Variable, data2$Variable)
  combined_data <- subset(combined_data, Variable %in% common_vars)
  
  # Use dplyr to preprocess Variable names
  combined_data <- combined_data %>%
    mutate(
      Variable = gsub("_", " ", Variable) # Replace underscores with spaces
    )
  
  return(combined_data)
}

# Function to plot coefficients and confidence intervals
plot_coefficients <- function(data, model_name) {
  # Replace colons with asterisks in the model name
  model_name <- gsub(":", "*", model_name)
  
  # Filter out the intercept
  data <- subset(data, Variable != "(Intercept)")
  
  ggplot(data, aes(x = Variable, y = Estimate, color = Model)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high), 
                  width = 0.2, 
                  position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + # Dashed line at 0
    coord_flip() + # Flip for better readability
    labs(title = NULL,
         x = "Variable",
         y = "Estimate") +
    theme_minimal() +
    ylim(-4, 4) +   # Set y-axis limits
    theme(
      axis.text.x = element_text(color = "black"),  # Black color for x-axis variable names
      axis.text.y = element_text(color = "black"),  # Black color for y-axis variable names
      axis.title.x = element_text(color = "black"), # Black color for x-axis title
      axis.title.y = element_text(color = "black")  # Black color for y-axis title
    )
}

# List of models and their names
models <- list(
  list(robust_model4, robust_model4full, "No Controls", "Full Controls"),
  list(robust_model5, robust_model5full, "No Controls", "Full Controls"),
  list(robust_model6, robust_model6full, "No Controls", "Full Controls"),
  list(robust_model7, robust_model7full, "No Controls", "Full Controls"),
  list(robust_model8, robust_model8full, "No Controls", "Full Controls"),
  list(robust_model9, robust_model9full, "No Controls", "Full Controls"),
  list(robust_model10, robust_model10full, "No Controls", "Full Controls"),
  list(robust_model11, robust_model11full, "No Controls", "Full Controls"),
  list(robust_model12, robust_model12full, "No Controls", "Full Controls"),
  list(robust_model13, robust_model13full, "No Controls", "Full Controls"),
  list(robust_model14, robust_model14full, "No Controls", "Full Controls"),
  list(robust_model15, robust_model15full, "No Controls", "Full Controls"),
  list(robust_model16, robust_model16full, "No Controls", "Full Controls"),
  list(robust_model17, robust_model17full, "No Controls", "Full Controls"),
  list(robust_model18, robust_model18full, "No Controls", "Full Controls")
)

# Directory to save the plots
output_dir <- "C:/Users/BENDAJ/Regressions/Plots/Refugees" # Change to your desired directory

# Create the directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Generate and save plots for each pair of models
for (i in 1:length(models)) {
  # Prepare data
  data_to_plot <- prepare_plot_data(models[[i]][[1]], models[[i]][[2]], models[[i]][[3]], models[[i]][[4]])
  
  # Create the plot
  plot <- plot_coefficients(data_to_plot, paste(models[[i]][[3]], "vs", models[[i]][[4]]))
  
  # Display the plot in the viewer
  print(plot)
  
  # Save the plot with sequential file names (1.png, 2.png, ..., 18.png)
  plot_filename <- paste0(output_dir, "/", i, ".png")
  
  # Save the plot
  ggsave(filename = plot_filename, plot = plot, width = 8, height = 4)
}

#Combined plot for Traits
# Function to extract and combine data for specific variables across models
prepare_combined_plot_data <- function(models, variables) {
  combined_data <- data.frame()
  
  for (i in seq_along(models)) {
    model1 <- models[[i]][[1]]  # No Controls
    model2 <- models[[i]][[2]]  # Full Controls
    model1_name <- models[[i]][[3]]
    model2_name <- models[[i]][[4]]
    
    coef1 <- coef(model1)
    conf1 <- confint(model1)
    coef2 <- coef(model2)
    conf2 <- confint(model2)
    
    # Create data frames for model 1 and model 2
    data1 <- data.frame(
      Variable = names(coef1),
      Estimate = coef1,
      CI_low = conf1[, 1],
      CI_high = conf1[, 2],
      Model = model1_name
    )
    
    data2 <- data.frame(
      Variable = names(coef2),
      Estimate = coef2,
      CI_low = conf2[, 1],
      CI_high = conf2[, 2],
      Model = model2_name
    )
    
    # Combine and filter by specified variables and their interactions
    combined_data <- rbind(
      combined_data,
      subset(data1, Variable %in% variables | grepl("Pooled_Treatment", Variable)),
      subset(data2, Variable %in% variables | grepl("Pooled_Treatment", Variable))
    )
  }
  
  # Use dplyr to clean variable names
  combined_data <- combined_data %>%
    mutate(Variable = gsub("_", " ", Variable))  # Replace underscores with spaces
  
  return(combined_data)
}

# Specify the variables of interest
variables_of_interest <- c("vividness_pca", "moral_universalism_pca", 
                           "zero_sum_pca", "perspectives_pca")

# Specify the models related to these variables
models_of_interest <- list(
  list(robust_model5, robust_model5full, "Vividness (No Controls)", "Vividness (Full Controls)"),
  list(robust_model14, robust_model14full, "Moral Universalism (No Controls)", "Moral Universalism (Full Controls)"),
  list(robust_model13, robust_model13full, "Zero Sum (No Controls)", "Zero Sum (Full Controls)"),
  list(robust_model4, robust_model4full, "Perspectives (No Controls)", "Perspectives (Full Controls)")
)

# Prepare data for plotting
combined_plot_data <- prepare_combined_plot_data(models_of_interest, variables_of_interest)


# Adjust the order of the variables for plotting
combined_plot_data$Variable <- factor(combined_plot_data$Variable, levels = c(
  "perspectives pca",
  "zero sum pca",
  "moral universalism pca",
  "vividness pca",
  "Pooled Treatment:perspectives pca",
  "Pooled Treatment:zero sum pca",
  "Pooled Treatment:moral universalism pca",
  "Pooled Treatment:vividness pca",
  "Pooled Treatment"
))


# Plotting the coefficients and confidence intervals
traits_plot <- ggplot(combined_plot_data, aes(x = Variable, y = Estimate, color = Model)) +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  coord_flip() +
  labs(title = NULL,
       x = "Variable",
       y = "Estimate") +
  theme_minimal() +
  ylim(-4, 4) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )



# Plot for Experiences in Learning
variables_of_interest <- c("parent", "Developing_lived", "statistics_exposure_pca")

# Update the models of interest to include only the models for the new variables
models_of_interest <- list(
  list(robust_model6, robust_model6full, "Parent (No Controls)", "Parent (Full Controls)"),
  list(robust_model7, robust_model7full, "Developing Lived (No Controls)", "Developing Lived (Full Controls)"),
  list(robust_model18, robust_model18full, "Statistics Exposure (No Controls)", "Statistics Exposure (Full Controls)")
)

# Prepare data for plotting using the function defined earlier
combined_plot_data <- prepare_combined_plot_data(models_of_interest, variables_of_interest)

# Adjust the order of the variables for plotting (only the new variables now)
combined_plot_data$Variable <- factor(combined_plot_data$Variable, levels = c(
  "parent",
  "Developing lived",
  "statistics exposure pca",
  "Pooled Treatment:parent",
  "Pooled Treatment:Developing lived",
  "Pooled Treatment:statistics exposure pca",
  "Pooled Treatment"
))

# Plotting the coefficients and confidence intervals
exp_plot <- ggplot(combined_plot_data, aes(x = Variable, y = Estimate, color = Model)) +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  coord_flip() +
  labs(title = NULL,
       x = "Variable",
       y = "Estimate") +
  theme_minimal() +
  ylim(-4, 4) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )

output_dir <- "C:/Users/BENDAJ/Regressions/Plots/Refugees"

# Create the directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE) # Use 'recursive = TRUE' for nested directories
}

# Save the Traits Plot
traits_filename <- file.path(output_dir, "traits_refugees.png")
ggsave(filename = traits_filename, plot = traits_plot, width = 8, height = 4)

# Save the Experiences in Learning Plot
exp_filename <- file.path(output_dir, "experiences_refugees.png")
ggsave(filename = exp_filename, plot = exp_plot, width = 8, height = 4)





###############################################################################################################################
# Heterogeneity in Policy Attitudes plots - Climate change
###############################################################################################################################

model4 <- lm(climate_change_pca ~ Pooled_Treatment + perspectives_pca + Pooled_Treatment*perspectives_pca, data = mydata)
robust_model4 <- robust_se(model4)
model4full <- lm(climate_change_pca ~ Pooled_Treatment + perspectives_pca + Pooled_Treatment*perspectives_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model4full <- robust_se(model4full)

model5 <- lm(climate_change_pca ~ Pooled_Treatment + vividness_pca + Pooled_Treatment*vividness_pca, data = mydata)
robust_model5 <- robust_se(model5)
model5full <- lm(climate_change_pca ~ Pooled_Treatment + vividness_pca + Pooled_Treatment*vividness_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model5full <- robust_se(model5full)

model6 <- lm(climate_change_pca ~ Pooled_Treatment + parent + Pooled_Treatment*parent, data = mydata)
robust_model6 <- robust_se(model6)
model6full <- lm(climate_change_pca ~ Pooled_Treatment + parent + Pooled_Treatment*parent + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model6full <- robust_se(model6full)

model7 <- lm(climate_change_pca ~ Pooled_Treatment + Developing_lived + Pooled_Treatment*Developing_lived, data = mydata)
robust_model7 <- robust_se(model7)
model7full <- lm(climate_change_pca ~ Pooled_Treatment + Developing_lived + Pooled_Treatment*Developing_lived + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model7full <- robust_se(model7full)

model8 <- lm(climate_change_pca ~ Pooled_Treatment + self_vs_others_pca + Pooled_Treatment*self_vs_others_pca, data = mydata)
robust_model8 <- robust_se(model8)
model8full <- lm(climate_change_pca ~ Pooled_Treatment + self_vs_others_pca + Pooled_Treatment*self_vs_others_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model8full <- robust_se(model8full)

model9 <- lm(climate_change_pca ~ Pooled_Treatment + Employment_status + Pooled_Treatment*Employment_status, data = mydata)
robust_model9 <- robust_se(model9)
model9full <- lm(climate_change_pca ~ Pooled_Treatment + Employment_status + Pooled_Treatment*Employment_status + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model9full <- robust_se(model9full)

model10 <- lm(climate_change_pca ~ Pooled_Treatment + Liberal + Pooled_Treatment*Liberal, data = mydata)
robust_model10 <- robust_se(model10)
model10full <- lm(climate_change_pca ~ Pooled_Treatment + Liberal + Pooled_Treatment*Liberal + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model10full <- robust_se(model10full)

model11 <- lm(climate_change_pca ~ Pooled_Treatment + Religiosity + Pooled_Treatment*Religiosity, data = mydata)
robust_model11 <- robust_se(model11)
model11full <- lm(climate_change_pca ~ Pooled_Treatment + Religiosity + Pooled_Treatment*Religiosity + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model11full <- robust_se(model11full)

model12 <- lm(climate_change_pca ~ Pooled_Treatment + Immigrants_share + Pooled_Treatment*Immigrants_share, data = mydata)
robust_model12 <- robust_se(model12)
model12full <- lm(climate_change_pca ~ Pooled_Treatment + Immigrants_share + Pooled_Treatment*Immigrants_share + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model12full <- robust_se(model12full)

model13 <- lm(climate_change_pca ~ Pooled_Treatment + zero_sum_pca + Pooled_Treatment*zero_sum_pca, data = mydata)
robust_model13 <- robust_se(model13)
model13full <- lm(climate_change_pca ~ Pooled_Treatment + zero_sum_pca + Pooled_Treatment*zero_sum_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model13full <- robust_se(model13full)

model14 <- lm(climate_change_pca ~ Pooled_Treatment + moral_universalism_pca + Pooled_Treatment*moral_universalism_pca, data = mydata)
robust_model14 <- robust_se(model14)
model14full <- lm(climate_change_pca ~ Pooled_Treatment + moral_universalism_pca + Pooled_Treatment*moral_universalism_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model14full <- robust_se(model14full)

model15 <- lm(climate_change_pca ~ Pooled_Treatment + shoes_pca + Pooled_Treatment*shoes_pca, data = mydata)
robust_model15 <- robust_se(model15)
model15full <- lm(climate_change_pca ~ Pooled_Treatment + shoes_pca + Pooled_Treatment*shoes_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model15full <- robust_se(model15full)

model16 <- lm(climate_change_pca ~ Pooled_Treatment + father_immigrant + Pooled_Treatment*father_immigrant, data = mydata)
robust_model16 <- robust_se(model16)
model16full <- lm(climate_change_pca ~ Pooled_Treatment + father_immigrant + Pooled_Treatment*father_immigrant + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model16full <- robust_se(model16full)

model17 <- lm(climate_change_pca ~ Pooled_Treatment + mother_immigrant + Pooled_Treatment*mother_immigrant, data = mydata)
robust_model17 <- robust_se(model17)
model17full <- lm(climate_change_pca ~ Pooled_Treatment + mother_immigrant + Pooled_Treatment*mother_immigrant + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model17full <- robust_se(model17full)

model18 <- lm(climate_change_pca ~ Pooled_Treatment + statistics_exposure_pca + Pooled_Treatment*statistics_exposure_pca, data = mydata)
robust_model18 <- robust_se(model18)
model18full <- lm(climate_change_pca ~ Pooled_Treatment + statistics_exposure_pca + Pooled_Treatment*statistics_exposure_pca + Age + Gender + Race + Income_group + Education + Minutes, data = mydata)
robust_model18full <- robust_se(model18full)

# Function to prepare data for plotting
prepare_plot_data <- function(model1, model2, model1_name, model2_name) {
  # Replace colons with asterisks in model names
  model1_name <- gsub(":", "*", model1_name)
  model2_name <- gsub(":", "*", model2_name)
  
  # Extract coefficients and confidence intervals
  coef1 <- coef(model1)
  conf1 <- confint(model1)
  coef2 <- coef(model2)
  conf2 <- confint(model2)
  
  # Create data frames for each model
  data1 <- data.frame(
    Variable = names(coef1),
    Estimate = coef1,
    CI_low = conf1[, 1],
    CI_high = conf1[, 2],
    Model = model1_name
  )
  
  data2 <- data.frame(
    Variable = names(coef2),
    Estimate = coef2,
    CI_low = conf2[, 1],
    CI_high = conf2[, 2],
    Model = model2_name
  )
  
  # Combine data and filter common variables
  combined_data <- rbind(data1, data2)
  common_vars <- intersect(data1$Variable, data2$Variable)
  combined_data <- subset(combined_data, Variable %in% common_vars)
  
  # Use dplyr to preprocess Variable names
  combined_data <- combined_data %>%
    mutate(
      Variable = gsub("_", " ", Variable) # Replace underscores with spaces
    )
  
  return(combined_data)
}

# Function to plot coefficients and confidence intervals
plot_coefficients <- function(data, model_name) {
  # Replace colons with asterisks in the model name
  model_name <- gsub(":", "*", model_name)
  
  # Filter out the intercept
  data <- subset(data, Variable != "(Intercept)")
  
  ggplot(data, aes(x = Variable, y = Estimate, color = Model)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high), 
                  width = 0.2, 
                  position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + # Dashed line at 0
    coord_flip() + # Flip for better readability
    labs(title = NULL,
         x = "Variable",
         y = "Estimate") +
    theme_minimal() +
    ylim(-4, 4) +   # Set y-axis limits
    theme(
      axis.text.x = element_text(color = "black"),  # Black color for x-axis variable names
      axis.text.y = element_text(color = "black"),  # Black color for y-axis variable names
      axis.title.x = element_text(color = "black"), # Black color for x-axis title
      axis.title.y = element_text(color = "black")  # Black color for y-axis title
    )
}

# List of models and their names
models <- list(
  list(robust_model4, robust_model4full, "No Controls", "Full Controls"),
  list(robust_model5, robust_model5full, "No Controls", "Full Controls"),
  list(robust_model6, robust_model6full, "No Controls", "Full Controls"),
  list(robust_model7, robust_model7full, "No Controls", "Full Controls"),
  list(robust_model8, robust_model8full, "No Controls", "Full Controls"),
  list(robust_model9, robust_model9full, "No Controls", "Full Controls"),
  list(robust_model10, robust_model10full, "No Controls", "Full Controls"),
  list(robust_model11, robust_model11full, "No Controls", "Full Controls"),
  list(robust_model12, robust_model12full, "No Controls", "Full Controls"),
  list(robust_model13, robust_model13full, "No Controls", "Full Controls"),
  list(robust_model14, robust_model14full, "No Controls", "Full Controls"),
  list(robust_model15, robust_model15full, "No Controls", "Full Controls"),
  list(robust_model16, robust_model16full, "No Controls", "Full Controls"),
  list(robust_model17, robust_model17full, "No Controls", "Full Controls"),
  list(robust_model18, robust_model18full, "No Controls", "Full Controls")
)

# Directory to save the plots
output_dir <- "C:/Users/BENDAJ/Regressions/Plots/climate" # Change to your desired directory

# Create the directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Generate and save plots for each pair of models
for (i in 1:length(models)) {
  # Prepare data
  data_to_plot <- prepare_plot_data(models[[i]][[1]], models[[i]][[2]], models[[i]][[3]], models[[i]][[4]])
  
  # Create the plot
  plot <- plot_coefficients(data_to_plot, paste(models[[i]][[3]], "vs", models[[i]][[4]]))
  
  # Display the plot in the viewer
  print(plot)
  
  # Save the plot with sequential file names (1.png, 2.png, ..., 18.png)
  plot_filename <- paste0(output_dir, "/", i, ".png")
  
  # Save the plot
  ggsave(filename = plot_filename, plot = plot, width = 8, height = 4)
}
  
#Combined plot for Traits
# Function to extract and combine data for specific variables across models
prepare_combined_plot_data <- function(models, variables) {
  combined_data <- data.frame()
  
  for (i in seq_along(models)) {
    model1 <- models[[i]][[1]]  # No Controls
    model2 <- models[[i]][[2]]  # Full Controls
    model1_name <- models[[i]][[3]]
    model2_name <- models[[i]][[4]]
    
    coef1 <- coef(model1)
    conf1 <- confint(model1)
    coef2 <- coef(model2)
    conf2 <- confint(model2)
    
    # Create data frames for model 1 and model 2
    data1 <- data.frame(
      Variable = names(coef1),
      Estimate = coef1,
      CI_low = conf1[, 1],
      CI_high = conf1[, 2],
      Model = model1_name
    )
    
    data2 <- data.frame(
      Variable = names(coef2),
      Estimate = coef2,
      CI_low = conf2[, 1],
      CI_high = conf2[, 2],
      Model = model2_name
    )
    
    # Combine and filter by specified variables and their interactions
    combined_data <- rbind(
      combined_data,
      subset(data1, Variable %in% variables | grepl("Pooled_Treatment", Variable)),
      subset(data2, Variable %in% variables | grepl("Pooled_Treatment", Variable))
    )
  }
  
  # Use dplyr to clean variable names
  combined_data <- combined_data %>%
    mutate(Variable = gsub("_", " ", Variable))  # Replace underscores with spaces
  
  return(combined_data)
}

# Specify the variables of interest
variables_of_interest <- c("vividness_pca", "moral_universalism_pca", 
                           "zero_sum_pca", "perspectives_pca")

# Specify the models related to these variables
models_of_interest <- list(
  list(robust_model5, robust_model5full, "Vividness (No Controls)", "Vividness (Full Controls)"),
  list(robust_model14, robust_model14full, "Moral Universalism (No Controls)", "Moral Universalism (Full Controls)"),
  list(robust_model13, robust_model13full, "Zero Sum (No Controls)", "Zero Sum (Full Controls)"),
  list(robust_model4, robust_model4full, "Perspectives (No Controls)", "Perspectives (Full Controls)")
)

# Prepare data for plotting
combined_plot_data <- prepare_combined_plot_data(models_of_interest, variables_of_interest)


# Adjust the order of the variables for plotting
combined_plot_data$Variable <- factor(combined_plot_data$Variable, levels = c(
  "perspectives pca",
  "zero sum pca",
  "moral universalism pca",
  "vividness pca",
  "Pooled Treatment:perspectives pca",
  "Pooled Treatment:zero sum pca",
  "Pooled Treatment:moral universalism pca",
  "Pooled Treatment:vividness pca",
  "Pooled Treatment"
))


# Plotting the coefficients and confidence intervals
traits_plot <- ggplot(combined_plot_data, aes(x = Variable, y = Estimate, color = Model)) +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  coord_flip() +
  labs(title = NULL,
       x = "Variable",
       y = "Estimate") +
  theme_minimal() +
  ylim(-4, 4) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )




# Plot for Experiences in climate
variables_of_interest <- c("parent", "Developing_lived", "statistics_exposure_pca")

# Update the models of interest to include only the models for the new variables
models_of_interest <- list(
  list(robust_model6, robust_model6full, "Parent (No Controls)", "Parent (Full Controls)"),
  list(robust_model7, robust_model7full, "Developing Lived (No Controls)", "Developing Lived (Full Controls)"),
  list(robust_model18, robust_model18full, "Statistics Exposure (No Controls)", "Statistics Exposure (Full Controls)")
)

# Prepare data for plotting using the function defined earlier
combined_plot_data <- prepare_combined_plot_data(models_of_interest, variables_of_interest)

# Adjust the order of the variables for plotting (only the new variables now)
combined_plot_data$Variable <- factor(combined_plot_data$Variable, levels = c(
  "parent",
  "Developing lived",
  "statistics exposure pca",
  "Pooled Treatment:parent",
  "Pooled Treatment:Developing lived",
  "Pooled Treatment:statistics exposure pca",
  "Pooled Treatment"
))

# Plotting the coefficients and confidence intervals
exp_plot <- ggplot(combined_plot_data, aes(x = Variable, y = Estimate, color = Model)) +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  coord_flip() +
  labs(title = NULL,
       x = "Variable",
       y = "Estimate") +
  theme_minimal() +
  ylim(-4, 4) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )

output_dir <- "C:/Users/BENDAJ/Regressions/Plots/Climate"

# Create the directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE) # Use 'recursive = TRUE' for nested directories
}

# Save the Traits Plot
traits_filename <- file.path(output_dir, "traits_climate.png")
ggsave(filename = traits_filename, plot = traits_plot, width = 8, height = 4)

# Save the Experiences in Learning Plot
exp_filename <- file.path(output_dir, "experiences_climate.png")
ggsave(filename = exp_filename, plot = exp_plot, width = 8, height = 4)



################################################################################################################################################
#Extra graphs for the video treatment
################################################################################################################################################
#learning
lvid <- lm(learning_pca ~ T3 + T3*vividness_pca + T3*perspectives_pca + T3*moral_universalism_pca + T3*zero_sum_pca + T3*parent + T3*Developing_lived + T3*statistics_exposure_pca, data = mydata)
robust_lvid <- robust_se(lvid)

lpool <- lm(learning_pca ~ Pooled_Treatment + Pooled_Treatment*vividness_pca + Pooled_Treatment*perspectives_pca + Pooled_Treatment*moral_universalism_pca + Pooled_Treatment*zero_sum_pca + Pooled_Treatment*parent + Pooled_Treatment*Developing_lived + Pooled_Treatment*statistics_exposure_pca, data = mydata)
robust_lpool <- robust_se(lpool)

#immigrants
ivid <- lm(immigrants_pca ~ T3 + T3*vividness_pca + T3*perspectives_pca + T3*moral_universalism_pca + T3*zero_sum_pca + T3*parent + T3*Developing_lived + T3*statistics_exposure_pca, data = mydata)
robust_ivid <- robust_se(ivid)

ipool <- lm(immigrants_pca ~ Pooled_Treatment + Pooled_Treatment*vividness_pca + Pooled_Treatment*perspectives_pca + Pooled_Treatment*moral_universalism_pca + Pooled_Treatment*zero_sum_pca + Pooled_Treatment*parent + Pooled_Treatment*Developing_lived + Pooled_Treatment*statistics_exposure_pca, data = mydata)
robust_ipool <- robust_se(ipool)

#refugees
rvid <- lm(refugees_pca ~ T3 + T3*vividness_pca + T3*perspectives_pca + T3*moral_universalism_pca + T3*zero_sum_pca + T3*parent + T3*Developing_lived + T3*statistics_exposure_pca, data = mydata)
robust_rvid <- robust_se(rvid)

rpool <- lm(refugees_pca ~ Pooled_Treatment + Pooled_Treatment*vividness_pca + Pooled_Treatment*perspectives_pca + Pooled_Treatment*moral_universalism_pca + Pooled_Treatment*zero_sum_pca + Pooled_Treatment*parent + Pooled_Treatment*Developing_lived + Pooled_Treatment*statistics_exposure_pca, data = mydata)
robust_rpool <- robust_se(rpool)

#climate change
cvid <- lm(climate_change_pca ~ T3 + T3*vividness_pca + T3*perspectives_pca + T3*moral_universalism_pca + T3*zero_sum_pca + T3*parent + T3*Developing_lived + T3*statistics_exposure_pca, data = mydata)
robust_cvid <- robust_se(cvid)

cpool <- lm(climate_change_pca ~ Pooled_Treatment + Pooled_Treatment*vividness_pca + Pooled_Treatment*perspectives_pca + Pooled_Treatment*moral_universalism_pca + Pooled_Treatment*zero_sum_pca + Pooled_Treatment*parent + Pooled_Treatment*Developing_lived + Pooled_Treatment*statistics_exposure_pca, data = mydata)
robust_cpool <- robust_se(cpool)

#Plots
# Function to create a tidy data frame with coefficients and confidence intervals
get_coef_and_ci <- function(model, model_name) {
  # Extract coefficients and confidence intervals
  coefs <- summary(model)$coefficients
  ci <- confint(model)
  
  # Create a tidy data frame
  coef_df <- data.frame(
    variable = rownames(coefs)[-1],  # Exclude the intercept
    Estimate = coefs[-1, 1],  # Exclude the intercept
    CI_low = ci[-1, 1], 
    CI_high = ci[-1, 2],
    Model = model_name
  )
  
  # Replace underscores with spaces and colons with asterisks
  coef_df$variable <- gsub("_", " ", coef_df$variable)  # Replace underscores with spaces
  coef_df$variable <- gsub(":", "*", coef_df$variable)  # Replace colons with asterisks
  
  return(coef_df)
}

# Extract coefficients and confidence intervals for all models
coef_df_lvid <- get_coef_and_ci(lvid, "Learning T3")
coef_df_lpool <- get_coef_and_ci(lpool, "Learning")
coef_df_ivid <- get_coef_and_ci(ivid, "Immigrants T3")
coef_df_ipool <- get_coef_and_ci(ipool, "Immigrants")
coef_df_rvid <- get_coef_and_ci(rvid, "Refugees T3")
coef_df_rpool <- get_coef_and_ci(rpool, "Refugees")
coef_df_cvid <- get_coef_and_ci(cvid, "Climate Change T3")
coef_df_cpool <- get_coef_and_ci(cpool, "Climate Change")

# Combine all data into a single data frame
coef_df <- bind_rows(coef_df_lvid, coef_df_lpool, coef_df_ivid, coef_df_ipool, coef_df_rvid, coef_df_rpool, coef_df_cvid, coef_df_cpool)

# Create one plot for each model and save it
model_names <- unique(coef_df$Model)

# Loop through each model and create a separate plot
for (model_name in model_names) {
  # Filter data for the current model
  plot_data <- subset(coef_df, Model == model_name)
  
  # Create the plot for the current model
  p <- ggplot(plot_data, aes(x = variable, y = Estimate, color = Model)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) + 
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high), 
                  width = 0.2, 
                  position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + # Dashed line at 0
    coord_flip() + # Flip for better readability
    labs(title = NULL,
         x = "Variable",
         y = "Estimate") +
    theme_minimal() +
    ylim(-4, 4) +   # Set y-axis limits
    theme(
      axis.text.x = element_text(color = "black"),  # Black color for x-axis variable names
      axis.text.y = element_text(color = "black"),  # Black color for y-axis variable names
      axis.title.x = element_text(color = "black"), # Black color for x-axis title
      axis.title.y = element_text(color = "black")  # Black color for y-axis title
    )
  
  # Save the plot to a file
  ggsave(paste0("plot_", gsub("[^a-zA-Z0-9]", "_", model_name), ".png"), p, width = 8, height = 6, dpi = 300)
}

