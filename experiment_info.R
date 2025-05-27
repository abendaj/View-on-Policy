# Clear environment
rm(list=ls())

# Load necessary libraries
library(readxl)
library(dplyr)
library(writexl)
library(stats)
library(haven)
library(gtsummary)

# Load mydata
mydata <- read_excel("add_to_path/data.xlsx")

# Count the number of observations in Treatment
control_count <- sum(mydata$Treatment == "Control")
vidnpap_count <- sum(mydata$Treatment == "Video and Short paper")
vid_count <- sum(mydata$Treatment == "Video")
papnvid_count <- sum(mydata$Treatment == "Short paper and Video")
pap_count <- sum(mydata$Treatment == "Short paper")

# Print the result
print(control_count)
print(vidnpap_count)
print(vid_count)
print(papnvid_count)
print(pap_count)

#Did not Pick Turquoise
turquoise_count <- sum(mydata$Q244 != "Turquoise")
print(turquoise_count)

#Spent less than 10 minutes
lessthan10_count <- sum(mydata$Duration..in.seconds. < 600)
print(lessthan10_count)

# Generate the Age variable based on Birth_Year
mydata$Age <- 2024 - mydata$Birth_Year_cleaned

# Convert string variables to factors
mydata$Gender_num <- as.numeric(as.factor(mydata$Gender))
mydata$Ethnic_group_num <- as.numeric(as.factor(mydata$Ethnic_group))
mydata$Education_num <- as.numeric(as.factor(mydata$Education))
mydata$Employment_status_num <- as.numeric(as.factor(mydata$Employment_status))
mydata$Income_group_num <- as.numeric(as.factor(mydata$Income_group))

# Create balance table using gtsummary
mydata %>%
  select(Gender, Age, Ethnic_group, Education, Employment_status, Income_group, Treatment) %>%
  tbl_summary(by = Treatment, statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_p(test = list(all_categorical() ~ "fisher.test"), test.args = all_categorical() ~ list(simulate.p.value = TRUE)) 



# Define the columns/questions you want to analyze
argument <- c("Q49", "Q50", "Q51", "Q243")
neutral <- c("Q52", "Q53", "Q54", "Q55", "Q56", "Q57", "Q61", "Q244", "Q197", "Q115", "Q116")
describes <- c("Q82", "Q84", "Q85", "Q86", "Q88", "Q91", "Q92", "Q89", "Q93")

# Function to check consistency without a middle option
check_consistency_no_middle <- function(df, questions, first_option_dict, last_option_dict) {
  df$consistent_first <- apply(df[, questions], 1, function(row) all(row == unlist(first_option_dict[questions])))
  df$consistent_last <- apply(df[, questions], 1, function(row) all(row == unlist(last_option_dict[questions])))
  
  first_consistent_count <- sum(df$consistent_first, na.rm = TRUE)
  last_consistent_count <- sum(df$consistent_last, na.rm = TRUE)
  
  return(list(first_consistent_count, last_consistent_count))
}

# Define first and last options for argument group
first_option_argument <- list(
  "Q49" = "Strongly agree with 1",
  "Q50" = "Strongly agree with 1",
  "Q51" = "Strongly agree with 1",
  "Q243" = "Strongly Agree with 1"
)

last_option_argument <- list(
  "Q49" = "Strongly agree with 2",
  "Q50" = "Strongly agree with 2",
  "Q51" = "Strongly agree with 2",
  "Q243" = "Strongly Agree with 2"
)

# Check consistency for argument group
argument_results <- check_consistency_no_middle(mydata, argument, first_option_argument, last_option_argument)
cat("Argument Group - First Option:", argument_results[[1]], "participants consistently chose the first option.\n")
cat("Argument Group - Last Option:", argument_results[[2]], "participants consistently chose the last option.\n")

# Function to check consistency with a middle option
check_consistency <- function(df, questions, first_option, neutral_option, last_option) {
  df$consistent_first <- apply(df[, questions], 1, function(row) all(row == first_option))
  df$consistent_middle <- apply(df[, questions], 1, function(row) all(row == neutral_option))
  df$consistent_last <- apply(df[, questions], 1, function(row) all(row == last_option))
  
  first_consistent_count <- sum(df$consistent_first, na.rm = TRUE)
  middle_consistent_count <- sum(df$consistent_middle, na.rm = TRUE)
  last_consistent_count <- sum(df$consistent_last, na.rm = TRUE)
  
  return(list(first_consistent_count, middle_consistent_count, last_consistent_count))
}

# Check consistency for neutral group
neutral_results <- check_consistency(mydata, neutral, "Strongly Agree", "Neutral", "Strongly Disagree")
cat("Neutral Group - First Option:", neutral_results[[1]], "participants consistently chose the first option.\n")
cat("Neutral Group - Middle Option:", neutral_results[[2]], "participants consistently chose the middle option.\n")
cat("Neutral Group - Last Option:", neutral_results[[3]], "participants consistently chose the last option.\n")

# Check consistency for describes group
describes_results <- check_consistency(mydata, describes, "Yes", "Partially", "No")
cat("Describes Group - First Option:", describes_results[[1]], "participants consistently chose the first option.\n")
cat("Describes Group - Middle Option:", describes_results[[2]], "participants consistently chose the middle option.\n")
cat("Describes Group - Last Option:", describes_results[[3]], "participants consistently chose the last option.\n")

