# Clear environment
rm(list=ls())

library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(readxl)
library(writexl)
library(lubridate)

mydata <- read.csv("add_to_path/qualtricsexperiment.csv")
mydata_raw <- mydata
head(mydata)
summary(mydata)

# Delete the first two rows using slice
mydata <- mydata %>% slice(-c(1, 2))

# Rename the specified columns
mydata <- mydata %>%
  rename(
    Gender = Q1,
    Birth_Year = Q2,
    Birth_Country = Q3,
    Education = Q9,
    Ethnic_group = Q6,
    Income_group = Q26,
    Employment_status = Q10
  )

# Function to clean unwanted characters from a column
clean_text <- function(x) {
  gsub("â€™|Â|â|£", "", x)  # Remove unwanted characters (â € ™ Â and £)
}

# Clean all columns except Income_group
mydata <- mydata %>%
  mutate(across(-Income_group, ~ clean_text(.)))

# Define the current year
current_year <- 2024

mydata <- mydata %>%
  mutate(
    Birth_Year_cleaned = gsub("[^0-9A-Za-z/-]", "", Birth_Year),  # Remove unwanted characters
    
    Birth_Year_cleaned = case_when(
      # Handle specific formats
      grepl("^\\d{1,2}/\\d{1,2}/\\d{2,4}$", Birth_Year_cleaned) ~ suppressWarnings(year(dmy(Birth_Year_cleaned))),  # e.g., DD/MM/YYYY
      grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{2}$", Birth_Year_cleaned) ~ suppressWarnings(year(dmy(Birth_Year_cleaned))),  # e.g., DD-MMM-YY
      grepl("^\\d{1,2}\\.\\d{1,2}\\.\\d{4}$", Birth_Year_cleaned) ~ suppressWarnings(year(as.Date(Birth_Year_cleaned, format = "%d.%m.%Y"))),  # e.g., DD.MM.YYYY
      
      # Extract numeric year from text like "1999(Identity Code: 85720)"
      grepl("^\\d{4}\\(.*\\)$", Birth_Year) ~ as.numeric(sub("\\(.*\\)$", "", Birth_Year_cleaned)),
      
      # Handle 4-digit years directly
      nchar(Birth_Year_cleaned) == 4 & grepl("^\\d+$", Birth_Year_cleaned) & as.numeric(Birth_Year_cleaned) <= current_year ~ as.numeric(Birth_Year_cleaned),
      
      # Handle 2-digit years or ages
      nchar(Birth_Year_cleaned) == 2 ~ ifelse(as.numeric(Birth_Year_cleaned) > 60, 
                                              1900 + as.numeric(Birth_Year_cleaned), 
                                              current_year - as.numeric(Birth_Year_cleaned)),
      
      # Invalid cases
      TRUE ~ NA_real_
    )
  )

# Manually fix the problematic cases:
mydata$Birth_Year_cleaned[mydata$Birth_Year == "01-Jan-75"] <- 1975
mydata$Birth_Year_cleaned[mydata$Birth_Year == "01.09.2001"] <- 2001
mydata$Birth_Year_cleaned[mydata$Birth_Year == "1999(Identity Code: 85720)"] <- 1999
mydata$Birth_Year_cleaned[mydata$Birth_Year == "08-Jan-93"] <- 1993
mydata$Birth_Year_cleaned[mydata$Birth_Year == "99/09/2000"] <- 2000
mydata$Birth_Year_cleaned[mydata$Birth_Year == "20000"] <- 2000
mydata$Birth_Year_cleaned[mydata$Birth_Year == "2 July 1082"] <- 1982
mydata$Birth_Year_cleaned[mydata$Birth_Year == "１９９６"] <- 1996
mydata$Birth_Year_cleaned[mydata$Birth_Year == "11.2.2000"] <- 2000
mydata$Birth_Year_cleaned[mydata$Birth_Year == "April 02 1990"] <- 1990
mydata$Birth_Year_cleaned[mydata$Birth_Year == "19999"] <- 1999
mydata$Birth_Year_cleaned[mydata$Birth_Year == "Aug 1, 2001"] <- 2001

unique(mydata$Birth_Year_cleaned)

mydata %>%
  filter(is.na(Birth_Year_cleaned)) %>%
  select(Birth_Year)




#Change Duration to minutes
mydata$Duration..in.seconds. <- as.numeric(as.character(mydata$Duration..in.seconds.))
mydata$Minutes <- mydata$Duration / 60

#Clean dataset
mydata_unfinished <- mydata %>% filter(Finished == FALSE)
mydata <- mydata %>% filter(Finished == TRUE)
mydata_turquoise <- mydata %>% filter(Q244 != "Turquoise") 
head(mydata_turquoise$Q244)
mydata_time <- mydata %>% filter(Minutes < 10) 
head(mydata_time$Minutes)



# Create the Treatment column based on conditions
mydata <- mydata %>%
  mutate(Treatment = case_when(
    !is.na(Q236) & Q236 != "" ~ "Short paper and Video",   # If Q236 is non-empty
    !is.na(Q233) & Q233 != "" ~ "Short paper",              # If Q233 is non-empty
    !is.na(Q238) & Q238 != "" ~ "Video and Short paper",    # If Q238 is non-empty
    !is.na(Q232) & Q232 != "" ~ "Video",                    # If Q232 is non-empty
    TRUE ~ "Control"                                         # If all Q236, Q233, Q238, and Q232 are empty
  ))

# Create the Pooled_Treatment column based on the Treatment column
mydata <- mydata %>%
  mutate(Pooled_Treatment = case_when(
    Treatment %in% c("Short paper", "Short paper and Video", "Video and Short paper", "Video") ~ 1,   # Set to 1 for these treatments
    Treatment == "Control" ~ 0,                                                    # Set to 0 for Control
  ))

# Create the Paper column based on the Treatment column
mydata <- mydata %>%
  mutate(Paper = case_when(
    Treatment %in% c("Short paper", "Short paper and Video", "Video and Short paper") ~ 1,   # Set to 1 for these treatments
    Treatment %in% c("Control", "Video") ~ 0,                                                    # Set to 0 for Control and Video
  ))

# Create the Video column based on the Treatment column
mydata <- mydata %>%
  mutate(Video = case_when(
    Treatment %in% c("Video", "Short paper and Video", "Video and Short paper") ~ 1,   # Set to 1 for these treatments
    Treatment %in% c("Control", "Short paper") ~ 0,                                                    # Set to 0 for Control and Short paper
  ))

# View the updated dataset
head(mydata$Pooled_Treatment)

mydata <- mydata %>%
  mutate(parent = case_when(
    Q190 == "Yes" | Q192 == "Yes" ~ 1,  # If either Q190 or Q192 is "Yes"
    TRUE ~ 0  # Otherwise, assign 0
  ))

# Convert EndDate to Date-Time format using the format 'day/month/year hour:minute'
mydata$EndDate_temp <- dmy_hm(mydata$EndDate)  # dmy_hm handles DD/MM/YYYY HH:MM format

# Create the Weekend column (1 for weekends, 0 for weekdays)
mydata <- mydata %>%
  mutate(Weekend = case_when(
    wday(EndDate_temp) %in% c(1, 7) ~ 1,  # 1 = Sunday, 7 = Saturday
    TRUE ~ 0  # Weekdays (Monday to Friday)
  )) %>%
select(-EndDate_temp)  # Delete the EndDate_temp column


# Convert Q94_1, Q94_2, Q94_3 to numeric, replacing non-numeric values with NA
mydata <- mydata %>%
  mutate(across(c(Q94_1, Q94_2, Q94_3), ~ as.numeric(as.character(.))))

# Replace NA or empty values with 0 in Q94_1, Q94_2, and Q94_3
mydata <- mydata %>%
  mutate(across(c(Q94_1, Q94_2, Q94_3), ~ ifelse(is.na(.) | . == "", 0, .)))

# Calculate the new columns
mydata$donation <- mydata$Q94_1 + mydata$Q94_2 + mydata$Q94_3
mydata$refugee_donation <- mydata$Q94_1 + mydata$Q94_2
mydata$climate_donation <- mydata$Q94_3
head(mydata$climate_donation)

# Create the Religiosity column based on the values of Q38
mydata <- mydata %>%
  mutate(Religiosity = case_when(
    Q38 == "Strongly Disagree" ~ 0,    # Strongly Disagree -> 0
    Q38 == "Disagree" ~ 0.25,          # Disagree -> 0.25
    Q38 == "Neutral" ~ 0.5,            # Neutral -> 0.5
    Q38 == "Agree" ~ 0.75,             # Agree -> 0.75
    Q38 == "Strongly Agree" ~ 1,       # Strongly Agree -> 1
    is.na(Q38) ~ NA_real_,             # NA -> NA
    TRUE ~ NA_real_                    # For any unexpected values, set to NA
  ))

# Create the Liberal column based on the values of Q27
mydata <- mydata %>%
  mutate(Liberal = case_when(
    Q27 == "Very conservative" ~ 0,    # Very conservative -> 0
    Q27 == "Conservative" ~ 0.25,      # Conservative -> 0.25
    Q27 == "Moderate" ~ 0.5,           # Moderate -> 0.5
    Q27 == "Liberal" ~ 0.75,           # Liberal -> 0.75
    Q27 == "Very liberal" ~ 1,         # Very liberal -> 1
    is.na(Q27) ~ NA_real_,             # NA -> NA
    TRUE ~ NA_real_                    # For any unexpected values, set to NA
  ))

# Create the Developing_lived column based on the values of Q13
mydata <- mydata %>%
  mutate(Developing_lived = case_when(
    Q13 == "Yes" ~ 1,   # If Q13 is Yes, set Developing_lived to 1
    Q13 == "No" ~ 0,    # If Q13 is No, set Developing_lived to 0
    is.na(Q13) ~ NA_real_,  # If Q13 is NA, set Developing_lived to NA
    TRUE ~ NA_real_       # For any unexpected values, set to NA
  ))

mydata <- mydata %>%
  mutate(across(
    c(Q46_1, Q46_2, Q46_3, Q46_4, Q46_5),
    ~ case_when(
      . == "None" ~ 0.33,
      . == "Some" ~ 0.66,
      . == "Many" ~ 1,
      . == "" ~ 0, 
      TRUE ~ NA_real_     # Handle unexpected values
    )
  )) %>%
  rowwise() %>%
  mutate(Immigrants_share = mean(c_across(c(Q46_1, Q46_2, Q46_3, Q46_4, Q46_5)), na.rm = TRUE)) %>%
  ungroup()


# Filter out the rows in mydata_time and mydata_turquoise from mydata
mydata_clean <- mydata %>%
  filter(!(id %in% mydata_time$id)) %>%
  filter(!(id %in% mydata_turquoise$id))

# Create a list of data frames with sheet names, including the Clean sheet
data_to_export <- list(
  Raw = mydata_raw,
  Mixed = mydata,
  Time = mydata_time,
  Turquoise = mydata_turquoise,
  Unfinished = mydata_unfinished,
  Clean = mydata_clean  # Add the Clean dataframe
)

# Write to an Excel file with the new Clean sheet
write_xlsx(data_to_export, "add_to_path/Until 25-03-2025.xlsx")



