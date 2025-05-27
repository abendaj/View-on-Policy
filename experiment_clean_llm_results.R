# --- CLEAR ENVIRONMENT --- #
rm(list = ls())

library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library(knitr)
library(tidyr)
library(kableExtra)

# Read the Excel file
df <- read_excel("add_to_path/analyzed_results.xlsx")

# --- Define possible values per category ---
balance_values <- c("Both", "Only refugees", "Only hosts", "Not specified")
empathy_values <- c("Empathetic toward refugees", "Empathetic toward hosts", "Objective", "Neutral", "Unclear")
favorability_values <- c("Favorable toward refugees", "Favorable toward hosts", "Neutral", "Unclear")
shocks_values <- c("Cash transfers", "Hurricane", "Both", "Neither", "Unclear")

# --- Helper function to find first match ---
extract_first_match <- function(text, options) {
  found <- options[str_detect(text, fixed(options))]
  if (length(found) > 0) return(found[1]) else return(NA)
}

# --- Apply extraction to all three columns ---
df <- df %>%
  rowwise() %>%
  mutate(
    # From analysis_combined
    Balance_combined = extract_first_match(analysis_combined, balance_values),
    Empathy_combined = extract_first_match(analysis_combined, empathy_values),
    Favorability_combined = extract_first_match(analysis_combined, favorability_values),
    Shocks_combined = extract_first_match(analysis_combined, shocks_values),
    
    # From analysis_paper
    Balance_paper = extract_first_match(analysis_paper, balance_values),
    Empathy_paper = extract_first_match(analysis_paper, empathy_values),
    Favorability_paper = extract_first_match(analysis_paper, favorability_values),
    Shocks_paper = extract_first_match(analysis_paper, shocks_values),
    
    # From analysis_video
    Balance_video = extract_first_match(analysis_video, balance_values),
    Empathy_video = extract_first_match(analysis_video, empathy_values),
    Favorability_video = extract_first_match(analysis_video, favorability_values),
    Shocks_video = extract_first_match(analysis_video, shocks_values)
  ) %>%
  ungroup()

# --- APPLY SCORING MAPPINGS ---
df <- df %>%
  mutate(
    # Balance scores
    Balance_combined_score = case_when(
      Balance_combined == "Only refugees" ~ 0,
      Balance_combined == "Both" ~ 1,
      TRUE ~ NA_real_
    ),
    Balance_paper_score = case_when(
      Balance_paper == "Only refugees" ~ 0,
      Balance_paper == "Both" ~ 1,
      TRUE ~ NA_real_
    ),
    Balance_video_score = case_when(
      Balance_video == "Only refugees" ~ 0,
      Balance_video == "Both" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Empathy scores (1 for Objective/Neutral, 0 for refugees, NA for hosts/unclear)
    Empathy_combined_score = case_when(
      Empathy_combined == "Empathetic toward refugees" ~ 0,
      Empathy_combined %in% c("Objective", "Neutral") ~ 1,
      TRUE ~ NA_real_  # This catches "Empathetic toward hosts", "Unclear", etc.
     ),
    Empathy_paper_score = case_when(
      Empathy_paper == "Empathetic toward refugees" ~ 0,
      Empathy_paper %in% c("Objective", "Neutral") ~ 1,
      TRUE ~ NA_real_
     ),
    Empathy_video_score = case_when(
      Empathy_video == "Empathetic toward refugees" ~ 0,
      Empathy_video %in% c("Objective", "Neutral") ~ 1,
      TRUE ~ NA_real_
    ),

    
    # Favorability scores
    Favorability_combined_score = case_when(
      Favorability_combined == "Neutral" ~ 1,
      Favorability_combined == "Favorable toward refugees" ~ 0,
      TRUE ~ NA_real_
    ),
    Favorability_paper_score = case_when(
      Favorability_paper == "Neutral" ~ 1,
      Favorability_paper == "Favorable toward refugees" ~ 0,
      TRUE ~ NA_real_
    ),
    Favorability_video_score = case_when(
      Favorability_video == "Neutral" ~ 1,
      Favorability_video == "Favorable toward refugees" ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Shocks scores
    Shocks_combined_score = case_when(
      Shocks_combined == "Neither" ~ 0,
      Shocks_combined %in% c("Cash transfers", "Hurricane") ~ 1,
      Shocks_combined == "Both" ~ 2,
      TRUE ~ NA_real_
    ),
    Shocks_paper_score = case_when(
      Shocks_paper == "Neither" ~ 0,
      Shocks_paper %in% c("Cash transfers", "Hurricane") ~ 1,
      Shocks_paper == "Both" ~ 2,
      TRUE ~ NA_real_
    ),
    Shocks_video_score = case_when(
      Shocks_video == "Neither" ~ 0,
      Shocks_video %in% c("Cash transfers", "Hurricane") ~ 1,
      Shocks_video == "Both" ~ 2,
      TRUE ~ NA_real_
    )
  )

# --- Write to Excel ---
write_xlsx(df, "add_to_path/data.xlsx")

# Filter only those in the specified treatment groups
df_filtered <- df %>%
  filter(Treatment %in% c("Video and Short paper", "Short paper and Video"))

# Create new logical columns to indicate where the paper and video responses differ
df_filtered <- df_filtered %>%
  mutate(
    Balance_diff = Balance_paper != Balance_video,
    Empathy_diff = Empathy_paper != Empathy_video,
    Favorability_diff = Favorability_paper != Favorability_video,
    Shocks_diff = Shocks_paper != Shocks_video
  )

# Count number of differences per category
summary_diff <- df_filtered %>%
  summarise(
    Balance_different = sum(Balance_diff, na.rm = TRUE),
    Empathy_different = sum(Empathy_diff, na.rm = TRUE),
    Favorability_different = sum(Favorability_diff, na.rm = TRUE),
    Shocks_different = sum(Shocks_diff, na.rm = TRUE)
  )

# Print summary
print(summary_diff)



library(dplyr)
library(tidyr)

# --- Create a balance table based on Treatment with Treatments as columns ---
balance_table <- df %>%
  pivot_longer(cols = c(Balance_combined, Empathy_combined, Favorability_combined, Shocks_combined), 
               names_to = "Variable", 
               values_to = "Answer") %>%
  count(Variable, Answer, Treatment) %>%
  pivot_wider(names_from = Treatment, values_from = n, values_fill = list(n = 0))

# Print balance table
print(balance_table)
