# Clear environment
rm(list=ls())

# Load necessary libraries
library(readxl)
library(dplyr)
library(writexl)
library(stats)

# Load data
mydata <- read_excel("add_to_path/Mapped_Questions.xlsx")

# Define your indices
index_list <- list(
  statistics_exposure_index = c('Q29_mapped', 'Q30_mapped', 'Q31_mapped'),
  vividness_index = c('Q101_1_mapped', 'Q101_2_mapped', 'Q101_3_mapped', 'Q101_4_mapped', 'Q102_mapped', 
                      'Q103_mapped', 'Q105_mapped', 'Q106_mapped', 'Q107_1_mapped', 'Q107_2_mapped'),  #'Q104_mapped' has a negative loading
  combined_index = c('Q40_mapped', 'Q41_mapped', 'Q39_mapped', 'Q42_mapped', 'Q43_mapped', 'Q70_mapped'),
  moral_universalism_index = c('Q110_1_mapped', 'Q111_1_mapped', 'Q113_1_mapped'),
  zero_sum_index = c('Q49_mapped', 'Q50_mapped', 'Q51_mapped', 'Q243_mapped'),
  civic_norms_index = c('Q53_mapped', 'Q54_mapped'),
  freedom_index = c('Q60_mapped', 'Q57_mapped', 'Q52_mapped'),
  immigrants_index = c('Q55_mapped', 'Q56_mapped'),
  refugees_index = c('Q61_mapped', 'Q71_mapped', 'Q197_mapped', 'Q115_mapped', 'Q116_mapped', 'Q77_mapped', 
                     'Q119_mapped', 'refugee_donation_mapped', paste0('Q80_', 1:11, '_mapped')),
  perspectives_index = c('Q82_mapped', 'Q88_mapped', 'Q84_mapped', 'Q85_mapped', 'Q91_mapped', 'Q92_mapped', 
                         'Q89_mapped', 'Q93_mapped'),
  shoes_index = c('Q82_mapped', 'Q88_mapped', 'Q84_mapped', 'Q85_mapped', 'Q91_mapped', 'Q92_mapped'),
  retention_index = c('Q121_mapped', 'Q122_mapped', 'Q123_mapped', 'Q124_mapped', 'Q127_mapped', 'Q126_mapped', 
                      'Q214_mapped'),
  climate_change_index = c('Q62_mapped', 'Q58_mapped', 'Q64_mapped', 'climate_donation_mapped', 
                           paste0('Q66_', 1:5, '_mapped'), paste0('Q67_', 1:5, '_mapped')),
  learning_index = c('Q39_mapped', 'text_analysis'),
  self_vs_others_index = paste0('Q138_', 1:6, '_mapped'),
  refugees_world_index = c('Q77_mapped', 'refugee_donation_mapped', paste0('Q80_', 1:11, '_mapped')),
  refugees_uk_index = c('Q61_mapped', 'Q71_mapped', 'Q197_mapped', 'Q115_mapped', 'Q116_mapped', 
                     'Q119_mapped')
)

# Initialize a list to store PCA scores
pca_scores_list <- list()

# Initialize lists to store factor loadings and eigenvalues
factor_loadings_list <- list()
eigenvalues_list <- list()

# Perform PCA for each index
for (index_name in names(index_list)) {
  index_columns <- index_list[[index_name]]
  
  # Filter data for the current index
  index_data <- mydata %>%
    select(all_of(index_columns))
  
  # Remove rows with NA or infinite values
  index_data <- index_data %>% filter_all(all_vars(!is.na(.) & is.finite(.)))
  
  # Check if there are enough columns for PCA
  if (ncol(index_data) > 1 && nrow(index_data) > 1) {
    # Standardize the data
    index_data_scaled <- scale(index_data, center = TRUE, scale = TRUE)
    
    # Perform PCA
    pca_result <- prcomp(index_data_scaled, center = TRUE, scale. = TRUE)
    
    # Flip signs for all PCs where the sum of loadings is negative
    for (pc_index in 1:ncol(pca_result$rotation)) {
      if (sum(pca_result$rotation[, pc_index]) < 0) {
        pca_result$rotation[, pc_index] <- -pca_result$rotation[, pc_index]
        pca_result$x[, pc_index] <- -pca_result$x[, pc_index]
      }
    }
    
    # Extract PCA scores (First Principal Component)
    pca_scores <- as.data.frame(pca_result$x[, 1]) # First component scores
    colnames(pca_scores) <- paste0(index_name, "_PC1")
    
    # Align PCA scores with original data by adding NA for removed rows
    aligned_scores <- rep(NA, nrow(mydata))
    complete_rows <- which(complete.cases(mydata[, index_columns]))
    
    if (length(complete_rows) == nrow(pca_scores)) {
      aligned_scores[complete_rows] <- pca_scores[[1]]
    } else {
      warning("Mismatch between PCA scores and complete cases for index: ", index_name)
    }
    
    # Store the aligned scores
    pca_scores_list[[index_name]] <- aligned_scores
    
    # Store factor loadings and eigenvalues for the first two PCs
    factor_loadings_list[[index_name]] <- pca_result$rotation[, 1:2]
    eigenvalues_list[[index_name]] <- pca_result$sdev[1:2]^2
  } else {
    # If not enough data, fill with NA
    pca_scores_list[[index_name]] <- rep(NA, nrow(mydata))
    factor_loadings_list[[index_name]] <- NA
    eigenvalues_list[[index_name]] <- NA
  }
}

# Print factor loadings and eigenvalues for each index
for (index_name in names(index_list)) {
  cat("Index:", index_name, "\n")
  
  cat("Factor Loadings (First 2 PCs):\n")
  print(factor_loadings_list[[index_name]])
  
  cat("Eigenvalues (First 2 PCs):\n")
  print(eigenvalues_list[[index_name]])
  
  cat("\n-----------------------------------------\n")
}

# Combine PCA scores into a single dataframe
pca_scores_df <- as.data.frame(pca_scores_list)

# Replace the last bit of column names from 'index' to 'pca'
colnames(pca_scores_df) <- gsub("_index", "_pca", colnames(pca_scores_df))

# Concatenate original data with PCA scores
combined_data <- cbind(mydata, pca_scores_df)

# Save the combined data to an Excel file
write_xlsx(combined_data, "add_to_path/data.xlsx")

