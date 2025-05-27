# Clear environment
rm(list=ls())

library(readxl)
library(dplyr)
library(writexl)

#Load data
mydata <- read_excel("add_to_path/Until 25-03-2025.xlsx", sheet = "Mixed")

#Mapping for the questions
map_q29 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  return(ifelse(response == "Yes", 1, 0))
}

map_q30 <- function(response) {
  if (is.na(response)) {
    return(0)
  }
  return(ifelse(response == "They eliminate confounding bias through randomization", 1, 0))
}

map_q31 <- function(response) {
  if (is.na(response)) {
    return(0)
  }
  correct_answers <- c(
    "A good instrument has to be relevant: i.e. the instrument has to be correlated with the endogenous regressor",
    "A good instrument only affects the outcome variable through its impact on the endogenous regressor",
    "A good instrument is uncorrelated with the error term",
    "A good instrument is one that yields an intention to treat estimate"
  )
  return(ifelse(response %in% correct_answers, 1, 0))
}


map_q101 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  # Try converting to numeric and divide by 10
  result <- tryCatch(as.numeric(response), error = function(e) NA)
  if (is.na(result)) {
    return(NA)
  }
  return(result / 10)
}

map_q102 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  if (response == "Diagrams, charts or visual representations") {
    return(1)
  } else if (response == "Instructional Video") {
    return(0.5)
  } else {
    return(0)
  }
}

map_q103 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  return(ifelse(response == "Picture specific images or scenes that represent freedom", 1, 0))
}

map_q104 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  return(ifelse(response == "Something cold like ice, an iceberg or an ice cream", 1, 0))
}

map_q105 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  return(ifelse(response == "the activities you will engage in", 1, 0))
}

map_q106 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  return(ifelse(response == "Images of the location or people", 1, 0))
}

map_q107 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  if (response == "Strongly Agree") {
    return(1)
  } else if (response == "Agree") {
    return(0.75)
  } else if (response == "Neither Agree nor Disagree") {
    return(0.5)
  } else if (response == "DIsagree") {
    return(0.25)
  } else if (response == "Strongly Disagree") {
    return(0)
  } else if (response == "Strongly disagree") {
    return(0)    
  } else {
    return(NA)
  }
}


map_q40 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  return(ifelse(response == "Randomized controlled trial (RCT)", 1, 0))
}

map_q41 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  return(ifelse(response == "Differences in differences", 1, 0))
}

map100 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  result <- tryCatch(as.numeric(response), error = function(e) NA)
  if (is.na(result)) {
    return(NA)
  }
  return(result / 100)
}


map100_reverse <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  result <- tryCatch(as.numeric(response), error = function(e) NA)
  if (is.na(result)) {
    return(NA)
  }
  return((100 - result) / 100)
}

map2 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  if (response == "Strongly Agree with 1") {
    return(0)
  } else if (response == "Strongly agree with 1") {
    return(0)
  } else if (response == "Agree with 1") {
    return(0.33)
  } else if (response == "Agree with 2") {
    return(0.66)
  } else if (response == "Strongly Agree with 2") {
    return(1)
  } else if (response == "Strongly agree with 2") {
    return(1)
  } else {
    return(NA)
  }
}

map1 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  if (response == "Strongly Agree with 2") {
    return(0)
  } else if (response == "Strongly agree with 2") {
    return(0)
  } else if (response == "Agree with 2") {
    return(0.33)
  } else if (response == "Agree with 1") {
    return(0.66)
  } else if (response == "Strongly Agree with 1") {
    return(1)
  } else if (response == "Strongly agree with 1") {
    return(1)
  } else {
    return(NA)
  }
}

mapagree <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  if (response == "Strongly Disagree") {
    return(0)
  } else if (response == "Disagree") {
    return(0.25)
  } else if (response == "Neutral") {
    return(0.5)
  } else if (response == "Agree") {
    return(0.75)
  } else if (response == "Strongly Agree") {
    return(1)
  } else {
    return(NA)
  }
}

mapdisagree <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  if (response == "Strongly Agree") {
    return(0)
  } else if (response == "Agree") {
    return(0.25)
  } else if (response == "Neutral") {
    return(0.5)
  } else if (response == "Disagree") {
    return(0.75)
  } else if (response == "Strongly Disagree") {
    return(1)
  } else {
    return(NA)
  }
}

map_q71 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  mapping <- c(
    "All unauthorized immigrants should be deported" = 0,
    "No unauthorized immigrant should be given a pathway to earn U.K citizenship." = 0.25,
    "All unauthorized immigrants brought here as children should be given a pathway to earn U.K. citizenship." = 0.5,
    "All unauthorized immigrants should be given a pathway to earn U.K citizenship." = 0.75,
    "All unauthorized immigrants should be granted full U.K citizenship, without any conditions." = 1
  )
  return(mapping[response])
}

map_q77 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  mapping <- c(
    "To boost the economy by bringing in refugees with necessary skills" = 0,
    "It is a legal obligation under international law" = 0.5,
    "To promote diversity and multiculturalism" = 0.75,
    "It is a humanitarian obligation" = 1
  )
  return(mapping[response])
}

map_q80 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  mapping <- c(
    "Much worse" = 0,
    "Worse" = 0.25,
    "The same" = 0.5,
    "Better" = 0.75,
    "Much better" = 1
  )
  return(mapping[response])
}

map_q119 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  if (response == "More") {
    return(1)
  } else if (response == "Less") {
    return(0)
  } else if (response == "Unsure") {
    return(0.5)
  } else {
    return(NA)
  }
}


mapreverse <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  if (response == "Describes me very well") {
    return(0)
  } else if (response == "Describes me somewhat well") {
    return(0.25)
  } else if (response == "Neither describes me well nor poorly (Neutral)") {
    return(0.5)
  } else if (response == "Describes me poorly (Disagree)") {
    return(0.75)
  } else if (response == "Describes me very poorly (Strongly disagree)") {
    return(1)
  } else {
    return(NA)
  }
}

mapforward <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  if (response == "Describes me very poorly (Strongly disagree)") {
    return(0)
  } else if (response == "Describes me poorly (Disagree)") {
    return(0.25)
  } else if (response == "Neither describes me well nor poorly (Neutral)") {
    return(0.5)
  } else if (response == "Describes me somewhat well") {
    return(0.75)
  } else if (response == "Describes me very well") {
    return(1)
  } else {
    return(NA)
  }
}

map_binary <- function(response, correct_answer) {
  if (is.na(response)) {
    return(NA)
  }
  if (response == correct_answer) {
    return(1)
  } else {
    return(0)
  }
}

map_q62 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  mapping <- c(
    "Almost never" = 0,
    "Several times a year" = 0.25,
    "Several times a month" = 0.5,
    "2-3 times a week" = 0.75,
    "Daily" = 1
  )
  return(mapping[response])
}

map_q64 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  # Try converting the response to a number and divide by 10
  tryCatch({
    return(as.numeric(response) / 10)
  }, error = function(e) {
    return(NA)
  })
}

map_q66 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  mapping <- c(
    "Very unlikely" = 0,
    "Somewhat unlikely" = 0.25,
    "Neutral" = 0.5,
    "Somewhat likely" = 0.75,
    "Very likely" = 1
  )
  return(mapping[response])
}

map_q67 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  mapping <- c(
    "Not at all" = 0,
    "A little" = 0.25,
    "Moderately" = 0.5,
    "A lot" = 0.75,
    "A great deal" = 1
  )
  return(mapping[response])
}

map_q138_custom <- function(row) {
  out <- rep(0, 7)
  
  # Case 1: If any are 3, set only those to 1
  if (any(row == 3, na.rm = TRUE)) {
    out <- ifelse(row == 3, 1, 0)
  } else {
    # Case 2: If none are 3, find the "On" value
    on_index <- which(row == "On")
    if (length(on_index) == 1) {
      out[on_index] <- 1
    }
  }
  
  return(out)
}


map_q39 <- function(response) {
  # Define the conditions
  conditions <- c(
    "The graduation programme significantly improved livelihoods for both host community members and refugees",
    "Host community members often resent refugees who are recipients of humanitarian aid",
    "Financial security leads to higher social cohesion between refugees and host community members",
    "Even a small reduction in financial security was associated with a large drop in social cohesion as host community members became less trusting, less willing to become friends with refugees and less willing to share resources with refugees"
  )
  
  # Check for NA
  if (is.na(response)) {
    return(NA)
  }
  
  # Check if all conditions are present in the response
  if (all(sapply(conditions, function(cond) grepl(cond, response)))) {
    return(1)
  } else {
    return(0)
  }
}

map_q42 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  return(ifelse(response == "The long-term sustainability of refugee camps", 1, 0))
}

map_q43 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  return(ifelse(response == "36 million", 1, 0))
}

map_q70 <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  return(ifelse(response == "Stay within their own country", 1, 0))  # "Neighbouring countries" is most common but not correct.
}

mapdonation <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  return(response / 30)  
}

#Apply mapping
mydata <- mydata %>%
  mutate(
    Q29_mapped = sapply(Q29, map_q29),
    Q30_mapped = sapply(Q30, map_q30),
    Q31_mapped = sapply(Q31, map_q31),
    Q101_1_mapped = sapply(Q101_1, map_q101),
    Q101_2_mapped = sapply(Q101_2, map_q101),
    Q101_3_mapped = sapply(Q101_3, map_q101),
    Q101_4_mapped = sapply(Q101_4, map_q101),
    Q102_mapped = sapply(Q102, map_q102),
    Q103_mapped = sapply(Q103, map_q103),
    Q104_mapped = sapply(Q104, map_q104),
    Q105_mapped = sapply(Q105, map_q105),
    Q106_mapped = sapply(Q106, map_q106),
    Q107_1_mapped = sapply(Q107_1, map_q107),
    Q107_2_mapped = sapply(Q107_2, map_q107),
    Q40_mapped = sapply(Q40, map_q40),
    Q41_mapped = sapply(Q41, map_q41),
    Q110_1_mapped = sapply(Q110_1, map100),
    Q111_1_mapped = sapply(Q111._1, map100),
    Q113_1_mapped = sapply(Q113_1, map100_reverse),
    Q49_mapped = sapply(Q49, map2),
    Q50_mapped = sapply(Q50, map2),
    Q51_mapped = sapply(Q51, map2),
    Q243_mapped = sapply(Q243, map1),
    Q53_mapped = sapply(Q53, mapagree),
    Q54_mapped = sapply(Q54, mapdisagree),
    Q60_mapped = sapply(Q60, mapagree),
    Q57_mapped = sapply(Q57, mapdisagree),
    Q52_mapped = sapply(Q52, mapdisagree),
    Q55_mapped = sapply(Q55, mapagree),
    Q56_mapped = sapply(Q56, mapdisagree),
    Q61_mapped = sapply(Q61, mapagree),
    Q197_mapped = sapply(Q197, mapagree),
    Q115_mapped = sapply(Q115, mapdisagree),
    Q116_mapped = sapply(Q116, mapagree),
    Q58_mapped = sapply(Q58, mapdisagree),
    Q80_1_mapped = sapply(Q80_1, map_q80),
    Q80_2_mapped = sapply(Q80_2, map_q80),
    Q80_3_mapped = sapply(Q80_3, map_q80),
    Q80_4_mapped = sapply(Q80_4, map_q80),
    Q80_5_mapped = sapply(Q80_5, map_q80),
    Q80_6_mapped = sapply(Q80_6, map_q80),
    Q80_7_mapped = sapply(Q80_7, map_q80),
    Q80_8_mapped = sapply(Q80_8, map_q80),
    Q80_9_mapped = sapply(Q80_9, map_q80),
    Q80_10_mapped = sapply(Q80_10, map_q80),
    Q80_11_mapped = sapply(Q80_11, map_q80),
    Q71_mapped = sapply(Q71, map_q71),
    Q77_mapped = sapply(Q77, map_q77),
    Q119_mapped = sapply(Q119, map_q119),
    Q82_mapped = sapply(Q82, mapreverse),
    Q88_mapped = sapply(Q88, mapreverse),
    Q84_mapped = sapply(Q84, mapforward),
    Q85_mapped = sapply(Q85, mapforward),
    Q91_mapped = sapply(Q91, mapforward),
    Q92_mapped = sapply(Q92, mapforward),
    Q89_mapped = sapply(Q89, mapforward),
    Q93_mapped = sapply(Q93, mapforward),
    Q121_mapped = sapply(Q121, map_binary, correct_answer = "0.60%"),
    Q122_mapped = sapply(Q122, map_binary, correct_answer = "4.20%"),
    Q123_mapped = sapply(Q123, map_binary, correct_answer = "76%"),
    Q124_mapped = sapply(Q124, map_binary, correct_answer = "1 million"),
    Q127_mapped = sapply(Q127, map_binary, correct_answer = "51%"),
    Q126_mapped = sapply(Q126, map_binary, correct_answer = "Germany"),
    Q214_mapped = sapply(Q214, map_binary, correct_answer = "30"),
    Q62_mapped = sapply(Q62, map_q62),
    Q64_mapped = sapply(Q64_1, map_q64),
    Q39_mapped = sapply(Q39, map_q39),
    Q42_mapped = sapply(Q42, map_q42),
    Q43_mapped = sapply(Q43, map_q43),
    Q70_mapped = sapply(Q70, map_q70),
    refugee_donation_mapped = sapply(refugee_donation, mapdonation),
    climate_donation_mapped = sapply(climate_donation, mapdonation)
  )

# Apply the mapping functions to the sub-questions (_1 to _5) for Q66 and Q67
for (i in 1:5) {
  mydata[[paste0("Q66_", i, "_mapped")]] <- sapply(mydata[[paste0("Q66_", i)]], map_q66)
  mydata[[paste0("Q67_", i, "_mapped")]] <- sapply(mydata[[paste0("Q67_", i)]], map_q67)
}

# Apply the mapping function to Q138_1 to Q138_7
q138_data <- mydata[, paste0("Q138_", 1:7)]
q138_mapped <- t(apply(q138_data, 1, map_q138_custom))

# Assign new mapped columns back
for (i in 1:7) {
  mydata[[paste0("Q138_", i, "_mapped")]] <- q138_mapped[, i]
}



# Write to an Excel file with the new Clean sheet
write_xlsx(mydata, "add_to_path/Mapped_Questions.xlsx")

