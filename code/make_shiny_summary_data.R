# Produce a summary dataset that contains all the unique categories to use
# as choices in app inputs
library(dplyr)
library(readr)

prn <- read_rds("data/pruned.rds")

summary_data <- select(prn, -LogSalary) %>% 
  distinct()

levels(summary_data$YearsCodingProf) <- c("0-2 years", "3-5 years", "6-8 years", "9-11 years", "12-14 years", "15-17 years", "18-20 years", "21-23 years", 
  "24-26 years", "27-29 years",  "30 or more years")

levels(summary_data$Age) <- c("Under 18 years old", "18 - 24 years old", "25 - 34 years old", "35 - 44 years old", 
                              "45 - 54 years old", "55 - 64 years old", "65 years or older")

levels(summary_data$FormalEducation) <- c(
  "I never completed any formal education", 
  "Primary/elementary school", 
  "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)", 
  "Some college/university study without earning a degree",
  "Associate degree", 
  "Bachelor’s degree (BA, BS, B.Eng., etc.)", 
  "Professional degree (JD, MD, etc.)",
  "Master’s degree (MA, MS, M.Eng., MBA, etc.)", 
  "Other doctoral degree (Ph.D, Ed.D., etc.)"
  )

write_rds(summary_data, "app/shiny_summary_data.rds")
