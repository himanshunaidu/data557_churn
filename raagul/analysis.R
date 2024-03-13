library(dplyr)
library(ggplot2)
library(readr)

data <- read_csv("Documents/data557/data557_churn/data/Telco_customer_churn_cleaned.csv")
data_best <- read_csv("Documents/best_case_final.csv")
data_worst <- read_csv("Documents/worst_case_final.csv")
# View(data_best)
CONF_LEVEL=0.95

perform_chi_square_test <- function(case, column1, column2, column_names,
                                    filter1, filter1_label,
                                    filter2, filter2_label,
                                    confidence_level=0.95) {
  cat('--------------', '\n')
  cat(column_names[1], case, '\n')
  cat('--------------', '\n')
  # cat('Proportion of ', filter1_label, ' in dataset - ', mean(filter1), '\n')
  # cat('Proportion of ', filter2_label, ' in dataset - ', mean(filter2), '\n\n')
  
  # Check for missing values
  if (any(is.na(column1)) | any(is.na(column2))) {
    stop("Error: The data contains missing values. This violates assumptions of the Chi-square test.")
  }
  
  churn_table <- table(column1, column2)
  # names(churn_table) <- c("Gender", "Churn Label")
  print(churn_table)
  cat('\n')
  
  expected_freq <- apply(churn_table, 1, function(x) {
    rowSums(churn_table) * colSums(churn_table) / sum(churn_table)
  })
  
  # Check for small expected frequencies
  small_expected_counts <- sum(expected_freq < 5)
  if (small_expected_counts > 0) {
    warning("Warning: Some cells have expected frequencies less than 5. This may violate assumptions of the Chi-square test.")
    warning("Consider using Fisher's exact test for small samples or sparse data.")
  }
  
  # Calculate Chi-square statistic
  # chi_square <- sum((churn_table - expected_freq)^2 / expected_freq)
  chi_square_test <- chisq.test(churn_table)
  # print(chi_square_test)
  
  # Print the results
  cat("Chi-square statistic:", chi_square_test$statistic, "\n")
  cat("Degrees of freedom:", chi_square_test$parameter, "\n")
  cat("p-value:", chi_square_test$p.value, "\n")
  
  # Interpretation
  if (chi_square_test$p.value < 0.05) {
    message = paste(
      "Statistically significant evidence (p-value < 0.05) that",
      column_names[1],
      "and churn value are associated\n\n"
    )
    cat(message)
  } else {
    message = paste(
      "No statistically significant evidence (p-value >= 0.05) that",
      column_names[1],
      "and churn value are related.\n\n"
    )
    cat(message)
  }
}

# All
# perform_chi_square_test(
#   "",
#   data$Gender,
#   data$`Churn Label`,
#   c('Gender', 'Churn Label'),
#   data$Gender == 'Male',
#   'men',
#   data$Gender == 'Female',
#   'female'
# )
# Best
perform_chi_square_test(
  "BEST CASE",
  data_best$Gender,
  data_best$churn_12month,
  c('Gender', 'churn_12month'),
  data_best$Gender == 'Male',
  'men',
  data_best$Gender == 'Female',
  'female'
)
# Worst
perform_chi_square_test(
  "WORST CASE",
  data_worst$Gender,
  data_worst$churn_12month,
  c('Gender', 'churn_12month'),
  data_worst$Gender == 'Male',
  'men',
  data_worst$Gender == 'Female',
  'female'
)




# SENIOR CITIZEN
# perform_chi_square_test(
#   "",
#   data$'Senior Citizen',
#   data$`Churn Label`,
#   c('Senior Citizen', 'Churn Label'),
#   data$'Senior Citizen' == 'Yes',
#   'senior',
#   data$'Senior Citizen' == 'No',
#   'non-senior'
# )
# Best
perform_chi_square_test(
  "BEST CASE",
  data_best$'Senior Citizen',
  data_best$churn_12month,
  c('Senior Citizen', 'churn_12month'),
  data$'Senior Citizen' == 'Yes',
  'senior',
  data$'Senior Citizen' == 'No',
  'non-senior'
)
# Worst
perform_chi_square_test(
  "WORST CASE",
  data_worst$'Senior Citizen',
  data_worst$churn_12month,
  c('Senior Citizen', 'churn_12month'),
  data$'Senior Citizen' == 'Yes',
  'senior',
  data$'Senior Citizen' == 'No',
  'non-senior'
)



# PARTNER
# perform_chi_square_test(
#   "",
#   data$Partner,
#   data$`Churn Label`,
#   c('Partner', 'Churn Label'),
#   data$Partner == 'Yes',
#   'people with partners',
#   data$Partner == 'No',
#   'people without partners'
# )
# Best
perform_chi_square_test(
  "BEST CASE",
  data_best$Partner,
  data_best$churn_12month,
  c('Partner', 'churn_12month'),
  data$Partner == 'Yes',
  'people with partners',
  data$Partner == 'No',
  'people without partners'
)
# Worst
perform_chi_square_test(
  "WORST CASE",
  data_worst$Partner,
  data_worst$churn_12month,
  c('Partner', 'churn_12month'),
  data$Partner == 'Yes',
  'people with partners',
  data$Partner == 'No',
  'people without partners'
)



# DEPENDENTS
# perform_chi_square_test(
#   "",
#   data$Dependents,
#   data$`Churn Label`,
#   c('Dependents', 'Churn Label'),
#   data$Dependents == 'Yes',
#   'people with dependents',
#   data$Dependents == 'No',
#   'people without dependents'
# )
# Best
perform_chi_square_test(
  "BEST CASE",
  data_best$Dependents,
  data_best$churn_12month,
  c('Dependents', 'churn_12month'),
  data$Dependents == 'Yes',
  'people with dependents',
  data$Dependents == 'No',
  'people without dependents'
)
# Worst
perform_chi_square_test(
  "WORST CASE",
  data_worst$Dependents,
  data_worst$churn_12month,
  c('Dependents', 'churn_12month'),
  data$Dependents == 'Yes',
  'people with dependents',
  data$Dependents == 'No',
  'people without dependents'
)


