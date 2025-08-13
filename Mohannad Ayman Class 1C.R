

gene <- "TP53"
gene 
# Store numeric values in one variable 
expression_levels <- c(2.3, 4.6, 3.6, 7.2, 4.7)
raw_data <- read.csv("C://Users/BAB AL SAFA/Downloads/patient_info.csv")
raw_data
# Removing Columns
data <- raw_data
raw_data$patient_id <- NULL
clean_data <- data[, -1]
# Sort age from largest to smallest
sorted_age <- sort(raw_data$age, decreasing = TRUE)
sorted_age
# Sort age from smallest to largest
sorted_age2 <- sort(raw_data$age, decreasing = FALSE)
sorted_age2
# Use if & else 
cholesterol <- 230
  if (cholesterol < 240) {
  print("High Cholesterol")
}

Systolic_bp <- 130
if (Systolic_bp < 120) {
  print("Blood Pressure is normal")
} else {
  print("Blood Pressure is high")
}

str(raw_data)

# The loop
clean_data <- raw_data
str(clean_data)
for (i in 1:ncol(clean_data)) {
  if (is.character(clean_data[[i]])) {
    clean_data[[i]] <- as.factor(clean_data[[i]])
  }



