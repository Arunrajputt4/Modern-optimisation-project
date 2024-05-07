# Load dataset
facebook_ad <- read.csv("D:/University Learning Support/Arun/Modern Optimization/project/facebook_ads.csv", stringsAsFactors = FALSE)

# Convert 'gender' and 'interest' to factors
facebook_ad$gender <- as.factor(facebook_ad$gender)
facebook_ad$interest <- as.factor(facebook_ad$interest)

# Convert age ranges to numeric
facebook_ad$age <- as.numeric(gsub("\\D+", "", facebook_ad$age))

# Label encoding for 'gender' and 'interest'
facebook_ad$gender <- as.numeric(facebook_ad$gender)
facebook_ad$interest <- as.numeric(facebook_ad$interest)

# Check for missing values
missing_values <- sapply(facebook_ad, function(x) sum(is.na(x)))
print(missing_values)

# Impute missing values in numeric columns
numeric_cols <- sapply(facebook_ad, is.numeric)
for (col in names(facebook_ad)[numeric_cols]) {
  if (sum(is.na(facebook_ad[, col])) > 0) {
    facebook_ad[, col][is.na(facebook_ad[, col])] <- mean(facebook_ad[, col], na.rm = TRUE)
  }
}

# Impute missing values in categorical columns
categorical_cols <- sapply(facebook_ad, is.factor)
for (col in names(facebook_ad)[categorical_cols]) {
  if (sum(is.na(facebook_ad[, col])) > 0) {
    facebook_ad[, col][is.na(facebook_ad[, col])] <- mode(facebook_ad[, col], na.rm = TRUE)
  }
}

# Check if there are still missing values
print(sum(is.na(facebook_ad)))

# Remove redundant features
facebook_ad <- facebook_ad[, !(names(facebook_ad) %in% c("ad_id", "xyz_campaign_id", "fb_campaign_id"))]

# Check data type of 'Approved_Conversion' column
str(facebook_ad$Approved_Conversion)

# Convert to 0s and 1s
if (is.character(facebook_ad$Approved_Conversion)) {
  facebook_ad$Approved_Conversion <- as.numeric(facebook_ad$Approved_Conversion)
  facebook_ad$Approved_Conversion[facebook_ad$Approved_Conversion > 1] <- 1
  facebook_ad$Approved_Conversion[facebook_ad$Approved_Conversion < 0] <- 0
} else if (is.numeric(facebook_ad$Approved_Conversion)) {
  facebook_ad$Approved_Conversion[facebook_ad$Approved_Conversion > 1] <- 1
  facebook_ad$Approved_Conversion[facebook_ad$Approved_Conversion < 0] <- 0
}

# Confirm the data type is numeric and contains only 0s and 1s
str(facebook_ad$Approved_Conversion)
table(facebook_ad$Approved_Conversion)

# Save the cleaned dataset as a new CSV file
write.csv(facebook_ad, "D:/University Learning Support/Arun/Modern Optimization/Project/clean_data.csv", row.names = FALSE)

