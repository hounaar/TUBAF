# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(psych)
library(DescTools)
library(reshape2)
library(Amelia)

# Load the dataset
file_path <- "testData123.csv"
data <- read.csv(file_path)

# View basic dataset information
str(data)
summary(data)
head(data)

# Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)

# Check for unusual patterns
# Univariate Analysis
numeric_columns <- data %>% select_if(is.numeric)

# Distribution Plots
for (col in names(numeric_columns)) {
  print(ggplot(data, aes_string(x = col)) +
          geom_histogram(bins = 30, fill = "blue", color = "black") +
          ggtitle(paste("Distribution of", col)) +
          theme_minimal())
}

# Check skewness
skewness_values <- sapply(numeric_columns, psych::skew)
print("Skewness of variables:")
print(skewness_values)

# Detect outliers using boxplots
for (col in names(numeric_columns)) {
  print(ggplot(data, aes_string(y = col)) +
          geom_boxplot(fill = "orange", color = "black") +
          ggtitle(paste("Boxplot of", col)) +
          theme_minimal())
}

# Check for bindings or rounded values
for (col in names(numeric_columns)) {
  unique_values <- length(unique(data[[col]]))
  cat("Column:", col, "has", unique_values, "unique values\n")
}

# Dependencies and correlations
cor_matrix <- cor(numeric_columns, use = "complete.obs")
print(cor_matrix)
print(ggplot(melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2() +
        ggtitle("Correlation Heatmap") +
        theme_minimal())

# Dependency scatterplots
combinations <- combn(names(numeric_columns), 2)
for (i in 1:ncol(combinations)) {
  x <- combinations[1, i]
  y <- combinations[2, i]
  print(ggplot(data, aes_string(x = x, y = y)) +
          geom_point(color = "blue") +
          ggtitle(paste("Scatterplot:", x, "vs", y)) +
          theme_minimal())
}

# Visual Inspection for Tectonic Faults (if applicable)
if ("Longitude" %in% names(data) && "Latitude" %in% names(data)) {
  print(ggplot(data, aes(x = Longitude, y = Latitude)) +
          geom_point(color = "red") +
          ggtitle("Spatial Distribution") +
          theme_minimal())
}

# Check if observations match expectations
# Add domain-specific checks here if applicable

# Save output
write.csv(missing_values, "missing_values.csv")
write.csv(skewness_values, "skewness_values.csv")
write.csv(cor_matrix, "correlation_matrix.csv")

# --- Additional visualizations for answering questions ---

# Question 1: Is something unusual? Why?
# Visualizing unusual patterns in x and y columns
ggplot(data, aes(x = x)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  ggtitle("Distribution of x (Unusual Patterns)") +
  theme_minimal()

ggplot(data, aes(x = y)) +
  geom_histogram(bins = 30, fill = "orange", color = "black") +
  ggtitle("Distribution of y (Unusual Patterns)") +
  theme_minimal()

# Question 2: How are the data distributed? Are the data skewed?
# Visualizing distribution and checking skewness for numeric columns
ggplot(data, aes(x = Co)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  ggtitle("Distribution of Co (Cobalt)") +
  theme_minimal()

ggplot(data, aes(x = Ni)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  ggtitle("Distribution of Ni (Nickel)") +
  theme_minimal()

# Question 3: Are there outliers or bindings (are the values rounded)?
# Detecting outliers using boxplots
ggplot(data, aes(y = Co)) +
  geom_boxplot(fill = "purple", color = "black") +
  ggtitle("Boxplot of Co (Cobalt)") +
  theme_minimal()

ggplot(data, aes(y = Ni)) +
  geom_boxplot(fill = "red", color = "black") +
  ggtitle("Boxplot of Ni (Nickel)") +
  theme_minimal()

# Question 4: Is the optical impression falsified through special features?
# Re-plot distributions with improved bin sizes
ggplot(data, aes(x = Co)) +
  geom_histogram(bins = 50, fill = "blue", color = "black") +
  ggtitle("Distribution of Co (Cobalt) - Better Bin Size") +
  theme_minimal()

ggplot(data, aes(x = Ni)) +
  geom_histogram(bins = 50, fill = "green", color = "black") +
  ggtitle("Distribution of Ni (Nickel) - Better Bin Size") +
  theme_minimal()

# Question 5: Can you see tectonic faults or hops in the map/data?
# Spatial data visualization (if applicable)
ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "red") +
  ggtitle("Spatial Distribution of x and y") +
  theme_minimal()

# Question 6: Which dependencies can you see?
# Correlation matrix and heatmap
cor_matrix <- cor(numeric_columns, use = "complete.obs")
cor_matrix_melted <- melt(cor_matrix)

ggplot(cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2() +
  ggtitle("Correlation Heatmap") +
  theme_minimal()


# Question 7: Are the dependencies strong or weak, linear or nonlinear, increasing or decreasing?
# Scatterplot for x vs Ni to show a weak, negative correlation
ggplot(data, aes(x = x, y = Ni)) +
  geom_point(color = "purple") +
  ggtitle("Scatterplot: x vs Ni (Weak, Negative Correlation)") +
  theme_minimal()

# Scatterplot for Co vs Ni to show a moderate, positive linear correlation
ggplot(data, aes(x = Co, y = Ni)) +
  geom_point(color = "red") +
  ggtitle("Scatterplot: Co vs Ni (Moderate Positive Correlation)") +
  theme_minimal()

# Question 8: Do the observations correspond to what we would expect?
# Checking the scatterplot of y vs Ni (to verify if y is truly independent)
ggplot(data, aes(x = y, y = Ni)) +
  geom_point(color = "purple") +
  ggtitle("Scatterplot: y vs Ni (Expected Independence)") +
  theme_minimal()

# Question 9: Any other noticeable problems?
# Visualizing missing values (if any)
missmap(data, main = "Missing Data Map", col = c("yellow", "black"))
