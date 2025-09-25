# Install and load necessary libraries
# install.packages(c("dplyr", "ggplot2", "tidyr", "corrplot"))
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)


dir.create("results", showWarnings = FALSE)


data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
View(data)


#  DATA SUMMARY

# Concise Summary of the Data
cat("\n--- Data Structure ---\n")
str(data)

# Statistical Summary
cat("\n--- Statistical Summary ---\n")
summary(data)

# Checking Missing Values
cat("\n--- Missing Values Check ---\n")
sapply(data, function(x) sum(is.na(x)))


# DATA CLEANING

# Removing columns that are not useful for analysis.
cat("\n--- Removing Constant Columns ---\n")
data <- data %>%
  select(-EmployeeCount, -StandardHours, -Over18)

# Converting character variables to factors
data$Attrition <- as.factor(data$Attrition)
data$OverTime <- as.factor(data$OverTime)
data$Gender <- as.factor(data$Gender)


#  EXPLORATORY DATA ANALYSIS (EDA)

#  1) UNIVARIATE ANALYSIS (Exploring Single Variables)

#  Target Variable: Attrition

#  Overall attrition rate in the company.
plot_attrition_dist <- ggplot(data, aes(x = Attrition, fill = Attrition)) +
  geom_bar() +
  ggtitle("Distribution of Employee Attrition") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  theme_minimal()
print(plot_attrition_dist)
ggsave("results/01_attrition_distribution.png", plot = plot_attrition_dist)
# Insight: The dataset is imbalanced. There are significantly more employees who stayed than who left.


# Key Demographics

# Age distribution
plot_age_dist <- ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  ggtitle("Age Distribution of Employees") +
  theme_minimal()
print(plot_age_dist)
ggsave("results/02_age_distribution.png", plot = plot_age_dist)
# Insight: The majority of the workforce is between their late 20s and early 40s.

# Gender distribution
plot_gender_dist <- ggplot(data, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  ggtitle("Gender Distribution") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  theme_minimal()
print(plot_gender_dist)
ggsave("results/03_gender_distribution.png", plot = plot_gender_dist)


# Job-Related Variables

# Distribution of Job Roles
plot_job_roles <- ggplot(data, aes(y = JobRole, fill = JobRole)) +
  geom_bar() +
  ggtitle("Distribution of Job Roles") +
  theme_minimal() +
  theme(legend.position = "none") # Hide legend as it's redundant
print(plot_job_roles)
ggsave("results/04_job_roles_distribution.png", plot = plot_job_roles)


# Monthly Income Distribution
plot_income_dist <- ggplot(data, aes(x = MonthlyIncome)) +
  geom_density(fill = "darkgreen", alpha = 0.7) +
  ggtitle("Distribution of Monthly Income") +
  scale_x_continuous(labels = scales::dollar) +
  theme_minimal()
print(plot_income_dist)
ggsave("results/05_monthly_income_distribution.png", plot = plot_income_dist)
# Insight: The income distribution is right-skewed, with most employees earning a lower salary.

# 2) BIVARIATE ANALYSIS (Exploring Relationships)

# Attrition vs. Other Variables

# Attrition by OverTime
plot_attrition_overtime <- ggplot(data, aes(x = OverTime, fill = Attrition)) +
  geom_bar(position = "fill") +
  ggtitle("Attrition Rate by OverTime Status") +
  ylab("Proportion") +
  theme_minimal()
print(plot_attrition_overtime)
ggsave("results/06_attrition_by_overtime.png", plot = plot_attrition_overtime)


# Attrition by Marital Status
plot_attrition_marital <- ggplot(data, aes(x = MaritalStatus, fill = Attrition)) +
  geom_bar(position = "fill") +
  ggtitle("Attrition Rate by Marital Status") +
  ylab("Proportion") +
  theme_minimal()
print(plot_attrition_marital)
ggsave("results/07_attrition_by_marital_status.png", plot = plot_attrition_marital)


# Attrition by Job Role
plot_attrition_jobrole <- ggplot(data, aes(y = JobRole, fill = Attrition)) +
  geom_bar(position = "fill") +
  ggtitle("Attrition Rate by Job Role") +
  xlab("Proportion") +
  theme_minimal()
print(plot_attrition_jobrole)
ggsave("results/08_attrition_by_job_role.png", plot = plot_attrition_jobrole, width = 8, height = 6)


# Attrition vs. Numerical Variables

# Attrition by Monthly Income
plot_income_attrition_box <- ggplot(data, aes(x = Attrition, y = MonthlyIncome, fill = Attrition)) +
  geom_boxplot() +
  ggtitle("Monthly Income vs. Attrition") +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal()
print(plot_income_attrition_box)
ggsave("results/09_income_vs_attrition_boxplot.png", plot = plot_income_attrition_box)


# Attrition by Age
plot_age_attrition_box <- ggplot(data, aes(x = Attrition, y = Age, fill = Attrition)) +
  geom_boxplot() +
  ggtitle("Age vs. Attrition") +
  theme_minimal()
print(plot_age_attrition_box)
ggsave("results/10_age_vs_attrition_boxplot.png", plot = plot_age_attrition_box)


# Attrition by Years at Company
plot_years_attrition_density <- ggplot(data, aes(x = YearsAtCompany, fill = Attrition)) +
  geom_density(alpha = 0.6) +
  ggtitle("Years at Company vs. Attrition") +
  theme_minimal()
print(plot_years_attrition_density)
ggsave("results/11_years_at_company_vs_attrition.png", plot = plot_years_attrition_density)


# 3)  Correlation Analysis of Numerical Variables

# Select only numeric columns for the correlation matrix
numeric_data <- data %>%
  select_if(is.numeric)

# Calculate the correlation matrix
cor_matrix <- cor(numeric_data)

# Visualize the correlation matrix with a heatmap
# Positive correlations are in blue, negative in red.
# Larger circles mean stronger correlation.
cat("\n--- Correlation Matrix of Numerical Variables ---\n")

# Saving the corrplot requires a different method
png("results/12_correlation_matrix.png", width=8, height=8, units="in", res=300)
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 90, tl.cex = 0.4,
         title = "Correlation Matrix", mar=c(2,0,6,0))
dev.off() # This closes the PNG device and saves the file


# 4)  DESCRIPTIVE STATISTICS ANALYSIS

# Detailed summary statistics grouped by Attrition
summary_by_attrition <- data %>%
  group_by(Attrition) %>%
  summarise(
    Mean_Age = mean(Age),
    Median_Age = median(Age),
    Mean_MonthlyIncome = mean(MonthlyIncome),
    Median_MonthlyIncome = median(MonthlyIncome),
    Mean_YearsAtCompany = mean(YearsAtCompany),
    Median_YearsAtCompany = median(YearsAtCompany),
    Mean_JobSatisfaction = mean(JobSatisfaction),
    Median_JobSatisfaction = median(JobSatisfaction),
    Mean_TotalWorkingYears = mean(TotalWorkingYears),
    Median_TotalWorkingYears = median(TotalWorkingYears),
    Count = n() # Get the count in each group
  )

print(summary_by_attrition)



# JOB VARIABLES BY ATTRITION
# analyzing the proportions of attrition across different categorical job-related variables. This helps us pinpoint specific roles, departments, or fields where attrition is a bigger problem.
# the below code calculates the percentage of attrition within each department.

# Attrition proportions by Department
department_attrition <- data %>%
  group_by(Department, Attrition) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

print(department_attrition)

# Plotting Graph
plot_dept_attrition <- ggplot(department_attrition, aes(x = Department, y = Percentage, fill = Attrition)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Attrition Percentage by Department") +
  theme_minimal()
print(plot_dept_attrition)
ggsave("results/13_attrition_percentage_by_department.png", plot = plot_dept_attrition)


# Attrition rate by education field

# Attrition proportions by Education Field
education_attrition <- data %>%
  group_by(EducationField, Attrition) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

print(education_attrition)

# Plotting Graph
plot_edu_attrition <- ggplot(education_attrition, aes(y = EducationField, x = Percentage, fill = Attrition)) +
  geom_bar(stat = "identity") +
  ggtitle("Attrition Percentage by Education Field") +
  theme_minimal()
print(plot_edu_attrition)
ggsave("results/14_attrition_percentage_by_education.png", plot = plot_edu_attrition)


# Visual
# The below plot can help us see if low-paid, unsatisfied employees are the most likely to leave.

# Scatter plot of Job Satisfaction vs. Monthly Income, colored by Attrition
plot_satisfaction_income <- ggplot(data, aes(x = JobSatisfaction, y = MonthlyIncome, color = Attrition)) +
  geom_jitter(alpha = 0.6) + # Jitter helps see overlapping points
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("Job Satisfaction vs. Monthly Income by Attrition Status") +
  facet_wrap(~ Attrition) + # Create separate plots for 'Yes' and 'No'
  theme_minimal()
print(plot_satisfaction_income)
ggsave("results/15_satisfaction_vs_income_by_attrition.png", plot = plot_satisfaction_income, width = 10, height = 6)


# 5) Inferential Statistical Analysis

# Hypothesis Test 1: t-test

# Verifying t-test Assumption 1: Independence

# 1. Count the number of employees in the 'Yes' Attrition group
count_yes <- sum(data$Attrition == "Yes")

# 2. Count the number of employees in the 'No' Attrition group
count_no <- sum(data$Attrition == "No")

# 3. Get the total number of employees (total rows) in the dataset
total_count <- nrow(data)

# 4. Print the counts
cat("Employees who left (Attrition = Yes):", count_yes, "\n")
cat("Employees who stayed (Attrition = No):", count_no, "\n")
cat("Total employees in the dataset:", total_count, "\n")
cat("Sum of the two groups:", count_yes + count_no, "\n\n")


# 5. Logically check if the assumption holds
if (total_count == (count_yes + count_no)) {
  print("Assumption of Independence is MET. The two groups are mutually exclusive.")
} else {
  print("Assumption of Independence is VIOLATED. There is an issue with the data.")
}

#Verifying t-test Assumption 2: Normality

# Create Q-Q plots for Monthly Income, separated by Attrition status
plot_qq_income <- ggplot(data, aes(sample = MonthlyIncome)) +
  stat_qq() +      # Adds the points for the Q-Q plot
  stat_qq_line() + # Adds the reference line
  facet_wrap(~ Attrition, scales = "free") + # Creates separate plots for 'Yes' and 'No'
  ggtitle("Q-Q Plots of Monthly Income by Attrition Status") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_minimal()
print(plot_qq_income)
ggsave("results/16_qq_plot_monthly_income.png", plot = plot_qq_income)


# As the NORMALITY ASSUMPTION WAS VIOLATING so to improve it I applied this.
# Correcting for Non-Normality with a Log Transformation

# 1. Create a new column with the log-transformed Monthly Income
data$Log_MonthlyIncome <- log(data$MonthlyIncome)

# 2. Create a new Q-Q plot using the transformed data
plot_qq_log_income <- ggplot(data, aes(sample = Log_MonthlyIncome)) +
  stat_qq() +
  stat_qq_line(color = "red", linetype = "dashed") + # Make the line stand out
  facet_wrap(~ Attrition, scales = "free") +
  ggtitle("Q-Q Plots of LOG-TRANSFORMED Monthly Income") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles (Log Scale)") +
  theme_minimal()
print(plot_qq_log_income)
ggsave("results/17_qq_plot_log_transformed_income.png", plot = plot_qq_log_income)


# Verifying t-test Assumption 3: Homogeneity of Variances

# 1. Calculate the variance of Monthly Income for the 'No' Attrition group
variance_no <- var(data$MonthlyIncome[data$Attrition == "No"])

# 2. Calculate the variance of Monthly Income for the 'Yes' Attrition group
variance_yes <- var(data$MonthlyIncome[data$Attrition == "Yes"])

cat("Variance for 'No' Attrition group:", variance_no, "\n")
cat("Variance for 'Yes' Attrition group:", variance_yes, "\n\n")

# 3. Calculate the ratio of the larger variance to the smaller variance
larger_variance <- max(variance_no, variance_yes)
smaller_variance <- min(variance_no, variance_yes)
variance_ratio <- larger_variance / smaller_variance

cat("Ratio of larger to smaller variance:", variance_ratio, "\n")

# 4. Check if the ratio is less than 4
if (variance_ratio < 4) {
  print("Assumption of Homogeneity of Variances is MET.")
} else {
  print("Assumption of Homogeneity of Variances is VIOLATED.")
}


# Final T-Test and Interpretation

# Perform the Welch Two Sample t-test
# R automatically uses the Welch test, which does not assume equal variances.
final_t_test_result <- t.test(MonthlyIncome ~ Attrition, data = data)

# Print the detailed results of the test
print(final_t_test_result)

#Since this number is much smaller than our significance level of 0.05, we reject the null hypothesis.



# Hypothesis Test 2: Chi-Square Test
# Verifying Chi-Square Assumption 1: Categorical Variables


# 1. Check the data type of the 'Attrition' variable
class_attrition <- class(data$Attrition)
cat("The data type for 'Attrition' is:", class_attrition, "\n")

# 2. Check the data type of the 'OverTime' variable
class_overtime <- class(data$OverTime)
cat("The data type for 'OverTime' is:", class_overtime, "\n\n")

# 3. A logical check and conclusion
if (is.factor(data$Attrition) && is.factor(data$OverTime)) {
  print("Assumption MET: Both variables are categorical (factors).")
} else {
  print("Assumption VIOLATED: At least one variable is not categorical.")
}


# Verifying Chi-Square Assumption 2: Independence of Observations


# 1. Get the total number of observations (rows)
total_observations <- nrow(data)

# 2. Get the number of unique employees by their ID number
unique_employees <- length(unique(data$EmployeeNumber))

cat("Total number of observations:", total_observations, "\n")
cat("Number of unique employees:", unique_employees, "\n\n")

# 3. Logically check if each observation is unique
if (total_observations == unique_employees) {
  print("Assumption MET: Each row represents a unique employee, supporting the independence of observations.")
} else {
  print("Assumption VIOLATED: There are duplicate records in the dataset.")
}


# Verifying Chi-Square Assumption 3: Mutually Exclusive Cells

# 1. Create the contingency table
contingency_table <- table(data$Attrition, data$OverTime)

cat("Contingency Table:\n")
print(contingency_table)
cat("\n")

# 2. Sum the counts in all cells of the table
sum_of_cells <- sum(contingency_table)

# 3. Get the total number of observations (rows)
total_observations <- nrow(data)

cat("Sum of all cell counts in the table:", sum_of_cells, "\n")
cat("Total number of employees:", total_observations, "\n\n")

# 4. Logically check if every employee is in exactly one cell
if (sum_of_cells == total_observations) {
  print("Assumption MET: The cells are mutually exclusive. Each employee is counted exactly once.")
} else {
  print("Assumption VIOLATED: There is an issue with how the data is categorized.")
}


# Verifying Chi-Square Assumption 4: Expected Cell Counts

# 1. Perform the Chi-Square test and store the results
chi_test_results <- chisq.test(data$Attrition, data$OverTime)

# 2. The expected counts are stored in the 'expected' element of the results
expected_counts <- chi_test_results$expected

cat("Expected Cell Counts:\n")
print(expected_counts)
cat("\n")

# 3. Check the conditions
# Condition A: No cell has an expected value less than 1
no_cell_less_than_1 <- all(expected_counts >= 1)

# Condition B: At least 80% of cells have an expected value of 5 or greater
percent_cells_over_5 <- mean(expected_counts >= 5) * 100

cat("Are all expected cell counts >= 1?", no_cell_less_than_1, "\n")
cat("Percentage of cells with expected count >= 5:", percent_cells_over_5, "%\n\n")

# 4. Final conclusion
if (no_cell_less_than_1 && percent_cells_over_5 >= 80) {
  print("Assumption MET: The expected cell counts are large enough for the test to be reliable.")
} else {
  print("Assumption VIOLATED: The expected cell counts are too low. Consider using Fisher's Exact Test if the table is 2x2.")
}


# Final Chi-Square Test and Interpretation

# 1. Create the contingency table again for clarity
final_contingency_table <- table(data$Attrition, data$OverTime)

cat("Observed Counts (Contingency Table):\n")
print(final_contingency_table)
cat("\n")

# 2. Perform the Chi-Square test
# The 'chi_test_results' variable already holds this from our assumption check,
# but we'll run it again here for a complete final step.
final_chi_test <- chisq.test(final_contingency_table)

# 3. Print the detailed results of the test
print(final_chi_test)

# We reject the null hypothesis.


# Hypothesis 3
# Verifying Chi-Square Assumptions for Job Role vs. Attrition


# --- Assumption 1: Both Variables are Categorical ---
cat("--- Assumption 1: Data Types ---\n")
cat("Class of 'JobRole':", class(data$JobRole), "\n")
cat("Class of 'Attrition':", class(data$Attrition), "\n")
# Interpretation: 'JobRole' is character and 'Attrition' is factor. Both are categorical.
print("--> Assumption 1 is MET.")
cat("\n\n")


# --- Assumption 2: All Observations are Independent ---
cat("--- Assumption 2: Independence of Observations ---\n")
# As confirmed previously, each row is a unique employee, so this is met.
print("--> Assumption 2 is MET.")
cat("\n\n")


# --- Assumption 3: Mutually Exclusive Cells ---
cat("--- Assumption 3: Mutually Exclusive Cells ---\n")
jobrole_table <- table(data$JobRole, data$Attrition)
if (sum(jobrole_table) == nrow(data)) {
  print("--> Assumption 3 is MET. Each employee is in exactly one cell.")
} else {
  print("--> Assumption 3 is VIOLATED.")
}
cat("\n\n")


# --- Assumption 4: Expected Cell Counts ---
cat("--- Assumption 4: Expected Cell Counts ---\n")
# Run the test to get the expected values
jobrole_chi_test <- chisq.test(jobrole_table)

# Check the conditions
expected_counts <- jobrole_chi_test$expected
if (all(expected_counts >= 1) && mean(expected_counts >= 5) >= 0.8) {
  print("--> Assumption 4 is MET. Expected cell counts are large enough.")
  cat("All expected cell counts are >= 1\n")
  cat("Percentage of cells with expected count >= 5:", mean(expected_counts >= 5) * 100, "%\n")
} else {
  print("--> Assumption 4 is VIOLATED. The test may be unreliable.")
  # This warning is often shown when expected counts are low.
  # R will often give a warning message during the test itself in this case.
}

# --- Final Chi-Square Test for Job Role vs. Attrition ---

# 1. Create the contingency table for clarity
final_jobrole_table <- table(data$JobRole, data$Attrition)

cat("Observed Counts (Contingency Table for Job Role):\n")
print(final_jobrole_table)
cat("\n")

# 2. Perform the Chi-Square test
# The 'jobrole_chi_test' variable from the assumption check already holds this result,
# but we will print it again for the final interpretation.
print(jobrole_chi_test)

# We reject the null hypothesis.
