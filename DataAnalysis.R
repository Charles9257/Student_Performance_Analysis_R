

#Library Installation
install.packages(c("ggplot2", "dplyr", "tidyr", "GGally", "corrplot", "reshape2", "plotly"))

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)
library(corrplot)
library(reshape2)
library(dplyr)
library(stringr)
library(plotly)

# Load the dataset
df <- read.csv("academic_performance_dataset_V2.csv")


# Data Cleaning & Preparation


# Read the dataset
df <- read.csv("students_performance.csv", stringsAsFactors = FALSE)

# -----------------------------
# 1. View structure of dataset
# -----------------------------
str(df) #3046 objects of 10 variables with unique year of 2010 to 2014, min CGPA = 1.520, max = 4.990, CGPA100 MIN=1.570, MAX = 5.00, CGPA200, MIN=1.170, MAX=5.00, CGPA300, MIN=0.630, MAX=5.00, CGPA400,MIN=0.000,MAX=5.000, AND FOR SGPA,1,46,MEAN=3.12,MEDIAN=3.06,
summary(df)
View(df)

# -----------------------------
# 2. Rename columns
# -----------------------------
# Make sure all column names are consistent and syntactically valid
colnames(df) <- make.names(colnames(df))
colnames(df)
# Optional renaming for clarity
df <- df %>%
  rename(
    StudentID = ID_No,
    ProgramCode = Prog_Code,
    Gender = Gender,
    YearOfGraduation = YoG,
    SGPA = SGPA,
    CGPA = CGPA,
    CGPA100 = CGPA100,
    CGPA200 = CGPA200,
    CGPA300 = CGPA300,
    CGPA400 = CGPA400
  )

# -----------------------------
# 3. Handle missing values
# -----------------------------

# Check number of missing values
sum(is.na(df)) # no missing value
colSums(is.na(df))

 
 
#Duplicate check
duplicates <- df[duplicated(df), ]

# View them
print(duplicates)

# Count of duplicate rows
n_duplicates <- sum(duplicated(df))
cat("Number of duplicate rows:", n_duplicates, "\n")



# ------------------------------------------
# 4. Standardize / Normalizing  text fields
# -------------------------------------------

# Remove leading/trailing whitespaces
df$Gender <- str_trim(df$Gender)
df$ProgramCode <- str_trim(df$ProgramCode)

# Convert gender to title case (e.g., "male" -> "Male")
df$Gender <- str_to_title(df$Gender)

ggplot(df)+
  geom_histogram(mapping = aes(x=CGPA))

ggplot(df) +
  geom_histogram(mapping = aes(x = CGPA100), fill = "steelblue", color = "black") +
  labs(title = "Histogram of CGPA100 for Normality Check")

ggplot(df) +
  geom_histogram(mapping = aes(x = CGPA200), fill = "steelblue", color = "black") +
  labs(title = "Histogram of CGPA200 for Normality Check")

ggplot(df) +
  geom_histogram(mapping = aes(x = CGPA300), fill = "steelblue", color = "black") +
  labs(title = "Histogram of CGPA300 for Normality Check")

ggplot(df) +
  geom_histogram(mapping = aes(x = CGPA400), fill = "steelblue", color = "black") +
  labs(title = "Histogram of CGPA400 for Normality Check")

ggplot(df) +
  geom_histogram(mapping = aes(x = SGPA), fill = "steelblue", color = "black") +
  labs(title = "Histogram of SGPA for Normality Check")

# -----------------------------------
# 5. Convert types/ Transformation
# ----------------------------------

# Ensure correct data types
df$Gender <- as.factor(df$Gender)
df$YearOfGraduation <- as.factor(df$YearOfGraduation)

# -------------------------------------
# 6. Check for outliers or invalid data
# --------------------------------------

# GPA should usually be between 0 and 5 
summary(df[, c("SGPA", "CGPA", "CGPA100", "CGPA200", "CGPA300", "CGPA400")])

# Remove rows with GPAs outside valid range
clean_data <- clean_data %>%
  filter(CGPA <= 5 & SGPA <= 5 & CGPA100 <= 5 & CGPA200 <= 5 & CGPA300 <= 5 & CGPA400 <= 5)

#calculation of skewness
skewness <- function (v){
  answer <- 3*(mean(v)-median(v)) / sd(v)
  return(answer)
}

skewness(df$CGPA)


#3. Using Interquartile Range (Outlier Detection)
Q1 <- quantile(df$CGPA, 0.25)
Q3 <- quantile(df$CGPA, 0.75)
IQR <- Q3 - Q1
LQR <- Q1-1.5*IQR
UQR <- Q3+1.5*IQR
Outlier <- subset(df, df$CGPA < LQR |
                    df$CGPA > UQR 
)
print(Outlier)

#Boxplot
ggplot(df, aes(x = CGPA, y = SGPA)) + 
  geom_boxplot()

install.packages("plotly")
library(plotly)
#Interactive Boxplot (to detect outliers)
p <- ggplot(df, aes(y = CGPA)) +
  geom_boxplot(fill = "orange", outlier.colour = "red", outlier.shape = 16)

ggplotly(p)

#Points beyond the whiskers are outliers.

ggplot(df, aes(x = SGPA, y = CGPA)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Box Plot of CGPA by SGPA",
       x = "SGPA",
       y = "CGPA") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot with labeled outliers
#Points beyond the whiskers are outliers.
ggplot(df, aes(y = CGPA)) +
  geom_boxplot(fill = "skyblue") +
  geom_text(data = Outlier, aes(label = rownames(Outlier), y = CGPA), 
            position = position_jitter(width = 0.2), vjust = -0.5, color = "red") +
  labs(title = "Boxplot with Labeled Outliers", y = "CGPA") +
  theme_minimal()

#Using 10 percent of the population sample
# Take a 10% sample of the data
sampled_data <- df %>% sample_frac(0.10)

# View the sample
head(sampled_data)
View(sampled_data)

# Optional: write to a new CSV
write.csv(sampled_data, "sampled_10_percent.csv", row.names = FALSE)


# -----------------------------
# Ready for Analysis
# -----------------------------

# Use clean_data for all downstream analysis
df_clean <- sampled_data




#Data Analysis and Visualization

# Convert Gender and YearOfGraduation to factors
data$Gender <- as.factor(data$Gender)
data$YearOfGraduation <- as.factor(data$YearOfGraduation)

# ---------------------------------------------
# 1. Cross-Sectional Analysis
# ---------------------------------------------

# Average CGPA by Gender
ggplot(sampled_data, aes(x = Gender, y = CGPA, fill = Gender)) +
  geom_boxplot() +
  labs(title = "CGPA Distribution by Gender", y = "CGPA") +
  theme_minimal()

# Average CGPA by Year of Graduation
ggplot(sampled_data, aes(x = YearOfGraduation, y = CGPA, fill = YearOfGraduation)) +
  geom_boxplot() +
  labs(title = "CGPA by Year of Graduation", y = "CGPA") +
  theme_minimal()

# ---------------------------------------------
# 2. Longitudinal Analysis
# ---------------------------------------------

# Melt GPA columns to long format
long_gpa <- sampled_data %>%
  select(ProgramCode, Gender, CGPA100, CGPA200, CGPA300, CGPA400) %>%
  pivot_longer(cols = starts_with("CGPA"), names_to = "Level", values_to = "GPA")

# GPA progression by level
ggplot(long_gpa, aes(x = Level, y = GPA)) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "steelblue", size = 1.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "Average GPA Progression by Level", x = "Level", y = "GPA") +
  theme_minimal()

# Individual GPA trajectories (sample of 10)
sample_ids <- sample(unique(sampled_data$StudentID), 10)
sampled_data <- sampled_data %>%
  filter(StudentID %in% sample_ids) %>%
  pivot_longer(cols = starts_with("CGPA"), names_to = "Level", values_to = "GPA")

ggplot(sampled_data, aes(x = Level, y = GPA, group = StudentID, color = StudentID)) +
  geom_line(size = 1) +
  labs(title = "GPA Trajectories for Sample Students", x = "Level", y = "GPA") +
  theme_minimal()

# ---------------------------------------------
# 3. Correlation Analysis
# ---------------------------------------------

# Select numeric columns for correlation
numeric_data <- df %>%
  select(SGPA, CGPA100, CGPA200, CGPA300, CGPA400, CGPA)

# Compute correlation matrix
cor_matrix <- round(cor(numeric_data, use = "complete.obs"), 2)
cor_matrix
# Display heatmap of correlation matrix
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black",
         tl.cex = 0.8, number.cex = 0.8, title = "Correlation Heatmap", mar = c(0,0,1,0))

# Optional pairwise scatterplots
ggpairs(numeric_data)

cor(numeric_data$CGPA, numeric_data$SGPA, use = "complete.obs")
# ---------------------------------------------
# 4. Summary Table by Gender
# ---------------------------------------------

summary_stats <- df %>%
  group_by(Gender) %>%
  summarise(
    Avg_CGPA = mean(CGPA, na.rm = TRUE),
    Avg_SGPA = mean(SGPA, na.rm = TRUE),
    Avg_CGPA100 = mean(CGPA100, na.rm = TRUE),
    Avg_CGPA400 = mean(CGPA400, na.rm = TRUE),
    Count = n()
  )
print(summary_stats)




#Visualization

# Load necessary libraries
library(ggplot2)
library(plotly)
library(dplyr)

# Optional: Check and show top few records
head(Student_data_Clean)

# Count occurrences per College for anomaly detection
college_counts <- df %>%
  group_by(CGPA100) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Mark anomalies (example: colleges with count below a threshold, say < 5)
college_counts <- college_counts %>%
  mutate(Anomaly = ifelse(Count < 5, "Yes", "No"))

# Merge with original data to mark anomalies
df <- df %>%
  left_join(college_counts, by = "CGPA100")

# Plot with ggplot
p <- ggplot(df, aes(x = reorder(CGPA100, Count), fill = Anomaly)) +
  geom_bar() +
  coord_flip() +  # Flip for better readability if many colleges
  labs(title = "Distribution of Students by CGPA100",
       x = "CGPA",
       y = "Count",
       fill = "Anomaly") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# Convert ggplot to interactive plotly
ggplotly(p)





#Interpretation using p-value and Rsquare
t.test(CGPA ~ Gender, data = df, var.equal = TRUE)

#RSquared
model <- lm(CGPA ~ Gender, data = df)
summary(model)$r.squared


#Bar Plot with 95% Confidence Intervals

# Calculate means and CI
summary_stats <- df %>%
  group_by(Gender) %>%
  summarise(
    mean_cgpa = mean(CGPA, na.rm = TRUE),
    sd_cgpa = sd(CGPA, na.rm = TRUE),
    n = n(),
    se = sd_cgpa / sqrt(n),
    ci_low = mean_cgpa - 1.96 * se,
    ci_high = mean_cgpa + 1.96 * se
  )


# Plot
ggplot(summary_stats, aes(x = Gender, y = mean_cgpa, fill = Gender)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  labs(title = "Mean CGPA by Gender with 95% Confidence Intervals",
       x = "Gender", y = "Mean CGPA") +
  theme_minimal()



#Point Plot for Multiple GPA Measures

# Reshape to long format
gpa_long <- df %>%
  select(Gender, CGPA, SGPA, CGPA100, CGPA400) %>%
  pivot_longer(-Gender, names_to = "Measure", values_to = "Score")

# Summary stats
summary_long <- gpa_long %>%
  group_by(Gender, Measure) %>%
  summarise(
    mean_score = mean(Score, na.rm = TRUE),
    se = sd(Score, na.rm = TRUE) / sqrt(n()),
    ci_low = mean_score - 1.96 * se,
    ci_high = mean_score + 1.96 * se,
    .groups = "drop"
  )

# Plot
ggplot(summary_long, aes(x = Measure, y = mean_score, color = Gender, group = Gender)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, position = position_dodge(0.3)) +
  labs(title = "GPA Measures by Gender with 95% Confidence Intervals",
       x = "GPA Measure", y = "Mean Score") +
  theme_minimal()


ggplot(df, aes(x = Prog_Code, fill = Prog_Code)) +
  geom_bar() +
  theme_minimal() +
  ggtitle("Number of Students by Program")

ggplot(df, aes(x = YoG, fill = "green")) +
  geom_bar() +
  theme_minimal() +
  ggtitle("Average CGPA by graduation year")


ggplot(df, aes(x = YoG, y = CGPA)) +
  geom_line(stat = "summary", fun = mean, color = "red", size = 1.5) +
  theme_minimal() +
  ggtitle("Average CGPA Over Graduation Years")

ggplot(df, aes(x = CGPA, y = CGPA100, color = Gender)) +
  geom_point() +
  theme_minimal() +
  ggtitle("CGPA vs CGPA100") 


ggplot(df, aes(x = CGPA, y = CGPA400, color = Gender)) +
  geom_point() +
  theme_minimal() +
  ggtitle("CGPA vs CGPA400") 

ggplot(df, aes(x = YoG, y = CGPA)) +
  geom_line(stat = "summary", fun = mean, color = "red", size = 1.5) +
  theme_minimal() +
  ggtitle("Average CGPA Over Graduation Years")


anova_model <- aov(CGPA ~ Prog_Code, data = df)
summary(anova_model)







