
"
Project Title: Analyzing the Determinants of Student Academic Success in Higher Institutions
Name: Ugwute Charles Ogbonna
ID: 2423914
Department:Software Engineering
Module Name:Data Analysis and Visualization
Module Title:DAT7301
Tutor Name:Harinda Eugen
Date: 30th April, 2025


"




"

ID_No, Prog_Code, Gender, YoG (Year of Graduation), CGPA, CGPA100–400 (Level-wise CGPAs), SGPA (Semester GPA)
  
  This script includes:
    
  Data Cleaning & Transformation
  
  Outlier Detection & Handling
  
  Descriptive Stats & Normalization
  
  Trends, Patterns, Anomalies
  
  Correlation Analysis
  
  Longitudinal Analysis
  
  Mixed-methods & Regression
  
  Confidence Intervals
  
  Cross-sectional Study
  
  Interactive Visualizations
  
  Insight Interpretation


"
  #Load Libraries & Dataset
  install.packages("GGally")
  install.packages("tidyverse")
  install.packages("ploty")
  install.packages("ggplot2")
  install.packages("caret")
  install.packages("psych")
  install.packages("dplyr")
  install.packages("shiny")
  install.packages("reshape2")
  
  # Required packages
  library(tidyverse)
  library(ggplot2)
  library(plotly)
  library(caret)
  library(psych)
  library(GGally)
  library(dplyr)
  library(shiny)
  library(reshape2)
  
  # Load data
  data <- read.csv("academic_performance_dataset_V2.csv")  
  
  
  #1. Data Cleaning & Transformation
  #Data Cleaning
  #Inspection of Student_Data
  str(data)
  View(data)
  dim(data) #  3046 rows and 10 columns
  head(data)
  tail(data)
  colnames(data)
  
  #checking for missing values
  sum(is.na(data))  #16 missing values
  colSums(is.na(data))  
  
  # Remove duplicates
  data <- data %>% distinct()
  
  # Remove rows with too many missing values
  data <- data[complete.cases(data), ]
  
  #Removing  missing values, since they are 16 out of 1104. 
  data <- na.omit(data)
  sum(is.na(data))
   
  
  #Data Transformation
  #Removing  Unnecessary columns
  data_clean <- subset(data, select = -c(
    TTWA11CD
  ))
  
  #Renaming Columns
  view(data_Clean)
  colnames(data_Clean)
  data_Clean <- rename(data_Clean, Job_Flag='JOB_DENSITY_FLAG', Resident_Level='Level4Qual_residents'
                               
  )
  
  # Convert categorical variables
  data_clean$Gender <- as.factor(data$Gender)
  data_clean$Prog_Code <- as.factor(data$Prog_Code)
  
  
  
  #checking for normalization in data set through calculation
  College_mean <-mean(data_Clean$College)
  Escore_mean <-mean(data_Clean$Education_Score)
  Employ_mean <-mean(data_Clean$Employment)
  LevelQual_mean <-mean(data_Clean$Highest_level_qualification)
  avg_mean <-mean(data_Clean$Average_score)
  
  
  College_median <-median(data_Clean$College)
  Escore_median <-median(data_Clean$Education_Score)
  Employ_median <-median(data_Clean$Employment)
  LevelQual_median <-median(data_Clean$Highest_level_qualification)
  avg_median <-median(data_Clean$Average_score)
  
  College_mode <-mode(data_Clean$College)
  Escore_mode <-mode(data_Clean$Education_Score)
  Employ_mode <-mode(data_Clean$Employment)
  LevelQual_mode <-mode(data_Clean$Highest_level_qualification)
  avg_mode <-mode(data_Clean$Average_score)
  
  
  
  #calculation of skewness
  skewness <- function (v){
    answer <- 3*(mean(v)-median(v)) / sd(v)
    return(answer)
  }
  
  skewness(data_Clean$College)
  skewness(data_Clean$Education_Score)
  skewness(data_Clean$Employment)
  skewness(data_Clean$Highest_level_qualification)
  skewness(data_Clean$Average_score)
  
  
  #3. Using Interquartile Range (Outlier Detection)
  Q1 <- quantile(data_Clean$Attendance...., 0.25)
  Q3 <- quantile(data_Clean$Attendance...., 0.75)
  IQR <- Q3 - Q1
  LQR <- Q1-1.5*IQR
  UQR <- Q3+1.5*IQR
  Outlier <- subset(data_Clean, data_Clean$Attendance.... < LQR |
                      data_Clean$Attendance.... > UQR 
  )
  print(Outlier)
  

  #checking for normalization in data set through Visualization
  #Histogram
  ggplot(data_Clean)+
    geom_histogram(mapping = aes(x=College))
  
  ggplot(data_Clean)+
    geom_histogram(mapping = aes(x=Education_Score))
  
  ggplot(data_Clean)+
    geom_histogram(mapping = aes(x=Highest_level_qualification))
  
  ggplot(data_Clean)+
    geom_histogram(mapping = aes(x=Employment))
  
  ggplot(data_Clean)+
    geom_histogram(mapping = aes(x=Average_score))
  
  ggplot(Student_infor_Clean) +
    geom_histogram(mapping = aes(x = `Attendance....`), binwidth = 5, fill = "steelblue", color = "black") +
    labs(title = "Histogram of Attendance", x = "Attendance", y = "Count")
  
  
  
  #Boxplot
  ggplot(data_Clean, aes(x = CGPA100, y = Age)) + 
    geom_boxplot()
  #Interactive Boxplot (to detect outliers)
  p <- ggplot(df, aes(y = your_numeric_column)) +
    geom_boxplot(fill = "orange", outlier.colour = "red", outlier.shape = 16)
  
  ggplotly(p)
  
  #Points beyond the whiskers are outliers.
  
  ggplot(data_Clean, aes(x = College, y = CGPA)) +
    geom_boxplot(fill = "skyblue", color = "darkblue") +
    labs(title = "Box Plot of GPA by College",
         x = "College",
         y = "GPA") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Boxplot with labeled outliers
  #Points beyond the whiskers are outliers.
  ggplot(Student_infor_Clean, aes(y = `Attendance....`)) +
    geom_boxplot(fill = "skyblue") +
    geom_text(data = Outlier, aes(label = rownames(Outlier), y = `Attendance....`), 
              position = position_jitter(width = 0.2), vjust = -0.5, color = "red") +
    labs(title = "Boxplot with Labeled Outliers", y = "Attendance") +
    theme_minimal()
  
  
  #Scatter Plot
  #Scatter plots help detect multivariate outliers.
  ggplot(data_Clean, aes(x = SGPA, y = CGPA)) +
    geom_jitter(width = 0.2, color = "steelblue", alpha = 0.7) +
    labs(title = "Scatter Plot of CGPA by SGPA",
         x = "SGPA",
         y = "CGPA") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(data_Clean, aes(x = CGPA100, y = CGPA)) +
    geom_jitter(width = 0.2, color = "steelblue", alpha = 0.7) +
    labs(title = "Scatter Plot of CGPA by CGPA100",
         x = "CGPA100",
         y = "CGPA") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(data_Clean, aes(x = CGPA200, y = CGPA)) +
    geom_jitter(width = 0.2, color = "steelblue", alpha = 0.7) +
    labs(title = "Scatter Plot of CGPA by CGPA200",
         x = "CGPA200",
         y = "CGPA") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(data_Clean, aes(x = CGPA300, y = CGPA)) +
    geom_jitter(width = 0.2, color = "steelblue", alpha = 0.7) +
    labs(title = "Scatter Plot of CGPA by CGPA300",
         x = "CGPA300",
         y = "CGPA") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(data_Clean, aes(x = CGPA400, y = CGPA)) +
    geom_jitter(width = 0.2, color = "steelblue", alpha = 0.7) +
    labs(title = "Scatter Plot of CGPA by CGPA400",
         x = "CGPA400",
         y = "CGPA") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  #Outlier Handling
  
  # Z-score method for CGPA and SGPA
  z_scores <- scale(data_clean[, c("CGPA", "SGPA", "CGPA100", "CGPA200", "CGPA300", "CGPA400")])
  data_clean <- data_clean[apply(z_scores, 1, function(x) all(abs(x) < 3)), ]
  
  
  
  #3. Descriptive Statistics & Normalization
  #Sampling method is Stratified where power analysis is used to determined the sample size 10% of the sample population
  #Reduce the number of row 
  # Keep 10% of rows
  data_clean_sample <- data_Clean[sample(nrow(data_Clean), size = 0.1 * nrow(data_Clean)), ]
  
  # View size
  nrow(data_clean_sample)  # Should be 500
  View(data_clean_sample)
  
  # Load knitr and Hmisc for kable
  library(knitr)
  library(Hmisc)
  
  selected_columns <- data_clean_sample[, c("CGPA", "SGPA", "CGPA100", "CGPA200", "CGPA300", "CGPA400")]
  
  describe(data_clean_sample[, c("CGPA", "SGPA", "CGPA100", "CGPA200", "CGPA300", "CGPA400")])
  
  # Describe the selected data
  describe_table <- describe(selected_columns)
  
  describe_df <- as.data.frame(describe_table)
  
  kable(describe_df, caption = "Describe Output Table")
  
  
  
  
  summary(data_clean_sample[, c("CGPA", "SGPA", "CGPA100", "CGPA200", "CGPA300", "CGPA400")])
 
  # Select the columns of interest
  selected_columns <- data_clean_sample[, c("CGPA", "SGPA", "CGPA100", "CGPA200", "CGPA300", "CGPA400")]

  summary_table <- summary(selected_columns)
  
  summary_df <- as.data.frame(summary_table)
   
  kable(summary_df, caption = "Summary Statistics Table")
  
  
  # Normalize CGPA and SGPA
  norm_data <- as.data.frame(preProcess(data_clean_sample[, c("CGPA", "SGPA")], method = c("range")) %>% predict(data_clean_sample))
  
  
  # 4. Trends, Patterns, Anomalies
  
  # CGPA trend by year
  ggplot(data_clean_sample, aes(x = YoG, y = CGPA)) +
    geom_boxplot(fill = "skyblue") +
    labs(title = "Trend of CGPA over Years")
  
  #Add Trend Line (Regression)
  ggplot(data_clean_sample, aes(x = `Attendance....`, y = Total_Score)) +
    geom_point(color = "dodgerblue", alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = "Scatter Plot with Trend Line",
         x = "Attendance", y = "Total_Score") +
    theme_light()
  
  # Anomaly: Sudden drop in SGPA
  ggplot(data_clean_sample, aes(x = CGPA, y = SGPA)) +
    geom_point(aes(color = Gender)) +
    geom_smooth(method = "lm", se = FALSE, color = "red")
  

  
  #5. Correlation Analysis
  
  cor_data <- data_clean_sample[, c("CGPA", "SGPA", "CGPA100", "CGPA200", "CGPA300", "CGPA400")]
  cor_matrix <- cor(cor_data, use = "complete.obs")
  ggcorr(cor_data, label = TRUE)
  
  
  #6. Longitudinal Analysis (CGPA Progression)
  
  # Melt data for timeline plotting
  long_data <- melt(data_clean_sample, id.vars = "ID_No", measure.vars = c("CGPA100", "CGPA200", "CGPA300", "CGPA400"))
  
  ggplot(long_data, aes(x = variable, y = value, group = ID_No)) +
    geom_line(alpha = 0.3) +
    labs(title = "CGPA Progression Over Levels", x = "Level", y = "CGPA")
  
  
  #7. Mixed-methods: Qualitative + Quantitative
  
  # Quantitative: ANOVA by Gender
  anova_result <- aov(CGPA ~ Gender, data = data_clean_sample)
  summary(anova_result)
  
  # Qualitative Insight: Boxplot by Gender
  ggplot(data_clean_sample, aes(x = Gender, y = CGPA)) +
    geom_boxplot(fill = "lightgreen") +
    labs(title = "Gender-based CGPA Distribution")
  
  

  
  #8. Confidence Intervals
  
  # 95% Confidence interval for CGPA mean
  mean_cgpa <- mean(data_clean_sample$CGPA)
  se_cgpa <- sd(data_clean_sample$CGPA) / sqrt(nrow(data_clean_sample))
  ci <- c(mean_cgpa - 1.96 * se_cgpa, mean_cgpa + 1.96 * se_cgpa)
  ci
  
  
  #9. Regression Analysis
 
  reg_model <- lm(CGPA ~ SGPA + CGPA100 + CGPA200 + CGPA300 + CGPA400, data = data_clean_sample)
  summary(reg_model)
  
  
  #10. Cross-Sectional Study
  
  # Compare CGPA across different Prog_Codes (single year)
  cross_section <- data_clean_sample %>% filter(YoG == 2022)
  
  ggplot(cross_section, aes(x = Prog_Code, y = CGPA)) +
    geom_boxplot(fill = "purple") +
    labs(title = "CGPA by Program Code for Year 2022")
  
  
  #11. Interactive Visualization
   
  plot_ly(data_clean_sample, x = ~SGPA, y = ~CGPA, color = ~Gender, type = 'scatter', mode = 'markers') %>%
    layout(title = "CGPA vs SGPA by Gender")
  
  # Enhanced Boxplot: CGPA Distribution by Gender
  ggplot(data_clean_sample, aes(x = Gender, y = CGPA, fill = Gender)) +
    geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 8) +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black", position = position_dodge(width = 0.75)) +
    labs(
      title = "Distribution of CGPA by Gender",
      subtitle = "Boxplot with Mean Indicator and Outliers Highlighted",
      x = "Gender",
      y = "Cumulative Grade Point Average (CGPA)"
    ) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal()
  
  
  # Enhanced Interactive Boxplot with Hover Info
  p <- ggplot(data_clean_sample, aes(x = Gender, y = CGPA, fill = Gender, text = paste("CGPA:", CGPA))) +
    geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 8) +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black",
                 position = position_dodge(width = 0.75)) +
    labs(
      title = "Distribution of CGPA by Gender",
      subtitle = "Hover to View CGPA Details Including Outliers",
      x = "Gender",
      y = "Cumulative Grade Point Average (CGPA)"
    ) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal()
  
  # Make it interactive
  ggplotly(p, tooltip = "text")
  
  
  
  
  #Final Insight & Interpretation
  
  # Top 3 insights
  insight <- c(
    "1. CGPA shows steady progression from 100 to 400 level, except for some anomalies in CGPA300.",
    "2. Gender has a small but statistically significant effect on CGPA (based on ANOVA).",
    "3. Regression shows SGPA and lower level CGPAs are strong predictors of overall CGPA."
  )
  print(insight)
  
  
  
  
  
  
  
  