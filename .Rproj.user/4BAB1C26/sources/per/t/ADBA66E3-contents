---
  title: "Academic Performance Analysis Report"
author: "Your Name"
date: "`r Sys.Date()`"
output: pdf_document
---
  
  ```{r setup, include=FALSE}
library(tidyverse)
library(ggplot2)
library(psych)
library(GGally)
library(caret)
library(reshape2)
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction
This report provides a comprehensive analysis of academic performance data using statistical and visual exploration techniques.

## Load and Clean Data
```{r}
data <- read.csv("academic_data.csv")
data <- data %>% distinct() %>% filter(complete.cases(.))
data$Gender <- as.factor(data$Gender)
data$Prog_Code <- as.factor(data$Prog_Code)
```

## Descriptive Statistics
```{r}
describe(data[, c("CGPA", "SGPA", "CGPA100", "CGPA200", "CGPA300", "CGPA400")])
```

## Data Visualization
### CGPA Distribution by Gender
```{r}
ggplot(data, aes(x = Gender, y = CGPA, fill = Gender)) +
  geom_boxplot() +
  labs(title = "CGPA Distribution by Gender")
```

### CGPA Progression
```{r}
long_data <- melt(data, id.vars = "ID_No", measure.vars = c("CGPA100", "CGPA200", "CGPA300", "CGPA400"))
ggplot(long_data, aes(x = variable, y = value, group = ID_No)) +
  geom_line(alpha = 0.3) +
  labs(title = "CGPA Progression Across Levels")
```

## Outlier Detection
```{r}
z_scores <- scale(data[, c("CGPA", "SGPA")])
outliers <- data[apply(z_scores, 1, function(x) any(abs(x) > 3)), ]
knitr::kable(head(outliers))
```

## Correlation Analysis
```{r}
cor_data <- data[, c("CGPA", "SGPA", "CGPA100", "CGPA200", "CGPA300", "CGPA400")]
ggally::ggcorr(cor_data, label = TRUE)
```

## Regression Analysis
```{r}
model <- lm(CGPA ~ SGPA + CGPA100 + CGPA200 + CGPA300 + CGPA400, data = data)
summary(model)
```

## Confidence Interval for CGPA Mean
```{r}
mean_cgpa <- mean(data$CGPA)
se_cgpa <- sd(data$CGPA) / sqrt(nrow(data))
ci <- c(mean_cgpa - 1.96 * se_cgpa, mean_cgpa + 1.96 * se_cgpa)
ci
```

## ANOVA - Gender Effect
```{r}
anova_result <- aov(CGPA ~ Gender, data = data)
summary(anova_result)
```

## Conclusion
This report explored key patterns in academic performance, highlighting the relationship between level-wise GPAs and final CGPA, gender effects, and outlier behaviors. Further studies can investigate curriculum alignment and predictive models for academic advising.
