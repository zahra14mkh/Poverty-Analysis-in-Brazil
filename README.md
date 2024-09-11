# Discrete data analysis of poverty in Brazil

## Overview
This R script analyzes the relationship between various socio-economic factors and poverty in Brazil using contingency tables, logistic regression models, and statistical tests. The dataset, `poverty_brazil.xlsx`, contains variables such as gender, age, education, work status, and ethnicity, which are used to examine the likelihood of poverty.

## Dependencies
Ensure you have the following libraries installed:
```R
install.packages(c("readxl", "epitools", "vcd", "lmtest", "jtools", "dplyr", "questionr", "DescTools", "ordinal", "fastDummies"))
```
These libraries are used for:
- **readxl**: Reading Excel files.
- **epitools**: Calculating odds ratios and risk ratios.
- **vcd**: Measures of association like the Phi coefficient.
- **lmtest**: Likelihood ratio tests.
- **jtools**: Summarizing logistic regression models.
- **fastDummies**: Creating dummy variables for categorical variables.
- **questionr, DescTools**: Advanced contingency table and odds ratio analysis.

## Key Analysis Steps

### 1. Data Import and Contingency Tables
- **Data Import**: Reads the `poverty_brazil.xlsx` file into a data frame `df`.
- **Contingency Tables**: For each independent variable (e.g., gender, age, ethnicity), a contingency table is printed against the poverty status.
```R
print(table(df[[var]], df$poverty))
```

### 2. Statistical Tests
- **Chi-Square Test**: Tests for independence between variables and poverty status.
- **Fisher's Exact Test**: Used for small sample sizes.
- **Odds Ratio and Risk Ratio**: Measures the strength of association between variables like ethnicity and poverty.
- **Proportions Test**: Compares the proportion of non-white and white people in poverty.
  
### 3. Logistic Regression Models
- **Full Model**: Logistic regression to predict poverty using all available variables.
- **Reduced Model**: Compares a model excluding a variable to the full model to see if the variable significantly improves the model.
- **Interaction Effects**: Explores if gender and ethnicity interact in predicting poverty.
- **Confidence Intervals**: Provides a 95% confidence interval for the odds ratios.

### 4. Likelihood Ratio Tests
- **LR Tests**: Compares the full and reduced models to assess the importance of specific variables like `woman`.
```R
anova(full_model, reduced_model, test = "LRT")
```

### 5. Measures of Association
- **Phi Coefficient**: Measures the strength of the association between two categorical variables.
```R
phi_coefficient <- association_stats$phi
```

### 6. Cumulative Logit Model
A complementary cumulative log-log model is fitted to analyze ordinal responses related to poverty status.

## Interpretation of Results
- **Odds Ratios**: Non-white individuals are approximately 2.59 times more likely to be in poverty.
- **Risk Ratios**: The risk of poverty is 1.45 times higher for non-white individuals.
- **Chi-Square & Fisher's Test**: Strong evidence against the null hypothesis of independence between variables like ethnicity and poverty.
- **Logistic Regression**: Significant predictors of poverty include gender, ethnicity, education, and living in urban areas.

## How to Run the Code
1. Place the `poverty_brazil.xlsx` file in the working directory.
2. Run the R script.
3. Check the console for contingency tables, statistical test results, and model summaries.

## Outputs
- **Contingency tables**: Summarized data between variables and poverty status.
- **Chi-Square and Fisher Test Results**: Indicates statistical significance of associations.
- **Odds and Risk Ratios**: Quantifies the likelihood of poverty for different groups.
- **Logistic Regression Summary**: Explains the influence of factors on poverty.
