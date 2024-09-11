library("readxl")
df <- read_excel("poverty_brazil.xlsx")
# Contingency tables
independent_vars <- c("woman", "age", "education", "work", "metropolitan_area", "non_white", "urban", "work_permit")
for (var in independent_vars) {
  cat("Contingency table for", var, "vs poverty:\n")
  print(table(df[[var]], df$poverty))
  cat("\n")
}
attach(df)
#choose one of them
contingency_table <- table(df$poverty, df$non_white)

# Print the contingency table
print(contingency_table)

# Add margins to the contingency table
ccontingency_table <- addmargins(contingency_table)
print(ccontingency_table)

# Perform Cross Table analysis
TABLE <- CrossTable(contingency_table, expected=TRUE, chisq=TRUE, prop.chisq=FALSE, prop.c=TRUE, prop.r=TRUE, fisher=TRUE)
print(TABLE)
#Summary of Findings:Significant Association: Both Pearson's chi-squared test and Fisher's exact test show a highly significant association between the two variables.
#Odds Ratio: The odds ratio indicates a substantial difference in the likelihood of being in the response category between the two groups.
#Proportions: The proportions in the contingency table help understand the distribution and independence of the variables.
#Overall, the analysis provides strong evidence of a significant association between the two categorical variables, with a clear quantification of the strength of this association through the odds ratio.


#Perform the Pearson Chi-Square test
chi_square_test <- chisq.test(contingency_table)
# Print the result of the Chi-Square test
print(chi_square_test)
#very strong evidence against the null hypothesis.null hypothesis:indepedance
lrt_result <- chisq.test(contingency_table, simulate.p.value = TRUE)
print(lrt_result)
#If the p-value is less than the significance level (0.05), you reject the null hypothesis.
#if the p-value is greater than the significance level, you fail to reject the null hypothesis.
#In our case, the p-value is 0.0004998, which is much smaller than 0.05.

#confidence interval
library(epitools)
contingency_table <- table(df$poverty, df$non_white)
print(contingency_table)
pearson_test_result <- chisq.test(contingency_table)
print(pearson_test_result)
# Calculate the odds ratio and its confidence interval
odds_ratio_result <- oddsratio(contingency_table)
print(odds_ratio_result$measure)
#Odds Ratio Estimate: 2.5925 Interpretation: Non-white individuals are approximately 2.59 times more likely to be in poverty compared to non-non-white (white) individuals.
# You can also calculate the risk ratio 
risk_ratio_result <- riskratio(contingency_table)
print(risk_ratio_result$measure)
#Risk Ratio Estimate: 1.4490 Interpretation: The risk of poverty is approximately 1.45 times higher for non-white individuals compared to non-non-white (white) individuals.


#difference of proportions
contingency_table <- table(df$poverty, df$non_white)
print(contingency_table)
# Extract counts for the two proportions
count_poverty_non_white <- contingency_table[2, 2]  # Non-White and in Poverty
count_non_poverty_non_white <- contingency_table[1, 2]  # Non-White not in Poverty
count_poverty <- sum(contingency_table[2, ])  # Total in Poverty
count_non_poverty <- sum(contingency_table[1, ])  # Total not in Poverty
# Create vectors of successes and trials
successes <- c(count_poverty_non_white, count_non_poverty_non_white)
trials <- c(count_poverty, count_non_poverty)
#Successes: The number of occurrences of the event of interest in each group.
#Trials: The total number of observations or trials in each group.
# Perform the two-proportion test
prop_test_result <- prop.test(successes, trials)
print(prop_test_result)
#Test Statistic
#X-squared = 721.72: This is the chi-squared test statistic. It measures the difference between the observed and expected proportions under the null hypothesis.
#3. Degrees of Freedom
#df = 1: The degrees of freedom for this test. For a two-sample proportion test, the degrees of freedom are typically 1.
#4. P-value
#p-value < 2.2e-16: The p-value is extremely small, indicating that the observed difference in proportions is highly unlikely to have occurred by chance. This strong evidence suggests rejecting the null hypothesis that the proportions are equal.
#5. Alternative Hypothesis
#alternative hypothesis: two.sided: The test is two-sided, meaning it checks for differences in both directions (whether one proportion is greater or less than the other).
#6. Confidence Interval
#95 percent confidence interval: 0.2073446 to 0.2377078: This interval estimates the range within which the true difference in proportions lies with 95% confidence. Since the interval does not include 0 and is entirely positive, it indicates a significant difference between the two proportions, with the first group having a higher proportion than the second group.
#7. Sample Estimates
#prop 1 = 0.7180805: The proportion of successes in the first group.
#prop 2 = 0.4955543: The proportion of successes in the second group.


# Measures of Association (e.g., Phi Coefficient)(teta)
# Load the vcd package
library(vcd)
contingency_table <- table(df$poverty, df$non_white)
print(contingency_table)
# Compute association statistics
association_stats <- assocstats(contingency_table)
print(association_stats)
# Phi coefficient for 2x2 tables
phi_coefficient <- association_stats$phi
print(paste("Phi Coefficient:", phi_coefficient))

# Perform Fisher's Exact Test
contingency_table <- table(df$poverty, df$non_white)
print(contingency_table)
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

#contingency 3
library(dplyr)
library(questionr)
library(DescTools)
# Create the contingency table with poverty as the dependent variable and woman and non_white as independent variables
TabPoverty = table(df$woman, df$non_white, df$poverty)
dimnames(TabPoverty) = list(
  Woman = c("No", "Yes"),
  NonWhite = c("No", "Yes"),
  Poverty = c("No", "Yes")
)

print(TabPoverty)

# Two-way tables for different combinations
TabWomanNo = TabPoverty["No", , ]
TabWomanYes = TabPoverty["Yes", , ]
TabNonWhiteNo = TabPoverty[, "No", ]
TabNonWhiteYes = TabPoverty[, "Yes", ]

# Combined table
TabCombined = TabWomanNo + TabWomanYes

# Odds ratios
oddsratio_combined = (TabCombined[1, 1] * TabCombined[2, 2]) / (TabCombined[1, 2] * TabCombined[2, 1])
print(oddsratio_combined)

odds.ratio(TabCombined)

oddsratio_woman_no = (TabWomanNo[1, 1] * TabWomanNo[2, 2]) / (TabWomanNo[1, 2] * TabWomanNo[2, 1])
print(oddsratio_woman_no)
odds.ratio(TabWomanNo)

oddsratio_woman_yes = (TabWomanYes[1, 1] * TabWomanYes[2, 2]) / (TabWomanYes[1, 2] * TabWomanYes[2, 1])
print(oddsratio_woman_yes)
odds.ratio(TabWomanYes)

oddsratio_nonwhite_no = (TabNonWhiteNo[1, 1] * TabNonWhiteNo[2, 2]) / (TabNonWhiteNo[1, 2] * TabNonWhiteNo[2, 1])
print(oddsratio_nonwhite_no)
odds.ratio(TabNonWhiteNo)

oddsratio_nonwhite_yes = (TabNonWhiteYes[1, 1] * TabNonWhiteYes[2, 2]) / (TabNonWhiteYes[1, 2] * TabNonWhiteYes[2, 1])
print(oddsratio_nonwhite_yes)
odds.ratio(TabNonWhiteYes)

# Chi-squared tests
chisq.test(TabCombined, correct = TRUE)
chisq.test(TabWomanNo, correct = TRUE)
chisq.test(TabWomanYes, correct = TRUE)
chisq.test(TabNonWhiteNo, correct = TRUE)
chisq.test(TabNonWhiteYes, correct = TRUE)




library(readxl)
attach(df)

library(fastDummies)
df1 <- dummy_cols(df,select_columns = c("education","work","work_permit"),
                  remove_selected_columns = TRUE,
                  remove_most_frequent_dummy = TRUE)


full_model <- glm(poverty ~ .,data = df1[1:24], family = binomial(link = "logit" ))
reduced_model <- glm(poverty ~ .,data = df1[2:24], family = binomial(link = "logit" ))


library(jtools)
summ(full_model , exp = TRUE)
#(Intercept):Odds Ratio: 0.31Interpretation: The baseline odds of poverty (for reference categories of all variables) is significantly less than 1 (p < 0.001), indicating a lower likelihood of poverty for the reference group.
#woman1:Odds Ratio: 1.13Interpretation: Women are 13% more likely to be in poverty compared to men, significant at the 0.01 level (p < 0.01).
#others the same
#The logistic regression model indicates that several factors significantly influence poverty status. Non-white individuals have more than twice the odds of being in poverty compared to white individuals (OR = 2.17, p < 0.001). Women are also slightly more likely to be in poverty (OR = 1.13, p < 0.01). Higher education levels reduce the odds of poverty, with the highest education level (education_7) having the most substantial effect (OR = 0.28, p < 0.001). Urban living significantly decreases the odds of poverty (OR = 0.52, p < 0.001). Other factors such as age, metropolitan area, and work status show varying levels of influence on poverty.


library(lmtest)
lrtest(full_model, reduced_model)
#Or
anova(full_modell, reduced_modell, test = "LRT")
#The log-likelihood ratio test and the analysis of deviance table both show that including the `woman` variable significantly improves the model's fit for predicting poverty, with p-values of 0.0067 and 0.0006, respectively. This indicates that the `woman` variable has a statistically significant effect on poverty status. Excluding this variable leads to a poorer model fit, underscoring its importance in the analysis.
#All Contingency tables
independent_vars <- c("woman", "age", "education", "work", "metropolitan_area", "non_white", "urban", "work_permit")
for (var in independent_vars) {
  cat("Contingency table for", var, "vs poverty:\n")
  print(table(df[[var]], df$poverty))
  cat("\n")
}


#Odds Ratio 
coef_full_model <- coef(full_model)
odds_ratios <- exp(coef_full_model)
print(odds_ratios)
se <- sqrt(diag(vcov(full_model)))
ci_lower <- exp(coef_full_model - qnorm(0.975) * se)
ci_upper <- exp(coef_full_model + qnorm(0.975) * se)
or_ci <- data.frame(
  Odds_Ratio = odds_ratios,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)
print(or_ci)


model <- glm(poverty ~ .,data = df1[1:24], family = binomial(link = "logit" ))
interaction_model <- glm(poverty ~ .+(df$woman*df$non_white),data = df1[1:24], family = binomial(link = "logit" ))
anova(interaction_model, model, test = "LRT")
# based on this analysis, Model 2 (without the interaction term) does not significantly worsen the fit compared to Model 1, suggesting that the interaction term (df$woman * df$non_white) may not be necessary in explaining the variation in the response variable (poverty).

library(ordinal)
df$poverty <- as.factor(df$poverty)
df$woman <- as.factor(df$woman)
df$age <- as.numeric(df$age)
df$education <- as.factor(df$education)
df$work <- as.factor(df$work)
df$metropolitan_area <- as.factor(df$metropolitan_area)
df$non_white <- as.factor(df$non_white)
df$urban <- as.factor(df$urban)
df$work_permit <- as.factor(df$work_permit)
model_cloglog <- glm(poverty ~ woman + age + education + work + metropolitan_area + non_white + urban + work_permit, 
                     family = binomial(link = cloglog), data = df)
summary(model_cloglog)
#Significance of Predictors: Many predictors are highly significant, suggesting they have a strong association with poverty status.
#The coefficients show the direction and magnitude of the effect each predictor has on the log odds of being in poverty. Positive coefficients indicate increased odds, while negative coefficients indicate decreased odds.
#The residual deviance is lower than the null deviance, indicating that the model with predictors fits the data better than the model with only the intercept.