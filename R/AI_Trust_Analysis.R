
# ============================================================
# IT3011 TPSM Project
# Trust in Data and Adoption of AI-Driven Insights
# Dataset: AI Trust Insights
# ============================================================

# -----------------------------
# 1. Load Dataset
# -----------------------------

setwd("D:\\Y3S1\\TPSM\\AI_Trust_Analysis")

data <- read.csv("ai_skepticism_dataset.csv")

head(data)
dim(data)
str(data)
summary(data)


# -----------------------------
# 2. Data Cleaning
# -----------------------------

# Check missing values
colSums(is.na(data))
sum(is.na(data))

# Check duplicates
sum(duplicated(data))

# Replace missing belief alignment values with "Unknown"
data$belief_alignment_status <- ifelse(is.na(data$belief_alignment_status),
                                       "Unknown",
                                       as.character(data$belief_alignment_status))

# Convert nominal variables to factors
data$ai_model_name <- as.factor(data$ai_model_name)
data$query_category <- as.factor(data$query_category)
data$respondent_age_bracket <- as.factor(data$respondent_age_bracket)
data$education_level <- as.factor(data$education_level)
data$fact_check_method_used <- as.factor(data$fact_check_method_used)
data$belief_alignment_status <- as.factor(data$belief_alignment_status)

# Convert ordinal variables to ordered factors
data$answer_detail_level <- factor(data$answer_detail_level,
                                   levels = c("Vague", "Moderate", "Specific", "Very Specific"),
                                   ordered = TRUE)

data$digital_literacy_score <- factor(data$digital_literacy_score,
                                      levels = c("Low", "Medium", "High", "Expert"),
                                      ordered = TRUE)

data$ai_familiarity_level <- factor(data$ai_familiarity_level,
                                    levels = c("First Time", "Beginner", "Intermediate", "Advanced", "Expert"),
                                    ordered = TRUE)

data$decision_importance <- factor(data$decision_importance,
                                   levels = c("Low", "Medium", "High", "Critical"),
                                   ordered = TRUE)

data$urgency_level <- factor(data$urgency_level,
                             levels = c("None", "Low", "Medium", "High"),
                             ordered = TRUE)

data$user_skepticism_category <- factor(data$user_skepticism_category,
                                        levels = c("Blind Trust", "Moderate Trust", "Skeptical", "Highly Skeptical"),
                                        ordered = TRUE)


# -----------------------------
# 3. Derived Variables
# -----------------------------

# Adoption proxy: 1 = Adopted, 0 = Non-adopted
data$adoption_proxy <- ifelse(data$trust_score_out_of_10 >= 7, 1, 0)
data$adoption_proxy <- as.factor(data$adoption_proxy)

# Numeric adoption variable for regression
data$adoption_numeric <- ifelse(data$adoption_proxy == 1, 1, 0)

# Categorized trust variable for chi-square test
data$trust_category <- cut(data$trust_score_out_of_10,
                           breaks = c(0, 6, 8, 10),
                           labels = c("Low", "Medium", "High"),
                           include.lowest = TRUE)

# Check derived variables
table(data$adoption_proxy)
prop.table(table(data$adoption_proxy))
table(data$trust_category)


# -----------------------------
# 4. Descriptive Statistics
# -----------------------------

mean(data$trust_score_out_of_10)
median(data$trust_score_out_of_10)
sd(data$trust_score_out_of_10)

mean(data$answer_accuracy_percentage)
median(data$answer_accuracy_percentage)
sd(data$answer_accuracy_percentage)

mean(data$ai_confidence_percentage)
median(data$ai_confidence_percentage)
sd(data$ai_confidence_percentage)

tapply(data$trust_score_out_of_10, data$adoption_proxy, mean)
tapply(data$trust_score_out_of_10, data$query_category, mean)
tapply(data$trust_score_out_of_10, data$ai_model_name, mean)


# -----------------------------
# 5. Descriptive Charts
# -----------------------------

# Histogram of trust scores
hist(data$trust_score_out_of_10,
     main = "Distribution of Trust Scores",
     xlab = "Trust Score (out of 10)",
     ylab = "Frequency",
     breaks = 10,
     col = "lightblue",
     border = "black")

# Boxplot: Trust vs Adoption
boxplot(trust_score_out_of_10 ~ adoption_proxy,
        data = data,
        main = "Trust Score vs Adoption",
        xlab = "Adoption (0 = Non-Adopted, 1 = Adopted)",
        ylab = "Trust Score",
        col = c("salmon", "lightgreen"))

# Bar plot: Adoption distribution
adopt_counts <- table(data$adoption_proxy)

barplot(adopt_counts,
        main = "Adoption of AI Insights",
        xlab = "User Group",
        ylab = "Number of Users",
        names.arg = c("Non-Adopted", "Adopted"),
        col = c("lightblue", "pink"))

text(x = c(0.7, 1.9),
     y = adopt_counts,
     label = adopt_counts,
     pos = 3)

# Scatter plot: Trust vs Accuracy
plot(data$answer_accuracy_percentage,
     data$trust_score_out_of_10,
     main = "Trust vs Accuracy",
     xlab = "Answer Accuracy (%)",
     ylab = "Trust Score",
     pch = 19,
     col = "blue")

abline(lm(trust_score_out_of_10 ~ answer_accuracy_percentage, data = data),
       col = "red",
       lwd = 2)

# Scatter plot: Trust vs AI Confidence
plot(data$ai_confidence_percentage,
     data$trust_score_out_of_10,
     main = "Trust vs AI Confidence",
     xlab = "AI Confidence (%)",
     ylab = "Trust Score",
     pch = 19,
     col = "darkgreen")

abline(lm(trust_score_out_of_10 ~ ai_confidence_percentage, data = data),
       col = "red",
       lwd = 2)

# Boxplot: Trust vs Answer Detail Level
boxplot(trust_score_out_of_10 ~ answer_detail_level,
        data = data,
        main = "Trust vs Answer Detail Level",
        xlab = "Answer Detail Level",
        ylab = "Trust Score",
        col = "purple")

# Boxplot: Trust by Query Category
boxplot(trust_score_out_of_10 ~ query_category,
        data = data,
        main = "Trust Score by Query Category",
        xlab = "Query Category",
        ylab = "Trust Score",
        las = 2,
        col = "lightblue")

# Bar plot: Mean Trust by AI Model
mean_trust_model <- tapply(data$trust_score_out_of_10,
                           data$ai_model_name,
                           mean)

barplot(mean_trust_model,
        main = "Mean Trust Score by AI Model",
        xlab = "AI Model",
        ylab = "Mean Trust Score",
        col = "skyblue",
        las = 2)

# Bar plot: Cited Sources vs Trust
mean_trust_cited <- tapply(data$trust_score_out_of_10,
                           data$has_cited_sources,
                           mean)

barplot(mean_trust_cited,
        main = "Effect of Cited Sources on Trust",
        xlab = "Has Cited Sources",
        ylab = "Mean Trust Score",
        names.arg = c("No Sources", "Sources Cited"),
        col = c("red", "green"))


# -----------------------------
# 6. Inferential Analysis
# -----------------------------

# 6.1 Pearson Correlation: Trust vs Accuracy
cor_accuracy <- cor.test(data$trust_score_out_of_10,
                         data$answer_accuracy_percentage)

cor_accuracy

# 6.2 Pearson Correlation: Trust vs AI Confidence
cor_confidence <- cor.test(data$trust_score_out_of_10,
                           data$ai_confidence_percentage)

cor_confidence

# 6.3 t-test: Trust vs Adoption
t_test_result <- t.test(trust_score_out_of_10 ~ adoption_proxy,
                        data = data)

t_test_result

# Decision for t-test
alpha <- 0.05

if (t_test_result$p.value < alpha) {
  print("t-test: Reject H0")
} else {
  print("t-test: Do not reject H0")
}

# 6.4 ANOVA: Trust vs Query Category
anova_query <- aov(trust_score_out_of_10 ~ query_category,
                   data = data)

anova_summary <- summary(anova_query)
anova_summary

# Extract ANOVA p-value
anova_p <- anova_summary[[1]]["query_category", "Pr(>F)"]

if (anova_p < alpha) {
  print("ANOVA: Reject H0")
} else {
  print("ANOVA: Do not reject H0")
}

# Optional post-hoc test
TukeyHSD(anova_query)

# 6.5 t-test: Trust vs Cited Sources
t_test_sources <- t.test(trust_score_out_of_10 ~ has_cited_sources,
                         data = data)

t_test_sources

# 6.6 Chi-square test: Trust Category vs Adoption
table_ct <- table(data$trust_category, data$adoption_proxy)

table_ct

chi_result <- chisq.test(table_ct)

chi_result

if (chi_result$p.value < alpha) {
  print("Chi-square: Reject H0")
} else {
  print("Chi-square: Do not reject H0")
}

# 6.7 Cramer's V
chi_sq <- chi_result$statistic
n <- sum(table_ct)

cramers_v <- sqrt(as.numeric(chi_sq) / (n * (min(dim(table_ct)) - 1)))

cramers_v


# -----------------------------
# 7. Predictive Analysis
# -----------------------------

# 7.1 Linear Regression: Adoption vs Trust
model_linear <- lm(adoption_numeric ~ trust_score_out_of_10,
                   data = data)

summary(model_linear)

# Extract R-squared and coefficients
summary(model_linear)$r.squared
summary(model_linear)$coefficients

# Decision for trust coefficient
linear_p <- summary(model_linear)$coefficients["trust_score_out_of_10", "Pr(>|t|)"]

if (linear_p < alpha) {
  print("Linear Regression: Reject H0")
} else {
  print("Linear Regression: Do not reject H0")
}

# 7.2 Multiple Regression Model
model_multiple <- lm(adoption_numeric ~ trust_score_out_of_10 +
                       answer_accuracy_percentage +
                       has_cited_sources +
                       decision_importance +
                       urgency_level +
                       user_skepticism_category,
                     data = data)

summary(model_multiple)

# Extract R-squared and coefficients
summary(model_multiple)$r.squared
summary(model_multiple)$coefficients


# -----------------------------
# 8. Model Performance Charts
# -----------------------------

# Predicted values from linear model
predicted_linear <- predict(model_linear)

# Actual vs predicted adoption
plot(data$adoption_numeric,
     predicted_linear,
     main = "Actual vs Predicted Adoption",
     xlab = "Actual Adoption",
     ylab = "Predicted Values",
     pch = 19,
     col = "blue")

abline(0, 1, col = "red", lwd = 2)

# Residual plot
plot(fitted(model_linear),
     residuals(model_linear),
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 19,
     col = "blue")

abline(h = 0, col = "red", lwd = 2)

# Q-Q plot of residuals
qqnorm(residuals(model_linear),
       main = "Normal Q-Q Plot of Residuals")

qqline(residuals(model_linear),
       col = "red",
       lwd = 2)


# -----------------------------
# 9. Save Charts as PNG Files
# -----------------------------

# Create folder for plots
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Save histogram
png("plots/01_trust_distribution.png", width = 900, height = 700)
hist(data$trust_score_out_of_10,
     main = "Distribution of Trust Scores",
     xlab = "Trust Score (out of 10)",
     ylab = "Frequency",
     breaks = 10,
     col = "lightblue",
     border = "black")
dev.off()

# Save boxplot trust vs adoption
png("plots/02_trust_vs_adoption.png", width = 900, height = 700)
boxplot(trust_score_out_of_10 ~ adoption_proxy,
        data = data,
        main = "Trust Score vs Adoption",
        xlab = "Adoption (0 = Non-Adopted, 1 = Adopted)",
        ylab = "Trust Score",
        col = c("salmon", "lightgreen"))
dev.off()

# Save adoption distribution
png("plots/03_adoption_distribution.png", width = 900, height = 700)
adopt_counts <- table(data$adoption_proxy)
barplot(adopt_counts,
        main = "Adoption of AI Insights",
        xlab = "User Group",
        ylab = "Number of Users",
        names.arg = c("Non-Adopted", "Adopted"),
        col = c("lightblue", "pink"))
text(x = c(0.7, 1.9),
     y = adopt_counts,
     label = adopt_counts,
     pos = 3)
dev.off()

# Save trust vs accuracy
png("plots/04_trust_vs_accuracy.png", width = 900, height = 700)
plot(data$answer_accuracy_percentage,
     data$trust_score_out_of_10,
     main = "Trust vs Accuracy",
     xlab = "Answer Accuracy (%)",
     ylab = "Trust Score",
     pch = 19,
     col = "blue")
abline(lm(trust_score_out_of_10 ~ answer_accuracy_percentage, data = data),
       col = "red",
       lwd = 2)
dev.off()

# Save trust vs AI confidence
png("plots/05_trust_vs_ai_confidence.png", width = 900, height = 700)
plot(data$ai_confidence_percentage,
     data$trust_score_out_of_10,
     main = "Trust vs AI Confidence",
     xlab = "AI Confidence (%)",
     ylab = "Trust Score",
     pch = 19,
     col = "darkgreen")
abline(lm(trust_score_out_of_10 ~ ai_confidence_percentage, data = data),
       col = "red",
       lwd = 2)
dev.off()

# Save trust vs answer detail level
png("plots/06_trust_vs_detail_level.png", width = 900, height = 700)
boxplot(trust_score_out_of_10 ~ answer_detail_level,
        data = data,
        main = "Trust vs Answer Detail Level",
        xlab = "Answer Detail Level",
        ylab = "Trust Score",
        col = "purple")
dev.off()

# Save trust by query category
png("plots/07_trust_by_query_category.png", width = 1000, height = 800)
boxplot(trust_score_out_of_10 ~ query_category,
        data = data,
        main = "Trust Score by Query Category",
        xlab = "Query Category",
        ylab = "Trust Score",
        las = 2,
        col = "lightblue")
dev.off()

# Save cited sources effect
png("plots/08_cited_sources_trust.png", width = 900, height = 700)
mean_trust_cited <- tapply(data$trust_score_out_of_10,
                           data$has_cited_sources,
                           mean)
barplot(mean_trust_cited,
        main = "Effect of Cited Sources on Trust",
        xlab = "Has Cited Sources",
        ylab = "Mean Trust Score",
        names.arg = c("No Sources", "Sources Cited"),
        col = c("red", "green"))
dev.off()

# Save actual vs predicted
png("plots/09_actual_vs_predicted.png", width = 900, height = 700)
plot(data$adoption_numeric,
     predicted_linear,
     main = "Actual vs Predicted Adoption",
     xlab = "Actual Adoption",
     ylab = "Predicted Values",
     pch = 19,
     col = "blue")
abline(0, 1, col = "red", lwd = 2)
dev.off()

# Save residual plot
png("plots/10_residuals_vs_fitted.png", width = 900, height = 700)
plot(fitted(model_linear),
     residuals(model_linear),
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 19,
     col = "blue")
abline(h = 0, col = "red", lwd = 2)
dev.off()


# -----------------------------
# 10. Final Summary Output
# -----------------------------

cat("\n================ FINAL RESULTS SUMMARY ================\n")

cat("\nDataset size:\n")
print(dim(data))

cat("\nAdoption distribution:\n")
print(table(data$adoption_proxy))
print(prop.table(table(data$adoption_proxy)))

cat("\nMean trust by adoption group:\n")
print(tapply(data$trust_score_out_of_10, data$adoption_proxy, mean))

cat("\nCorrelation: Trust vs Accuracy:\n")
print(cor_accuracy)

cat("\nCorrelation: Trust vs AI Confidence:\n")
print(cor_confidence)

cat("\nT-test: Trust vs Adoption:\n")
print(t_test_result)

cat("\nANOVA: Trust vs Query Category:\n")
print(anova_summary)

cat("\nChi-square: Trust Category vs Adoption:\n")
print(chi_result)

cat("\nCramer's V:\n")
print(cramers_v)

cat("\nLinear Regression Summary:\n")
print(summary(model_linear))

cat("\nMultiple Regression Summary:\n")
print(summary(model_multiple))

cat("\nFinal Decision:\n")
cat("Since the major p-values are less than 0.05, we reject H0.\n")
cat("Conclusion: Trust significantly influences adoption of AI-driven insights.\n")

cat("\n=======================================================\n")
