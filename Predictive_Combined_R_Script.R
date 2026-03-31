# Project:
# Predicting depression severity from social media behaviour

# Part 1: Data Cleaning

#----------------------------------------------------------------------------

# Part 2: Exploratory Data Analysis

# 1. Load Libraries

library(tidyverse)
library(ggplot2)


# 2. Load Data

data <- read.csv("cleaned_social_media_data.csv")


# What platforms are associated with higher depression scores?

ggplot(data, aes(x = Primary_Platform, y = PHQ_9_Score)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal() +
  labs(
    title = "Depression Scores by Primary Platform",
    x = "Primary Platform",
    y = "PHQ-9 Score"
  )


# Does dominant content type relate to depression scores?

ggplot(data, aes(x = Dominant_Content_Type, y = PHQ_9_Score)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(
    title = "Depression Scores by Dominant Content Type",
    x = "Content Type",
    y = "PHQ-9 Score"
  )


# Does social comparison trigger relate to higher depression scores?

ggplot(data, aes(x = factor(Social_Comparison_Trigger), y = PHQ_9_Score)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  labs(
    title = "Depression Scores by Social Comparison Trigger",
    x = "Social Comparison Trigger",
    y = "PHQ-9 Score"
  )


# Which platforms are more associated with social comparison?

ggplot(data, aes(x = Primary_Platform, fill = factor(Social_Comparison_Trigger))) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(
    title = "Social Comparison by Platform",
    x = "Primary Platform",
    y = "Proportion",
    fill = "Social Comparison"
  )


# Are certain activity types associated with higher depression scores?

ggplot(data, aes(x = Activity_Type, y = PHQ_9_Score)) +
  geom_boxplot(fill = "purple") +
  theme_minimal() +
  labs(
    title = "Depression Scores by Activity Type",
    x = "Activity Type",
    y = "PHQ-9 Score"
  )


# What type of content is dominant on each platform?

ggplot(data, aes(x = Primary_Platform, fill = Dominant_Content_Type)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(
    title = "Dominant Content Type by Platform",
    x = "Primary Platform",
    y = "Proportion",
    fill = "Content Type"
  )


# Do different social media behavior groups show different depression levels?

data$User_Group <- ifelse(data$Late_Night_Usage == 1 & data$Daily_Screen_Time_Hours > 4,
                          "Heavy Late Night Users",
                          ifelse(data$Late_Night_Usage == 1,
                                 "Late Night Users",
                                 ifelse(data$Daily_Screen_Time_Hours > 4,
                                        "Heavy Screen Time Users",
                                        "Lower Usage Users")))

ggplot(data, aes(x = User_Group, y = PHQ_9_Score)) +
  geom_boxplot(fill = "red") +
  theme_minimal() +
  labs(
    title = "Depression Scores by User Behavior Group",
    x = "User Behavior Group",
    y = "PHQ-9 Score"
  )

#----------------------------------------------------------------------------

# Part 3: Logistic Regression

# Load Libraries
library(tidyverse)
library(car)
library(caret)
library(ggplot2)

# Load Data
data <- read.csv("cleaned_social_media_data.csv")

# Basic Data Checks
cat("Dataset Dimensions:\n")
print(dim(data))

cat("\nMissing Values Per Column:\n")
print(colSums(is.na(data)))

str(data)

# Create Binary Depression Risk Variable
data$Depression_Risk <- ifelse(data$PHQ_9_Score >= 10, 1, 0)
data$Depression_Risk <- factor(data$Depression_Risk)

cat("\nDistribution of Depression Risk:\n")
print(table(data$Depression_Risk))
prop.table(table(data$Depression_Risk))

# Train/Test Split
set.seed(123)

trainIndex <- createDataPartition(data$Depression_Risk, p = 0.8, list = FALSE)

train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Logistic Regression Model
logistic_model <- glm(
  Depression_Risk ~ 
    Daily_Screen_Time_Hours +
    Late_Night_Usage +
    Social_Comparison_Trigger +
    Activity_Type +
    Primary_Platform +
    Dominant_Content_Type +
    Daily_Screen_Time_Hours:Late_Night_Usage +
    Daily_Screen_Time_Hours:Social_Comparison_Trigger +
    Late_Night_Usage:Social_Comparison_Trigger,
  data = train_data,
  family = binomial
)

summary(logistic_model)

# Multicollinearity Check
vif(logistic_model)

# Predict Probabilities
prob_predictions <- predict(logistic_model, newdata = test_data, type = "response")

# Convert Probabilities to Class Predictions
class_predictions <- ifelse(prob_predictions > 0.5, 1, 0)
class_predictions <- factor(class_predictions)

# Model Performance
conf_matrix <- confusionMatrix(class_predictions, test_data$Depression_Risk)

cat("\nConfusion Matrix:\n")
print(conf_matrix)

# Accuracy
accuracy <- conf_matrix$overall["Accuracy"]

cat("\nModel Accuracy:\n")
print(accuracy)

# ROC Curve Visualization
library(pROC)

roc_obj <- roc(test_data$Depression_Risk, prob_predictions)

plot(roc_obj,
     col = "blue",
     main = "ROC Curve for Logistic Regression")

auc_value <- auc(roc_obj)

cat("\nAUC Value:\n")
print(auc_value)

# Visualization of Predicted Probabilities
ggplot(data.frame(prob_predictions), aes(x = prob_predictions)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Distribution of Predicted Depression Risk Probabilities",
       x = "Predicted Probability",
       y = "Frequency")

# Visualization of Depression Risk by Late Night Usage
ggplot(data, aes(x = factor(Late_Night_Usage), fill = Depression_Risk)) +
  geom_bar(position = "fill") +
  labs(title = "Depression Risk by Late Night Social Media Usage",
       x = "Late Night Social Media Use",
       y = "Proportion") +
  scale_fill_discrete(name = "Depression Risk")

#----------------------------------------------------------------------------

# Part 4: Linear Regression


# 1. Load Libraries

library(tidyverse)
library(car)
library(caret)
library(ggplot2)
library(gridExtra)


# 2. Load Data

data <- read.csv("cleaned_social_media_data.csv")


# 3. Convert Categorical Variables to Factors

data$Late_Night_Usage <- factor(data$Late_Night_Usage)
data$Social_Comparison_Trigger <- factor(data$Social_Comparison_Trigger)
data$Activity_Type <- factor(data$Activity_Type)
data$Primary_Platform <- factor(data$Primary_Platform)
data$Dominant_Content_Type <- factor(data$Dominant_Content_Type)


# 4. Basic Data Checks

cat("Dataset Dimensions:\n")
print(dim(data))

cat("\nMissing Values Per Column:\n")
print(colSums(is.na(data)))

str(data)


# 5. Descriptive Statistics

cat("\nSummary Statistics:\n")
summary(data)


# Distribution of Screen Time
ggplot(data, aes(x = Daily_Screen_Time_Hours)) +
  geom_histogram(bins = 30, fill = "blue") +
  labs(
    title = "Distribution of Daily Screen Time",
    x = "Hours Per Day",
    y = "Frequency"
  )


# Distribution of Depression Severity
ggplot(data, aes(x = PHQ_9_Severity)) +
  geom_bar(fill = "blue") +
  labs(
    title = "Distribution of Depression Severity Levels",
    x = "Depression Severity",
    y = "Count"
  )


# Platform Distribution
platform_table <- table(data$Primary_Platform)
print(platform_table)
prop.table(platform_table)


# Activity Type Distribution
activity_table <- table(data$Activity_Type)
print(activity_table)
prop.table(activity_table)


# Dominant Content Distribution
content_table <- table(data$Dominant_Content_Type)
print(content_table)
prop.table(content_table)


# Social Comparison Distribution
comparison_table <- table(data$Social_Comparison_Trigger)
print(comparison_table)
prop.table(comparison_table)


# 6. Cross Tabulation Analysis

cat("\nPlatform vs Depression Severity\n")
table(data$Primary_Platform, data$PHQ_9_Severity)

cat("\nSocial Comparison vs Depression Severity\n")
table(data$Social_Comparison_Trigger, data$PHQ_9_Severity)

cat("\nActivity Type vs Depression Severity\n")
table(data$Activity_Type, data$PHQ_9_Severity)

cat("\nDominant Content vs Depression Severity\n")
table(data$Dominant_Content_Type, data$PHQ_9_Severity)


# 7. Train/Test Split

set.seed(123)

trainIndex <- createDataPartition(data$PHQ_9_Score, p = 0.8, list = FALSE)

train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]


# 8. Baseline Regression Model

baseline_model <- lm(
  PHQ_9_Score ~ Daily_Screen_Time_Hours + Late_Night_Usage,
  data = train_data
)

summary(baseline_model)


# 9. Behavioral Interaction Model

interaction_model <- lm(
  PHQ_9_Score ~ Daily_Screen_Time_Hours +
    Late_Night_Usage +
    Social_Comparison_Trigger +
    Activity_Type +
    Primary_Platform +
    Dominant_Content_Type +
    Daily_Screen_Time_Hours:Late_Night_Usage +
    Daily_Screen_Time_Hours:Social_Comparison_Trigger,
  data = train_data
)

summary(interaction_model)


# 10. Multicollinearity Check

vif(interaction_model)


# 11. Model Comparison (Nested F Test)

anova(baseline_model, interaction_model)


# 12. Predictions

predictions <- predict(interaction_model, newdata = test_data)


# 13. Model Performance

rmse <- sqrt(mean((test_data$PHQ_9_Score - predictions)^2))

cat("\nModel RMSE:\n")
print(rmse)


# 14. Actual vs Predicted Plot

ggplot(
  data.frame(actual = test_data$PHQ_9_Score, predicted = predictions),
  aes(x = actual, y = predicted)
) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm") +
  labs(
    title = "Actual vs Predicted Depression Scores",
    x = "Actual PHQ-9 Score",
    y = "Predicted PHQ-9 Score"
  )


# 15. Late Night Usage vs Depression

ggplot(data, aes(x = Late_Night_Usage, y = PHQ_9_Score)) +
  geom_boxplot(fill = "blue") +
  theme_minimal() +
  labs(
    title = "Depression Scores by Late Night Social Media Usage",
    x = "Late Night Social Media Use",
    y = "PHQ-9 Depression Score"
  )

#----------------------------------------------------------------------------

# Part 5: Random Forest

rm(list = ls())


# 0) Packages

pkgs <- c("readr","dplyr","ggplot2","randomForest","pdp")

for(p in pkgs){
  if(!requireNamespace(p, quietly = TRUE)){
    install.packages(p)
  }
}

lapply(pkgs, library, character.only = TRUE)


# 1) Load Data

train_path <- "train_data.csv"
test_path  <- "test_data.csv"

train <- readr::read_csv(train_path, show_col_types = FALSE)
test  <- readr::read_csv(test_path,  show_col_types = FALSE)

cat("Train rows:", nrow(train), "\n")
cat("Test rows:", nrow(test), "\n\n")

cat("Variables:\n")
print(names(train))


# 2) Convert categorical variables

train <- train %>% mutate(across(where(is.character), as.factor))
test  <- test  %>% mutate(across(where(is.character), as.factor))


# 3) Random Forest Model (PHQ-9 only)

set.seed(123)

rf_dep <- randomForest(
  PHQ_9_Score ~ . - PHQ_9_Severity - GAD_7_Score - GAD_7_Severity,
  data = train,
  ntree = 500,
  importance = TRUE
)

print(rf_dep)


# 4) Evaluate Model

pred <- predict(rf_dep, newdata = test)

rmse <- sqrt(mean((test$PHQ_9_Score - pred)^2))
mae  <- mean(abs(test$PHQ_9_Score - pred))

cat("\nModel Performance\n")
cat("Test RMSE:", rmse, "\n")
cat("Test MAE :", mae, "\n")


# 5) Variable Importance

importance_matrix <- importance(rf_dep, type = 1)

importance_df <- data.frame(
  Variable = rownames(importance_matrix),
  IncMSE = importance_matrix[,1]
) %>%
  arrange(desc(IncMSE))

print(head(importance_df, 10))


# 6) Plot Variable Importance

ggplot(head(importance_df, 10),
       aes(x = reorder(Variable, IncMSE), y = IncMSE)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance (PHQ-9 Depression)",
    x = "",
    y = "% Increase in MSE"
  )


# 7) Partial Dependence Plot: Daily Screen Time

p1 <- pdp::partial(
  rf_dep,
  pred.var = "Daily_Screen_Time_Hours",
  train = train
)

pdp::plotPartial(
  p1,
  main = "Effect of Daily Screen Time on PHQ-9 Depression Score"
)


# 8) Partial Dependence Plot: Late Night Usage

p2 <- pdp::partial(
  rf_dep,
  pred.var = "Late_Night_Usage",
  train = train
)

pdp::plotPartial(
  p2,
  main = "Effect of Late Night Social Media Use on PHQ-9 Depression Score"
)


# 9) Extra Insight (Group Differences)

cat("\nAverage PHQ-9 by User Archetype\n")
train %>%
  group_by(User_Archetype) %>%
  summarise(n = n(), mean_PHQ = mean(PHQ_9_Score), .groups = "drop") %>%
  arrange(desc(mean_PHQ)) %>%
  print()

cat("\nAverage PHQ-9 by Activity Type\n")
train %>%
  group_by(Activity_Type) %>%
  summarise(n = n(), mean_PHQ = mean(PHQ_9_Score), .groups = "drop") %>%
  arrange(desc(mean_PHQ)) %>%
  print()


# 10) PHQ-9 Risk Matrix (Late-Night Usage x Activity Type)
#     CHANGE MADE: Darker = higher depression
risk_tab <- train %>%
  group_by(Late_Night_Usage, Activity_Type) %>%
  summarise(
    n = n(),
    mean_PHQ = mean(PHQ_9_Score),
    sd_PHQ = sd(PHQ_9_Score),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_PHQ))

print(risk_tab)

ggplot(risk_tab, aes(x = Activity_Type, y = as.factor(Late_Night_Usage), fill = mean_PHQ)) +
  geom_tile() +
  geom_text(aes(label = paste0("Mean=", round(mean_PHQ, 2), "\nN=", n)), color = "black") +
  # Dark = higher PHQ-9, Light = lower PHQ-9
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "PHQ-9 Risk Matrix: Late-Night Usage x Activity Type",
    x = "Activity Type",
    y = "Late Night Usage (0=No, 1=Yes)",
    fill = "Mean PHQ-9"
  )

# 11) Screen Time Buckets Plot (Mean PHQ-9 + 95% CI)
train_bins <- train %>%
  mutate(ScreenTime_Bin = cut(
    Daily_Screen_Time_Hours,
    breaks = c(-Inf, 2, 4, 6, 8, Inf),
    labels = c("0–2", "2–4", "4–6", "6–8", "8+")
  )) %>%
  group_by(ScreenTime_Bin) %>%
  summarise(
    n = n(),
    mean_PHQ = mean(PHQ_9_Score),
    se = sd(PHQ_9_Score) / sqrt(n),
    .groups = "drop"
  )

print(train_bins)

ggplot(train_bins, aes(x = ScreenTime_Bin, y = mean_PHQ)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_PHQ - 1.96 * se, ymax = mean_PHQ + 1.96 * se), width = 0.15) +
  labs(
    title = "Average PHQ-9 by Daily Screen Time (Buckets)",
    x = "Daily Screen Time (hours)",
    y = "Average PHQ-9"
  )


#----------------------------------------------------------------------------

# Part 6: Cross tabulation


library(dplyr)
library(ggplot2)

table(data$PHQ_9_Severity)

#cross tab with percentages
prop.table(table(data$PHQ_9_Severity)) * 100


#cross tab dperession with geneder
table(data$Gender, data$PHQ_9_Severity)
prop.table(table(data$Gender, data$PHQ_9_Severity), 1) * 100

# Create table
tab1 <- as.data.frame(table(data$Gender, data$PHQ_9_Severity))

colnames(tab1) <- c("Gender", "Severity", "Count")

tab1 <- tab1 %>%
  group_by(Gender) %>%
  mutate(Percent = Count / sum(Count) * 100)

tab1$Severity <- factor(
  tab1$Severity,
  levels = c(
    "Severe",
    "Moderately Severe",
    "Moderate",
    "Mild",
    "None-Minimal"
  )
)

ggplot(tab1, aes(x = Gender, y = Percent, fill = Severity)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Depression Severity Distribution by Gender",
    y = "Proportion",
    x = "Gender"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()



#####cross tab depression with primary platform
table(data$Primary_Platform, data$PHQ_9_Severity)
prop.table(table(data$Primary_Platform, data$PHQ_9_Severity), 1) * 100

#create table
tab_platform <- as.data.frame(
  table(data$Primary_Platform, data$PHQ_9_Severity)
)

colnames(tab_platform) <- c("Platform", "Severity", "Count")

#convert to percentages
tab_platform <- tab_platform %>%
  group_by(Platform) %>%
  mutate(Percent = Count / sum(Count))

#reorder severity
tab_platform$Severity <- factor(
  tab_platform$Severity,
  levels = c(
    "Severe",
    "Moderately Severe",
    "Moderate",
    "Mild",
    "None-Minimal"
  )
)

#create stackbar
ggplot(tab_platform, aes(x = Platform, y = Percent, fill = Severity)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Depression Severity Distribution by Primary Platform",
    x = "Primary Platform",
    y = "Proportion"
  ) +
  theme_minimal()


#Regression Model

# Install Required Package

required_packages <- c("caret")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Load Data

data <- read.csv("cleaned_social_media_data.csv")

# Convert categorical variables to factors
data$Gender <- as.factor(data$Gender)
data$Primary_Platform <- as.factor(data$Primary_Platform)
data$Activity_Type <- as.factor(data$Activity_Type)

# Remove severity columns to avoid leakage
data$PHQ_9_Severity <- NULL
data$GAD_7_Severity <- NULL

# Validation Set Approach (80/20 Split)

set.seed(123)

train_index <- createDataPartition(data$PHQ_9_Score, p = 0.8, list = FALSE)
train <- data[train_index, ]
test  <- data[-train_index, ]

cat("Training size:", nrow(train), "\n")
cat("Test size:", nrow(test), "\n")


###################################

###Does the effect of platform depend on the user archetype?

model_platform_archetype <- lm(
  Daily_Screen_Time_Hours ~ 
    Primary_Platform * User_Archetype +
    Late_Night_Usage +
    Social_Comparison_Trigger +
    Sleep_Duration_Hours +
    GAD_7_Score +
    PHQ_9_Score,
  data = train
)

summary(model_platform_archetype)

model_no_interaction <- lm(
  Daily_Screen_Time_Hours ~
    Primary_Platform +
    User_Archetype +
    Late_Night_Usage +
    Social_Comparison_Trigger +
    Sleep_Duration_Hours +
    GAD_7_Score +
    PHQ_9_Score,
  data = train
)

anova(model_no_interaction, model_platform_archetype)

####################

#full regression model using daily screen time as intercept and seeing
#if any other variables my affect it.

model_full <- lm(Daily_Screen_Time_Hours ~ ., data = train)

summary(model_full)

#what is significant: 
#latte_night_usage
#social_comparison_trigger
#sleep_duration_hours
#gad7 (depression)
#phq9 (anxiety)
#userarchetype minimalist
#userarchetype connected
#userarchetype scroller


#Do a stepwise regression to find the best subset of variables
model_step <- step(model_full, direction = "both")

summary(model_step)


#check interaction now
model_interaction <- lm(Daily_Screen_Time_Hours ~
                          Primary_Platform * Late_Night_Usage +
                          Primary_Platform * Social_Comparison_Trigger +
                          Sleep_Duration_Hours +
                          GAD_7_Score +
                          PHQ_9_Score +
                          User_Archetype,
                        data = train)

summary(model_interaction)

#compare models
AIC(model_full, model_step, model_interaction)

#stepwise model has a lower AIC so it was the better model

#interaction variables

#####“Does the effect of social comparison differ by platform?”
library(dplyr)

interaction_tab <- data %>%
  group_by(Primary_Platform,
           Social_Comparison_Trigger,
           PHQ_9_Severity) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Primary_Platform,
           Social_Comparison_Trigger) %>%
  mutate(Percent = Count / sum(Count))

ggplot(interaction_tab,
       aes(x = Primary_Platform,
           y = Percent,
           fill = PHQ_9_Severity)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ Social_Comparison_Trigger) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Interaction: Social Comparison × Platform on Depression Severity")




