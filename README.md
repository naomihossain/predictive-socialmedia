# predictive-socialmedia

## Objective
Identify which social media behaviors have predictive power for depression severity.

## Why this matters?
* Social media use is extremely widespread among young adults.
* 78% of U.S. adults aged 18-29 use Instagram and over 60% use TikTok and/or Snapchat (Pew Research, 2024).
* Heavy social media use has been linked to negative mental health outcomes.

## Our inital hypothesis
Possible drivers of depression severity:
1. Higher daily screen time ⟶ higher depression severity
2. Late-night social media use ⟶ higher depression severity
3. Active engagement ⟶ higher depression severity than passive use
4. Depression severity may vary across platforms

## Data Overview
Dataset: Social Media and Mental Health Dataset (Kaggle)
* generated using a Behavioral Simulation Engine that enforces logic based on current pyshological literature.

Sample Size: 8000 individuals
Variables: 12 variables
Unit of Observation: individual survey respondent
Dependent Variable: PHQ_9_Score is a depression score calculated from the overall questions in the survey

## Random Forest

Why random forest?
* Captures nonlinear relationships between behavior and depression scores
* Allows ranking of most predictive variables

Model Performance:
* RMSE = 3.49
* Predictions differ from actual PHQ-9 scores by about 3–4 points on average
* R² ≈ 0.47
* ~47% of variation in PHQ-9 scores explained

Top Predictors:
* Late-night social media usage
* Daily screen time
* Sleep duration
* User engagement archetype

Conclusion: 
Behaviroal patterns are significantly more predictive of depression severity than demographic characteristics. 

Key Behavioral Insgits: 
(insert image)

## Linear Regression Model

Why linear regression?
* Provides interpretable relationships between social media behavior and depression scores
* Identifies which predictors are statistically significant

Model Performance
* RMSE ≈ 3.4
* Predictions differ from actual PHQ-9 scores by about 3–4 points on average
* R² ≈ 0.498
* Model explains about 50% of the variation in depression scores

Top Predictors:
Strongest predictors of depression severity:
* Late-night social media usage
* Daily screen time

Conclusion:
Behavioral usage patterns are stronger predictors of depression severity than platform choice or demographic variables.

(insert image)


## Conclusion
Hypothesis 3 and 4 were not supported by our project. Contrary to expectations, passive scrolling showed a stronger association with higher depression scores than active engagement. Results for Hypothesis 4 were not significant, suggesting that depression severity was similar across platforms.

## Implications and Limitations
Practical Implications:
Behavioral patterns in social media use may help identify individuals at higher risk for depression.
These insights could support:
* Digital wellness tools that track screen time and encourage breaks
* Mental health screening to identify individuals who may need support
* Behavioral interventions that promote healthier social media habits

Limitations:
* Data comes from a single cross-sectional survey
* Self-reported behavior may be inaccurate
* Results show correlation, not causation

Future Improvements:
* Collect longitudinal data over time
* Use actual smartphone usage logs
* Include additional psychological and lifestyle factors








