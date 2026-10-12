# Housing Market Classification
This project uses Zillow metro-level housing data to classify housing markets based on whether they experienced above-median monthly home value growth. Logistic regression models incorporating housing supply-demand conditions, market activity indicators, and interaction effects were developed and evaluated using time-based train/test validation.

## Dataset
Housing market datasets sourced from Zillow Research Data:
https://www.zillow.com/research/data/

The analysis uses monthly metropolitan housing market observations beginning in 2018. After cleaning and merging the datasets, 40,182 observations were retained for modeling.

## Target Variable
The response variable is a binary indicator representing whether a housing market experienced above-median month-over-month home value growth.

Monthly percentage growth was calculated using the Zillow Home Value Index (ZHVI):

Price Growth (%) = ((Current Month Value − Previous Month Value) / Previous Month Value) × 100

The median monthly growth rate was calculated using the training data (before January 2022), resulting in a threshold of approximately 0.606%.

- Hot Market (1): Monthly home value growth above 0.606%.
- Not Hot Market (0): Monthly home value growth at or below 0.606%.

The same threshold was applied to the training and testing datasets to maintain a consistent classification.

## Methods
- Logistic Regression
- Interaction Effects
- Time-Based Train/Test Split Validation
- Multicollinearity Checks (VIF)
- ROC/AUC Evaluation
- Model Diagnostics
- Log Transformations
- Predictor Centering

The data was divided chronologically to prevent future observations from influencing model development.

- Training: Observations before January 2022 (17,284 observations)
- Testing: Observations from January 2022 onward (22,898 observations)

Feature selection, model diagnostics, and interaction comparisons were performed using the training data. The final model was evaluated on the testing data.

## Features Used
- Median Home Value (ZHVI)
- Inventory
- Days Pending
- Sale-to-List Ratio
- Market Heat Index
- Census Region

New listings were initially considered but excluded from the final model following multicollinearity analysis.

## Results

The final logistic regression model achieved the following performance on the testing dataset:

| Metric | Result |
|---|---|
| Accuracy | 71.7% |
| Sensitivity | 77.1% |
| Specificity | 70.0% |
| ROC AUC | 0.814 |

Although the model's overall accuracy was below the 75.7% accuracy achieved by always predicting the non-hot class on the testing dataset, it achieved a sensitivity of 77.1% and an ROC AUC of 0.814. These results indicate that the model was effective at identifying above-median growth observations and distinguishing between the two classes, despite the imbalance in the testing dataset.

Including interaction effects improved model fit compared with the baseline logistic regression model, as indicated by a lower AIC and statistically significant likelihood-ratio tests on the training data.

## Key Insights
- Lower inventory levels and shorter pending times were associated with higher odds of above-median monthly home value growth.

- Higher market heat index values were associated with an increased likelihood of a housing market being classified as hot.

- Interaction effects suggested that the relationships between sale-to-list ratios, pending times, and housing growth varied depending on overall market heat conditions.

- Logistic regression provided interpretable relationships while demonstrating useful classification performance on later observations.

## Visuals
### Correlation Heatmap
Correlation heatmap showing relationships between the major housing market indicators used in the logistic regression model. The correlations were calculated using the training dataset to examine relationships among predictors and identify potential multicollinearity.

<img width="463" height="515" alt="image" src="https://github.com/user-attachments/assets/fbd7adcb-3a37-4c43-bf63-e1d2df67e451" />

### ROC Curve
Receiver Operating Characteristic (ROC) curve for the final logistic regression model evaluated on the testing dataset. The model achieved an AUC of approximately 0.814, indicating good discrimination between above-median and below-median housing market growth observations.

<img width="463" height="515" alt="image" src="https://github.com/user-attachments/assets/c5ad045f-095f-45fe-95f3-26656222d6c3" />

## Tools
Analysis and modeling were completed entirely in R using statistical and machine learning libraries.
- R
- readr
- dplyr
- ggplot2
- pROC
- car
- tidyr
- corrplot

## Files
- `housing_market_classification.R` — main analysis and modeling workflow
- `Data/` — datasets used in the analysis

## Limitations

The model uses housing market indicators observed during the same month as home value growth. Therefore, the results should be interpreted as classification rather than forecasting future housing market performance.

Observations with missing housing market indicators were excluded, so the results may not generalize to all metropolitan housing markets.

Additionally, repeated monthly observations from the same metropolitan areas may introduce dependencies that are not fully accounted for by the logistic regression model.
