# Housing Market Classification

This project uses Zillow metro-level housing data to classify housing markets as “hot” versus average based on housing price growth and market activity indicators.

## Methods
- Logistic Regression
- Interaction Effects
- Time-Based Train/Test Split
- Multicollinearity Checks (VIF)
- ROC/AUC Evaluation
- Model Diagnostics

## Features Used
- Median Home Price
- Inventory
- Days Pending
- Sale-to-List Ratio
- Market Heat Index
- Census Region

## Results
- Final logistic regression model achieved approximately 71.5% test accuracy.
- Model achieved an AUC of approximately 0.77.
- Inventory levels, transaction speed, and market heat indicators were significant predictors of housing market growth.

## Tools
- R
- dplyr
- ggplot2
- pROC
- car
- tidyr

## Files
- `housing_market_classification.R` — main analysis and modeling workflow
- `Data/` — datasets used in the analysis
