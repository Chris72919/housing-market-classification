# Housing Market Classification
This project uses Zillow metro-level housing data to classify housing markets as “hot” versus average based on housing price growth, supply-demand conditions, and market activity indicators. Logistic regression models with interaction effects were developed and evaluated using time-based train/test validation.

## Dataset
Housing market datasets sourced from Zillow Research Data:
https://www.zillow.com/research/data/

## Methods
- Logistic Regression
- Interaction Effects
- Time-Based Train/Test Split Validation
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
- Final logistic regression model achieved approximately 71.5% test accuracy and an AUC of approximately 0.77.
- Interaction effects improved model fit and predictive performance.
- Lower inventory levels, faster transaction speeds, and stronger market heat indicators were associated with higher likelihood of above-average housing market growth.

## Key Insights
- Housing market conditions were strongly influenced by supply-demand dynamics and market activity indicators.
- Lower inventory and faster pending times were associated with higher odds of above-average housing market growth.
- Interaction effects suggested that the impact of pricing competitiveness and transaction speed depended partly on overall market heat conditions.
- Logistic regression provided interpretable relationships while maintaining moderate predictive performance.

## Visuals
### Correlation Heatmap
Correlation heatmap showing relationships between major housing market indicators used in the logistic regression model. Strong relationships between transaction speed, sale-to-list ratio, and market heat indicators supported the inclusion of interaction effects in the final model.

<img width="711" height="516" alt="image" src="https://github.com/user-attachments/assets/0b2265c0-813e-400a-b7ab-70cfc23697e8" />

### ROC Curve
Receiver Operating Characteristic (ROC) curve for the final logistic regression model. The model achieved an AUC of approximately 0.77, indicating moderate classification performance.

<img width="711" height="516" alt="image" src="https://github.com/user-attachments/assets/7957d727-7959-4a9b-bff7-a81afe957946" />


## Tools
Analysis and modeling were completed entirely in R using statistical and machine learning libraries.
- R
- dplyr
- ggplot2
- pROC
- car
- tidyr

## Files
- `housing_market_classification.R` — main analysis and modeling workflow
- `Data/` — datasets used in the analysis
