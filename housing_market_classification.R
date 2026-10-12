# Zillow Housing Market Classification
# Author: Christian Lopez
# Goal: classify a metro-month as above the historical training-period median
# month-over-month ZHVI growth, using contemporaneous market indicators.

# Libraries ####
library(readr)
library(dplyr)
library(tidyr)
library(pROC)
library(corrplot)
library(car)
library(ggplot2)

# Data loading and reshaping ####
start_date <- as.Date("2018-01-01")
cutoff_date <- as.Date("2022-01-01")

to_long <- function(path, value_name) {
  read_csv(path, show_col_types = FALSE) %>%
    filter(RegionType == "msa") %>%
    pivot_longer(
      cols = starts_with("20"),
      names_to = "date",
      values_to = value_name
    ) %>%
    mutate(date = as.Date(date)) %>%
    select(RegionID, date, all_of(value_name))
}

# Read ZHVI independently so growth is calculated on consecutive calendar months
# BEFORE other datasets are joined or rows with missing predictors are removed.
zhvi <- read_csv(
  "Data/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv",
  show_col_types = FALSE
)

zhvi_long <- zhvi %>%
  filter(RegionType == "msa") %>%
  pivot_longer(cols = starts_with("20"), names_to = "date", values_to = "price") %>%
  mutate(date = as.Date(date)) %>%
  select(RegionID, region = RegionName, date, price) %>%
  arrange(RegionID, date) %>%
  group_by(RegionID) %>%
  mutate(
    previous_date = lag(date),
    price_lag = lag(price)
  ) %>%
  ungroup()

# Vectorized calendar-month check; varying month lengths do not matter.
# Months are encoded as year*12 + month, avoiding varying month lengths.
zhvi_long <- zhvi_long %>%
  mutate(
    month_index = as.integer(format(date, "%Y")) * 12L +
      as.integer(format(date, "%m")),
    previous_month_index = as.integer(format(previous_date, "%Y")) * 12L +
      as.integer(format(previous_date, "%m")),
    consecutive_month = !is.na(previous_date) &
      month_index - previous_month_index == 1L,
    price_growth = if_else(
      consecutive_month & !is.na(price_lag) & price_lag > 0,
      (price - price_lag) / price_lag * 100,
      NA_real_
    )
  ) %>%
  select(RegionID, region, date, price, price_growth) %>%
  filter(date >= start_date)

listings_long <- to_long("Data/Metro_new_listings_uc_sfrcondo_month.csv", "new_listings")
inventory_long <- to_long("Data/Metro_invt_fs_uc_sfrcondo_month.csv", "inventory")
days_long <- to_long("Data/Metro_med_doz_pending_uc_sfrcondo_month.csv", "days_pending")
sale_to_list_long <- to_long("Data/Metro_median_sale_to_list_uc_sfrcondo_month.csv", "sale_to_list")
heat_long <- to_long("Data/Metro_market_temp_index_uc_sfrcondo_month.csv", "market_heat_index")

# Merge on metro ID and month, retaining only months covered by every data source.
df <- zhvi_long %>%
  inner_join(listings_long, by = c("RegionID", "date")) %>%
  inner_join(inventory_long, by = c("RegionID", "date")) %>%
  inner_join(days_long, by = c("RegionID", "date")) %>%
  inner_join(sale_to_list_long, by = c("RegionID", "date")) %>%
  inner_join(heat_long, by = c("RegionID", "date"))

# Census region is derived from the state abbreviation in the metro name.
# Multi-state metros are assigned according to the final state abbreviation.
df <- df %>%
  mutate(
    state = sub(".*,\\s*", "", region),
    region_census = case_when(
      state %in% c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA") ~ "Northeast",
      state %in% c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD") ~ "Midwest",
      state %in% c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX") ~ "South",
      state %in% c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA") ~ "West",
      TRUE ~ NA_character_
    ),
    region_census = factor(region_census)
  ) %>%
  select(-state)

# Complete-case analysis: conclusions apply to metro-months with sufficient
# Zillow data coverage, not necessarily all US housing markets.
df_model <- df %>%
  drop_na(price_growth, price, new_listings, inventory, days_pending,
          sale_to_list, market_heat_index, region_census) %>%
  filter(price > 0, inventory > 0, days_pending > 0)

# Chronological split before target threshold, diagnostics, or feature selection ####
train <- df_model %>% filter(date < cutoff_date)
test <- df_model %>% filter(date >= cutoff_date)

# Freeze the outcome definition using training data only.
median_growth <- median(train$price_growth)
train <- train %>% mutate(hot = as.integer(price_growth > median_growth))
test <- test %>% mutate(hot = as.integer(price_growth > median_growth))

cat("Training observations:", nrow(train), "\n")
cat("Testing observations:", nrow(test), "\n")
cat("Training-period median monthly growth (%):", round(median_growth, 4), "\n")
cat("Training class distribution:\n")
print(table(train$hot))
cat("Testing class distribution:\n")
print(table(test$hot))

# Training-only multicollinearity analysis ####
initial_vars <- train %>%
  select(price, new_listings, inventory, days_pending, sale_to_list, market_heat_index)
corr_matrix <- cor(initial_vars, use = "complete.obs")
corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8)

model_check <- glm(
  hot ~ price + new_listings + inventory + days_pending +
    sale_to_list + market_heat_index + region_census,
  data = train, family = binomial
)
cat("Initial training-only VIF:\n")
print(vif(model_check))

# Evaluate the specification without new_listings.
# The choice must be justified by the training-only VIF results above.
model_check_reduced <- glm(
  hot ~ price + inventory + days_pending +
    sale_to_list + market_heat_index + region_census,
  data = train, family = binomial
)
cat("Reduced training-only VIF:\n")
print(vif(model_check_reduced))

reduced_vars <- train %>%
  select(price, inventory, days_pending, sale_to_list, market_heat_index)

corrplot(
  cor(reduced_vars),
  method = "color",
  type = "upper",
  tl.cex = 0.5,
  cl.cex = 0.5,
  mar = c(0, 0, 0, 0)
)

# Training-only functional-form exploration ####
# Fitted logits are exploratory plots, not a formal test of logit linearity.
train$exploratory_logit <- predict(model_check_reduced, type = "link")
plot_with_smoother <- function(x, y, label) {
  plot(x, y, main = label, xlab = label, ylab = "Fitted log-odds")
  lines(lowess(x, y), col = "red")
}
plot_with_smoother(train$price, train$exploratory_logit, "Price")
plot_with_smoother(train$inventory, train$exploratory_logit, "Inventory")
plot_with_smoother(train$days_pending, train$exploratory_logit, "Days pending")

# Log transformations do not require estimating parameters from the test set.
train <- train %>%
  mutate(log_price = log(price),
         log_inventory = log(inventory),
         log_days_pending = log(days_pending))
test <- test %>%
  mutate(log_price = log(price),
         log_inventory = log(inventory),
         log_days_pending = log(days_pending))

plot_with_smoother(train$log_price, train$exploratory_logit, "Log(price)")
plot_with_smoother(train$log_inventory, train$exploratory_logit, "Log(inventory)")
plot_with_smoother(train$log_days_pending, train$exploratory_logit, "Log(days pending)")

# Baseline model trained only on pre-2022 observations ####
model_final2 <- glm(
  hot ~ log_price + log_inventory + log_days_pending +
    sale_to_list + market_heat_index + region_census,
  data = train, family = binomial
)
cat("Baseline training VIF:\n")
print(vif(model_final2))

# Training-only diagnostics
cooks_d <- cooks.distance(model_final2)
plot(cooks_d, type = "h", main = "Training Cook's distance", ylab = "Cook's D")
abline(h = 4 / length(cooks_d), col = "red")
lev <- hatvalues(model_final2)
plot(lev, type = "h", main = "Training leverage", ylab = "Hat values")
abline(h = 2 * mean(lev), col = "red")
res <- residuals(model_final2, type = "deviance")
plot(res, main = "Training deviance residuals", ylab = "Residuals")
abline(h = c(-3, 3), col = "red")

# Training-only interaction comparisons ####
model_interact <- glm(
  hot ~ log_price + log_inventory + log_days_pending +
    sale_to_list * market_heat_index + region_census,
  data = train, family = binomial
)
model_interact2 <- glm(
  hot ~ log_price + log_inventory * market_heat_index +
    log_days_pending + sale_to_list + region_census,
  data = train, family = binomial
)
model_interact3 <- glm(
  hot ~ log_price + log_inventory +
    log_days_pending * market_heat_index + sale_to_list + region_census,
  data = train, family = binomial
)
model_final3 <- glm(
  hot ~ log_price + log_inventory + log_days_pending +
    sale_to_list * market_heat_index +
    log_days_pending * market_heat_index + region_census,
  data = train, family = binomial
)

cat("Training AIC comparisons:\n")
print(AIC(model_final2, model_interact, model_interact2,
          model_interact3, model_final3))
cat("Likelihood-ratio tests (training data):\n")
print(anova(model_final2, model_interact, test = "Chisq"))
print(anova(model_final2, model_interact2, test = "Chisq"))
print(anova(model_final2, model_interact3, test = "Chisq"))
print(anova(model_final2, model_final3, test = "Chisq"))

# Center interaction variables using training means only ####
mu_heat <- mean(train$market_heat_index)
mu_sale <- mean(train$sale_to_list)
mu_days <- mean(train$log_days_pending)

center_predictors <- function(data) {
  data %>% mutate(
    market_heat_c = market_heat_index - mu_heat,
    sale_to_list_c = sale_to_list - mu_sale,
    log_days_pending_c = log_days_pending - mu_days
  )
}
train <- center_predictors(train)
test <- center_predictors(test)

# Final model ####
model_final4 <- glm(
  hot ~ log_price + log_inventory + log_days_pending_c +
    sale_to_list_c * market_heat_c +
    log_days_pending_c * market_heat_c + region_census,
  data = train, family = binomial
)
cat("Final model summary:\n")
print(summary(model_final4))
cat("Final training VIF/GVIF:\n")
print(vif(model_final4))
cat("Final training AIC:", AIC(model_final4), "\n")

# One-time out-of-sample evaluation ####
pred <- predict(model_final4, newdata = test, type = "response")
pred_class <- as.integer(pred > 0.5)
cm <- table(
  Predicted = factor(pred_class, levels = c(0, 1)),
  Actual = factor(test$hot, levels = c(0, 1))
)
print(cm)
TN <- cm["0", "0"]
FP <- cm["1", "0"]
FN <- cm["0", "1"]
TP <- cm["1", "1"]
accuracy <- (TP + TN) / sum(cm)
sensitivity <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_
specificity <- if ((TN + FP) > 0) TN / (TN + FP) else NA_real_
roc_obj <- roc(response = test$hot, predictor = pred,
               levels = c(0, 1), direction = "<", quiet = TRUE)
auc_value <- as.numeric(auc(roc_obj))
plot(roc_obj, main = "Test ROC")

cat(sprintf("Test accuracy: %.3f\n", accuracy))
cat(sprintf("Test sensitivity: %.3f\n", sensitivity))
cat(sprintf("Test specificity: %.3f\n", specificity))
cat(sprintf("Test ROC AUC: %.3f\n", auc_value))

# Interpretation notes:
# - This is a same-month classification model, not a forecast of future growth.
# - The target is above the PRE-2022 training median, applied consistently to test.
# - Missing market indicators restrict the analysis to complete metro-months.

