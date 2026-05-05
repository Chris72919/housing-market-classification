# Zillow Housing Market Classification
# Author: Christian Lopez
# Description:
# This project builds a logistic regression model to classify metro housing markets
# as above-median growth ("hot") using Zillow housing indicators.

# Libraries ####
library(readr)
library(dplyr)
library(tidyr)
library(pROC)
library(corrplot)
library(car)
library(ggplot2)

# Adding, altering, and merging datasets ####
# Adding datasets
zhvi <- read_csv("Data/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
listings <- read_csv("Data/Metro_new_listings_uc_sfrcondo_month.csv")
inventory <- read_csv("Data/Metro_invt_fs_uc_sfrcondo_month.csv")
days_pending <- read_csv("Data/Metro_med_doz_pending_uc_sfrcondo_month.csv")

# Inspecting datasets
# Has a primary key that can be used to merge (RegionID main) 
# (SizeRank and RegionName could be used as alt keys but unlikely to use)
head(zhvi) # Starts in 2001 instead of 2018 have to filter out
head(listings)
head(inventory)
head(days_pending)

# Only keeping metros 
zhvi <- zhvi %>% filter(RegionType == "msa")
listings <- listings %>% filter(RegionType == "msa")
inventory <- inventory %>% filter(RegionType == "msa")
days_pending <- days_pending %>% filter(RegionType == "msa")

# Long format (puts all dates under one column so that each row is one obs.)
# Likely a composite key of (RegionID, Date) now (Maybe RegionName can be used if unique)
zhvi_long <- zhvi %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "date",
    values_to = "price"
  )

listings_long <- listings %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "date",
    values_to = "new_listings"
  )

inventory_long <- inventory %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "date",
    values_to = "inventory"
  )

days_long <- days_pending %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "date",
    values_to = "days_pending"
  )

# Changing date class from character to date
zhvi_long$date <- as.Date(zhvi_long$date)
listings_long$date <- as.Date(listings_long$date)
inventory_long$date <- as.Date(inventory_long$date)
days_long$date <- as.Date(days_long$date)

# Giving all datasets same start date (should only change zhvi)
start_date <- as.Date("2018-01-01")

zhvi_long <- zhvi_long %>% filter(date >= start_date)
listings_long <- listings_long %>% filter(date >= start_date)
inventory_long <- inventory_long %>% filter(date >= start_date)
days_long <- days_long %>% filter(date >= start_date)

# Cleaning (removing unneeded columns) (SizeRank, RegionType, StateName)
zhvi_long <- zhvi_long %>% select(RegionID, RegionName, date, price)
listings_long <- listings_long %>% select(RegionID, RegionName, date, new_listings)
inventory_long <- inventory_long %>% select(RegionID, RegionName, date, inventory)
days_long <- days_long %>% select(RegionID, RegionName, date, days_pending)

# Merging with inner joins using composite key (RegionID, Date)
df <- zhvi_long %>%
  inner_join(listings_long, by = c("RegionID", "date")) %>%
  inner_join(inventory_long, by = c("RegionID", "date")) %>%
  inner_join(days_long, by = c("RegionID", "date"))

# Decided to add two new predictors will
sale_to_list_long <- read_csv("Data/Metro_median_sale_to_list_uc_sfrcondo_month.csv") %>%
  filter(RegionType == "msa") %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "date",
    values_to = "sale_to_list"
  ) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= start_date) %>%
  select(RegionID, date, sale_to_list)

heat_long <- read_csv("Data/Metro_market_temp_index_uc_sfrcondo_month.csv") %>%
  filter(RegionType == "msa") %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "date",
    values_to = "market_heat_index"
  ) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= start_date) %>%
  select(RegionID, date, market_heat_index)

df <- df %>%
  inner_join(sale_to_list_long, by = c("RegionID", "date")) %>%
  inner_join(heat_long, by = c("RegionID", "date"))

# Check
head(df) # Duplicated region name will clean

# Clean duplicates
# Retained regionname col.
df <- df %>%
  rename(region = RegionName.x)
# Drop others
df <- df %>%
  select(RegionID, region, date, price, new_listings, inventory, days_pending,
        sale_to_list, market_heat_index)

# Adding a region_census predictor
df <- df %>%
  mutate(
    state = sub(".*,\\s*", "", region),
    region_census = case_when(
      state %in% c("CT","ME","MA","NH","RI","VT","NJ","NY","PA") ~ "Northeast",
      state %in% c("IL","IN","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD") ~ "Midwest",
      state %in% c("DE","FL","GA","MD","NC","SC","VA","DC","WV","AL","KY","MS","TN","AR","LA","OK","TX") ~ "South",
      state %in% c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA") ~ "West",
      TRUE ~ NA_character_
    )
  )
df$region_census <- as.factor(df$region_census)

# removing state
df <- df %>% select(-state)

# Check
head(df) # Looks good
dim(df) # 50440 10
summary(df) # lots of NA's will have to look at the nature of them before 
            # removing as to be informed on what not having them means

# High-coverage macro metros are complete, but lower-activity or thin-market
# observations are partially unobserved

# Missing values are not randomly distributed but reflect Zillow’s data coverage
# constraints and reporting thresholds. Variables such as median sale-to-list
# ratio and days pending are only computed when sufficient underlying
# transaction volume is available within a metropolitan area-month.

# I will be removing the NA's with the idea that this model focuses on metros
# with sufficient market activity (high-liquidity housing markets where reliable
# transaction-based indicators exist)

# dropping nas
df_model <- df %>% drop_na()

head(df_model)
summary(df_model)

# Adding response ####
df_model <- df_model %>%
  arrange(RegionID, date) %>%
  group_by(RegionID) %>%
  mutate(
    price_lag = lag(price),
    price_growth = (price - price_lag) / price_lag * 100,
    hot = ifelse(price_growth > 0, 1, 0)
  ) %>%
  ungroup() %>%
  drop_na(price_lag)

# Checking response (imbalanced as most markets are experiencing growth)
table(df_model$hot)
mean(df_model$hot)

# Changing my response to median based. Changes question from Is growth positive?
# to Is growth above normal? or Is this metro-month performing better than a
# typical metro-month?
df_model <- df_model %>%
  mutate(
    hot = ifelse(
      price_growth > median(price_growth, na.rm = TRUE),
      1, 0
    )
  )

table(df_model$hot)
mean(df_model$hot) # balanced

# Multicollinearity ####
# Correlation matrix
vars <- df_model %>%
  select(price, new_listings, inventory, days_pending, sale_to_list, market_heat_index)

corr_matrix <- cor(vars, use = "complete.obs")

corr_matrix
corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8)

# VIF
model_check <- glm(
  hot ~ price + new_listings + inventory + days_pending +
    sale_to_list + market_heat_index + region_census,
  data = df_model,
  family = binomial
)

vif(model_check)

# Removing new listings due to multicollinearity issues
df_model <- df_model %>%
  select(-new_listings)

vars <- df_model %>%
  select(price, inventory, days_pending, sale_to_list, market_heat_index)
corr_matrix <- cor(vars, use = "complete.obs")
corr_matrix
corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8)

model_check <- glm(
  hot ~ price + inventory + days_pending +
    sale_to_list + market_heat_index + region_census,
  data = df_model,
  family = binomial
)
vif(model_check)

# No multicollinearity issues

# Linearity ####
df_model$logit <- log(
  predict(model_check, type = "response") /
    (1 - predict(model_check, type = "response"))
)

# try log
plot(df_model$price, df_model$logit,
     main = "Price vs Logit")
lines(lowess(df_model$price, df_model$logit), col="red")

# try log
plot(df_model$inventory, df_model$logit,
     main = "Inventory vs Logit")
lines(lowess(df_model$inventory, df_model$logit), col="red")

# try log
plot(df_model$days_pending, df_model$logit,
     main = "Days Pending vs Logit")
lines(lowess(df_model$days_pending, df_model$logit), col="red")

# logs
df_model$log_price <- log(df_model$price)
df_model$log_inventory <- log(df_model$inventory)
df_model$log_days_pending <- log(df_model$days_pending)

# Think improvement
plot(df_model$log_price, df_model$logit,
     main = "Log(Price) vs Logit")
lines(lowess(df_model$log_price, df_model$logit), col="red")

# Improved
plot(df_model$log_inventory, df_model$logit,
     main = "Log(Inventory) vs Logit")
lines(lowess(df_model$log_inventory, df_model$logit), col="red")

# Improved
plot(df_model$log_days_pending, df_model$logit,
     main = "Log(Days Pending) vs Logit")
lines(lowess(df_model$log_days_pending, df_model$logit), col="red")

# Final logistic regression model
model_final <- glm(
  hot ~ log_price + log_inventory + log_days_pending +
    sale_to_list + market_heat_index + region_census,
  data = df_model,
  family = binomial
)

# Outlier ####
# Cook
cooks_d <- cooks.distance(model_final)

plot(cooks_d, type = "h",
     main = "Cook's Distance",
     ylab = "Cook's D")

abline(h = 4/length(cooks_d), col = "red")

# Leverage
lev <- hatvalues(model_final)

plot(lev, type = "h",
     main = "Leverage",
     ylab = "Hat Values")

abline(h = 2*mean(lev), col = "red")

# Residuals
res <- residuals(model_final, type = "deviance")

plot(res,
     main = "Deviance Residuals",
     ylab = "Residuals")

abline(h = c(-3, 3), col = "red")

set.seed(1)

# Split ####
# Splitting based on time so future data won't be in training and past data won't be in testing
# (forgot to do earlier)

train <- df_model %>% filter(date < as.Date("2022-01-01")) # about 60%
test  <- df_model %>% filter(date >= as.Date("2022-01-01")) # about 40%

model_final2 <- glm(
  hot ~ log_price + log_inventory + log_days_pending +
    sale_to_list + market_heat_index + region_census,
  data = train,
  family = binomial
)

# Rechecks ####
# linearity recheck (linear enough)
train$logit <- log(
  predict(model_final2, type = "response") /
    (1 - predict(model_final2, type = "response"))
)

plot(train$log_price, train$logit)
lines(lowess(train$log_price, train$logit), col="red")

# multicollinearity recheck (good)
vif(model_final2)

# outlier recheck (looks good)
plot(model_final2)

# Interactions ####
# checking sale_to_list * market_heat_index, log_inventory * market_heat_index, and log_days_pending * market_heat_index
# sale and market
model_interact <- glm(
  hot ~ log_price + log_inventory + log_days_pending +
    sale_to_list * market_heat_index + region_census,
  data = train,
  family = binomial
)

# is both significant and lowers aic will add
AIC(model_final2, model_interact) # 18281.50
anova(model_final2, model_interact, test = "Chisq") 

model_interact2 <- glm(
  hot ~ log_price + log_inventory * market_heat_index +
    log_days_pending + sale_to_list + region_census,
  data = train,
  family = binomial
)

# not sig or aic lowering (not using)
AIC(model_final2, model_interact2)

anova(model_final2, model_interact2, test = "Chisq")

model_interact3 <- glm(
  hot ~ log_price + log_inventory + log_days_pending +
    log_days_pending * market_heat_index +
    sale_to_list + region_census,
  data = train,
  family = binomial
)

# both sig and lower aic (use)
AIC(model_final2, model_interact3) #18285.67

anova(model_final2, model_interact3, test = "Chisq")

model_final3 <- glm(
  hot ~ log_price + log_inventory + log_days_pending +
    sale_to_list * market_heat_index +
    log_days_pending * market_heat_index +
    region_census,
  data = train,
  family = binomial
)

AIC(model_final2, model_final3) # 18275.93 (lower than original and with just one int)
anova(model_final2, model_final3, test = "Chisq") # sig with two interactions

summary(model_final3)

# VIF is affected by interactions
vif(model_final3)

# Try centering
mu_heat <- mean(train$market_heat_index)
mu_sale <- mean(train$sale_to_list)
mu_days <- mean(train$log_days_pending)

train$market_heat_c <- train$market_heat_index - mu_heat
test$market_heat_c  <- test$market_heat_index - mu_heat

train$sale_to_list_c <- train$sale_to_list - mu_sale
test$sale_to_list_c  <- test$sale_to_list - mu_sale

train$log_days_pending_c <- train$log_days_pending - mu_days
test$log_days_pending_c  <- test$log_days_pending - mu_days

# Final Model ####
model_final4 <- glm(
  hot ~ log_price + log_inventory + log_days_pending_c +
    sale_to_list_c * market_heat_c +
    log_days_pending_c * market_heat_c +
    region_census,
  data = train,
  family = binomial
)

vif(model_final4) # fixed

summary(model_final4) # looks good still

AIC(model_final4) # 18275.93 good

# Evaluations ####
# acc, sens, spec
pred <- predict(model_final4, newdata = test, type = "response")
pred_class <- pred > 0.5

table(Predicted = pred > 0.5, Actual = test$hot)
cm <- table(Predicted = pred_class, Actual = test$hot)

TN <- cm[1,1]
FP <- cm[2,1]
FN <- cm[1,2]
TP <- cm[2,2]

accuracy <- (TP + TN) / sum(cm)
sensitivity <- TP / (TP + FN) 
specificity <- TN / (TN + FP)

accuracy # 0.715
sensitivity # 0.663
specificity # 0.744

# auc
roc_obj <- roc(test$hot, pred)
auc(roc_obj)

plot(roc_obj)

summary(model_final4)

# The final logistic regression model achieved approximately:
# - 71.5% test accuracy
# - 0.663 sensitivity
# - 0.744 specificity
# - 0.77 AUC

# Inventory levels, transaction speed, and market heat indicators
# were among the strongest predictors of housing market growth.

# Interaction effects improved model fit, suggesting that the effect
# of pricing competitiveness and pending days depends partly on overall
# market heat conditions.

# Overall, the model demonstrated moderate predictive ability while
# remaining interpretable and statistically stable.

