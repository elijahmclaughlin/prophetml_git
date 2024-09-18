library(e1071)
library(caret)
library(class)
library(xgboost)
library(randomForest)

# Simple logistic model
logistic_model_home <- glm(
  home_team_win ~ home_team_grade + away_team_grade + avg_home_grade + avg_away_grade + home_offense + away_offense + home_defense + away_defense
  + home_offense_away_defense_interaction + home_grade_away_grade_interaction + home_team_grade_diff + away_team_grade_diff,
  data = nfl_viable, 
  family = binomial
)
summary(logistic_model_home)
exp(coef(logistic_model_home))

# Split the data into training and testing sets
train_data <- nfl_viable %>% filter(season < 2021)
test_data <- nfl_viable %>% filter(season >= 2021)

# Predict on the test set
test_data$predicted_prob <- predict(logistic_model_home, newdata = test_data, type = "response")
test_data$predicted_win <- ifelse(test_data$predicted_prob > 0.5, 1, 0)
head(test_data)

# Confusion Matrix
confusion_matrix <- confusionMatrix(
  factor(test_data$predicted_win), 
  factor(test_data$home_team_win)
)
print(confusion_matrix)

#### Naive Bayes ####
naive_bayes_model_home <- naiveBayes(
  home_team_win ~  home_team_grade + away_team_grade + avg_home_grade + avg_away_grade + home_offense + away_offense + home_defense + away_defense
  + home_offense_away_defense_interaction + home_grade_away_grade_interaction + home_team_grade_diff + away_team_grade_diff,
  data = train_data
)

test_data$predicted_prob_nb <- predict(naive_bayes_model_home, newdata = test_data, type = "raw")[, 2]

test_data$predicted_win_nb <- ifelse(test_data$predicted_prob_nb > 0.5, 1, 0)

head(test_data)

# confusion matrix
confusion_matrix <- confusionMatrix(
  factor(test_data$predicted_win_nb), 
  factor(test_data$home_team_win)
)
print(confusion_matrix)

### SVM home ###

svm_model_home <- svm(
  home_team_win ~ home_team_grade + away_team_grade + avg_home_grade + avg_away_grade + home_offense + away_offense + home_defense + away_defense
  + home_offense_away_defense_interaction + home_grade_away_grade_interaction + home_team_grade_diff + away_team_grade_diff,
  data = train_data,
  type = "C-classification",
  kernel = "radial",
  cost = 10,
  probability = TRUE
)

test_data$predicted_prob <- predict(svm_model_home, newdata = test_data, probability = TRUE)
test_data$predicted_win <- ifelse(attr(test_data$predicted_prob, "probabilities")[,2] > 0.5, 1, 0)

confusion_matrix <- confusionMatrix(
  factor(test_data$predicted_win),
  factor(test_data$home_team_win)
)
print(confusion_matrix)

######## Ensemble model #########

# Logistic Regression Predictions
test_data$predicted_prob_logistic_home <- predict(logistic_model_home, newdata = test_data, type = "response")

# Naive Bayes Predictions
test_data$predicted_prob_nb_home <- predict(naive_bayes_model_home, newdata = test_data, type = "raw")[, 2]

# SVM Predictions
test_data$predicted_prob_svm_home <- predict(svm_model_home, newdata = test_data, probability = TRUE)
test_data$predicted_prob_svm_home <- attr(test_data$predicted_prob_svm_home, "probabilities")[, 2]

mean_probability = (test_data$predicted_prob_logistic_home + test_data$predicted_prob_nb_home + test_data$predicted_prob_svm_home) / 3

#### XGBoost

ensemble_data <- data.frame(
  logistic = test_data$predicted_prob_logistic_home,
  naive_bayes = test_data$predicted_prob_nb_home,
  svm = test_data$predicted_prob_svm_home,
  mean_prob = mean_probability,
  actual = as.numeric(test_data$home_team_win)
)

# Convert the data to a DMatrix
X <- as.matrix(ensemble_data[, c("logistic", "naive_bayes", "svm", "mean_prob")])
y <- ensemble_data$actual

dtrain <- xgb.DMatrix(data = X, label = y)

# params
xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 5,
  eta = 0.01,
  subsample = 0.8,
  colsample_bytree = 0.7,
  min_child_weight = 1,
  gamma = 0.1,
  lambda = 2,
  alpha = 1,
  scale_pos_weight = sum(y == 0) / sum(y == 1)
)

# cross validation
n_folds <- 5
cv_results <- xgb.cv(
  params = xgb_params,
  data = dtrain,
  nrounds = 10000,
  nfold = n_folds,
  verbose = 1,
  showsd = TRUE,
  stratified = TRUE,
  early_stopping_rounds = 7500,
  maximize = FALSE,
)
best_nrounds <- cv_results$best_ntreelimit

# Train the final XGBoost model
xgb_model <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = list(train = dtrain),
  verbose = 0
)

# predictions on the test data using the trained XGBoost model
ensemble_data$final_prob_xgb <- predict(xgb_model, newdata = X)
ensemble_data$final_prediction_xgb <- ifelse(ensemble_data$final_prob_xgb > 0.5, 1, 0)

# confusion matrix
confusion_matrix_xgb <- confusionMatrix(
  factor(ensemble_data$final_prediction_xgb),
  factor(ensemble_data$actual)
)
print(confusion_matrix_xgb)

# log loss for the last boosting round
final_logloss <- cv_results$evaluation_log$test_logloss_mean[nrow(cv_results$evaluation_log)]
print(final_logloss)

# using the glm function to fit a logistic regression model for Platt scaling
platt_model <- glm(
  actual ~ final_prob_xgb,
  data = ensemble_data,
  family = binomial(link = "logit")
)

ensemble_data$calibrated_prob_platt <- predict(platt_model, newdata = ensemble_data, type = "response")

log_loss <- function(actual, predicted) {
  epsilon <- 1e-15
  predicted <- pmin(pmax(predicted, epsilon), 1 - epsilon)
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

# Calculate log loss for Platt-calibrated model
log_loss_platt <- log_loss(ensemble_data$actual, ensemble_data$calibrated_prob_platt)
print(log_loss_platt)

get_calibration_data <- function(predicted_probs, actuals, n_bins = 10) {
  data <- data.frame(
    predicted_probs = predicted_probs,
    actuals = actuals
  )
  
  data$bin <- cut(data$predicted_probs, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)
  
  calibration_data <- data %>%
    group_by(bin) %>%
    summarise(
      mean_predicted_prob = mean(predicted_probs),
      actual_event_rate = mean(actuals)
    )
  
  return(calibration_data)
}

# Calibration data for XGBoost probabilities
calibration_data_xgb <- get_calibration_data(
  predicted_probs = ensemble_data$final_prob_xgb,
  actuals = ensemble_data$actual
)

# Calibration data for Platt-calibrated probabilities
calibration_data_platt <- get_calibration_data(
  predicted_probs = ensemble_data$calibrated_prob_platt,
  actuals = ensemble_data$actual
)

# Plot calibration curve
ggplot() +
  geom_line(data = calibration_data_xgb, aes(x = mean_predicted_prob, y = actual_event_rate, color = "XGBoost"), size = 1) +
  
  geom_line(data = calibration_data_platt, aes(x = mean_predicted_prob, y = actual_event_rate, color = "Platt Scaling"), size = 1) +
  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  
  labs(x = "Mean Predicted Probability", y = "Actual Event Rate",
       title = "Calibration Plot",
       color = "Model") +
  
  theme_minimal() +
  scale_color_manual(values = c("XGBoost" = "darkblue", "Platt Scaling" = "red"))

# ensure XGBoost predictions are stored in ensemble_data
ensemble_data$final_prediction_xgb <- ifelse(ensemble_data$final_prob_xgb > 0.5, 1, 0)

# weekly accuracy
ensemble_data$week <- test_data$week

accuracy_by_week_xgb <- ensemble_data %>%
  group_by(week) %>%
  summarize(
    correct_predictions = sum(final_prediction_xgb == actual),  # Use XGBoost predictions
    total_predictions = n(),
    accuracy = correct_predictions / total_predictions
  )

# accuracy by week
print(accuracy_by_week_xgb, n=22)

ggplot(accuracy_by_week_xgb, aes(x = week, y = accuracy)) +
  geom_line() +
  geom_point() +
  labs(title = "XGBoost Model Accuracy by Week", x = "Week", y = "Accuracy") +
  theme_minimal()

# cm plot
confusion_matrix <- confusionMatrix(factor(ensemble_data$final_prediction_tree), factor(ensemble_data$actual))
cm_table <- as.data.frame(confusion_matrix$table)
colnames(cm_table) <- c("Prediction", "Reference", "Count")

ggplot(data = cm_table, aes(x = Reference, y = Prediction, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), vjust = 1.5, color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "forestgreen") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()

########### XGBoost regression ##############

# Prepare data for XGBoost home score model
X_home <- as.matrix(points_pred[, c("home_team_grade", "away_team_grade", "home_offense", "away_offense", 
                                    "avg_home_grade", "avg_away_grade")])
y_home <- points_pred$home_score

X_away <- as.matrix(points_pred[, c("home_team_grade", "away_team_grade", "home_offense", "away_offense", 
                                    "avg_home_grade", "avg_away_grade")])
y_away <- points_pred$away_score

# convert to dmatrix
dtrain_home <- xgb.DMatrix(data = X_home, label = y_home)
dtrain_away <- xgb.DMatrix(data = X_away, label = y_away)

params <- list(
  objective = "reg:squarederror",
  booster = "gbtree",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6
)

# home score
xgb_home_model <- xgboost(
  params = params,
  data = dtrain_home,
  nrounds = 1000,
  verbose = 0
)

# away score
xgb_away_model <- xgboost(
  params = params,
  data = dtrain_away,
  nrounds = 1000,
  verbose = 0
)

# predictions for home and away scores
points_pred$predicted_home_score <- predict(xgb_home_model, newdata = X_home)
points_pred$predicted_away_score <- predict(xgb_away_model, newdata = X_away)
head(points_pred[, c("home_score", "predicted_home_score", "away_score", "predicted_away_score")])

# an interesting idea - classification for spread and total score predictions
########### spread win

# logistic
logistic_model_ats <- glm(
  spread_win ~ home_team_grade + away_team_grade + avg_home_grade + avg_away_grade + home_offense + away_offense + home_defense + away_defense
  + home_offense_away_defense_interaction + home_grade_away_grade_interaction + home_team_grade_diff + away_team_grade_diff,
  data = nfl_viable, 
  family = binomial
)
summary(logistic_model_ats)
exp(coef(logistic_model_ats))

# training and testing sets
train_data <- nfl_viable %>% filter(season < 2021)
test_data <- nfl_viable %>% filter(season >= 2021)

# Predict on the test set
test_data$predicted_prob <- predict(logistic_model_ats, newdata = test_data, type = "response")
test_data$predicted_win <- ifelse(test_data$predicted_prob > 0.5, 1, 0)
head(test_data)

# Confusion Matrix
confusion_matrix <- confusionMatrix(
  factor(test_data$predicted_win), 
  factor(test_data$home_team_win)
)
print(confusion_matrix)

#### Naive Bayes ####
naive_bayes_model_ats <- naiveBayes(
  spread_win ~  home_team_grade + away_team_grade + avg_home_grade + avg_away_grade + home_offense + away_offense + home_defense + away_defense
  + home_offense_away_defense_interaction + home_grade_away_grade_interaction + home_team_grade_diff + away_team_grade_diff,
  data = train_data
)

test_data$predicted_prob_nb <- predict(naive_bayes_model_ats, newdata = test_data, type = "raw")[, 2]

test_data$predicted_win_nb <- ifelse(test_data$predicted_prob_nb > 0.5, 1, 0)

head(test_data)

# confusion matrix
confusion_matrix <- confusionMatrix(
  factor(test_data$predicted_win_nb), 
  factor(test_data$home_team_win)
)
print(confusion_matrix)

### SVM home ###

svm_model_ats <- svm(
  spread_win ~ home_team_grade + away_team_grade + avg_home_grade + avg_away_grade + home_offense + away_offense + home_defense + away_defense
  + home_offense_away_defense_interaction + home_grade_away_grade_interaction + home_team_grade_diff + away_team_grade_diff,
  data = train_data,
  type = "C-classification",
  kernel = "radial",
  cost = 10,
  probability = TRUE
)

test_data$predicted_prob <- predict(svm_model_ats, newdata = test_data, probability = TRUE)
test_data$predicted_win <- ifelse(attr(test_data$predicted_prob, "probabilities")[,2] > 0.5, 1, 0)

confusion_matrix <- confusionMatrix(
  factor(test_data$predicted_win),
  factor(test_data$home_team_win)
)
print(confusion_matrix)

######## Ensemble model #########

# Logistic Regression Predictions
test_data$predicted_prob_logistic_ats <- predict(logistic_model_ats, newdata = test_data, type = "response")

# Naive Bayes Predictions
test_data$predicted_prob_nb_ats <- predict(naive_bayes_model_ats, newdata = test_data, type = "raw")[, 2]

# SVM Predictions
test_data$predicted_prob_svm_ats <- predict(svm_model_ats, newdata = test_data, probability = TRUE)
test_data$predicted_prob_svm_ats <- attr(test_data$predicted_prob_svm_ats, "probabilities")[, 2]

ats_mean = (test_data$predicted_prob_logistic_ats + test_data$predicted_prob_nb_ats + test_data$predicted_prob_svm_ats) / 3

#### XGBoost

ensemble_data <- data.frame(
  logistic_ats = test_data$predicted_prob_logistic_ats,
  naive_bayes_ats = test_data$predicted_prob_nb_ats,
  svm_ats = test_data$predicted_prob_svm_ats,
  mean_prob = ats_mean,
  actual = as.numeric(test_data$spread_win)
)

X <- as.matrix(ensemble_data[, c("logistic_ats", "naive_bayes_ats", "svm_ats", "mean_prob")])
y <- ensemble_data$actual

dtrain <- xgb.DMatrix(data = X, label = y)

# params
xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 5,
  eta = 0.01,
  subsample = 0.8,
  colsample_bytree = 0.7,
  min_child_weight = 1,
  gamma = 0.1,
  lambda = 2,
  alpha = 1,
  scale_pos_weight = sum(y == 0) / sum(y == 1)
)

# cross validation
n_folds <- 5
cv_results <- xgb.cv(
  params = xgb_params,
  data = dtrain,
  nrounds = 10000,
  nfold = n_folds,
  verbose = 1,
  showsd = TRUE,
  stratified = TRUE,
  early_stopping_rounds = 7500,
  maximize = FALSE,
)
best_nrounds <- cv_results$best_ntreelimit

# Train the final XGBoost model
xgb_model_ats <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = list(train = dtrain),
  verbose = 0
)

# predictions on the test data using the trained XGBoost model
ensemble_data$final_prob_xgb <- predict(xgb_model_ats, newdata = X)
ensemble_data$final_prediction_xgb <- ifelse(ensemble_data$final_prob_xgb > 0.5, 1, 0)

# confusion matrix
confusion_matrix_xgb <- confusionMatrix(
  factor(ensemble_data$final_prediction_xgb),
  factor(ensemble_data$actual)
)
print(confusion_matrix_xgb)

# log loss
final_logloss <- cv_results$evaluation_log$test_logloss_mean[nrow(cv_results$evaluation_log)]
print(final_logloss)

# Platt scaling
platt_model_ats <- glm(
  actual ~ final_prob_xgb,
  data = ensemble_data,
  family = binomial(link = "logit")
)

ensemble_data$calibrated_prob_platt <- predict(platt_model_ats, newdata = ensemble_data, type = "response")

log_loss <- function(actual, predicted) {
  epsilon <- 1e-15  # Small value to avoid log(0)
  predicted <- pmin(pmax(predicted, epsilon), 1 - epsilon)
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

# log loss for Platt-calibrated model
log_loss_platt <- log_loss(ensemble_data$actual, ensemble_data$calibrated_prob_platt)
print(log_loss_platt)

get_calibration_data <- function(predicted_probs, actuals, n_bins = 10) {
  data <- data.frame(
    predicted_probs = predicted_probs,
    actuals = actuals
  )
  
  data$bin <- cut(data$predicted_probs, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)
  
  calibration_data <- data %>%
    group_by(bin) %>%
    summarise(
      mean_predicted_prob = mean(predicted_probs),
      actual_event_rate = mean(actuals)
    )
  
  return(calibration_data)
}

# Calibration data for XGBoost probabilities
calibration_data_xgb <- get_calibration_data(
  predicted_probs = ensemble_data$final_prob_xgb,
  actuals = ensemble_data$actual
)

# Calibration data for Platt-calibrated probabilities
calibration_data_platt <- get_calibration_data(
  predicted_probs = ensemble_data$calibrated_prob_platt,
  actuals = ensemble_data$actual
)

# Plot calibration curve
ggplot() +
  geom_line(data = calibration_data_xgb, aes(x = mean_predicted_prob, y = actual_event_rate, color = "XGBoost"), size = 1) +
  
  geom_line(data = calibration_data_platt, aes(x = mean_predicted_prob, y = actual_event_rate, color = "Platt Scaling"), size = 1) +
  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  
  labs(x = "Mean Predicted Probability", y = "Actual Event Rate",
       title = "Calibration Plot",
       color = "Model") +
  
  theme_minimal() +
  scale_color_manual(values = c("XGBoost" = "darkblue", "Platt Scaling" = "red"))

########### total score win

# logistic
logistic_model_ts <- glm(
  total_win ~ home_team_grade + away_team_grade + avg_home_grade + avg_away_grade + home_offense + away_offense + home_defense + away_defense
  + home_offense_away_defense_interaction + home_grade_away_grade_interaction + home_team_grade_diff + away_team_grade_diff,
  data = nfl_viable, 
  family = binomial
)
summary(logistic_model_ts)
exp(coef(logistic_model_ts))

# training and testing sets
train_data <- nfl_viable %>% filter(season < 2021)
test_data <- nfl_viable %>% filter(season >= 2021)

# Predict on the test set
test_data$predicted_prob <- predict(logistic_model_ts, newdata = test_data, type = "response")
test_data$predicted_win <- ifelse(test_data$predicted_prob > 0.5, 1, 0)
head(test_data)

# Confusion Matrix
confusion_matrix <- confusionMatrix(
  factor(test_data$predicted_win), 
  factor(test_data$home_team_win)
)
print(confusion_matrix)

#### Naive Bayes ####
naive_bayes_model_ts <- naiveBayes(
  total_win ~  home_team_grade + away_team_grade + avg_home_grade + avg_away_grade + home_offense + away_offense + home_defense + away_defense
  + home_offense_away_defense_interaction + home_grade_away_grade_interaction + home_team_grade_diff + away_team_grade_diff,
  data = train_data
)

test_data$predicted_prob_nb <- predict(naive_bayes_model_ts, newdata = test_data, type = "raw")[, 2]

test_data$predicted_win_nb <- ifelse(test_data$predicted_prob_nb > 0.5, 1, 0)

head(test_data)

# confusion matrix
confusion_matrix <- confusionMatrix(
  factor(test_data$predicted_win_nb), 
  factor(test_data$home_team_win)
)
print(confusion_matrix)

### SVM home ###

svm_model_ts <- svm(
  total_win ~ home_team_grade + away_team_grade + avg_home_grade + avg_away_grade + home_offense + away_offense + home_defense + away_defense
  + home_offense_away_defense_interaction + home_grade_away_grade_interaction + home_team_grade_diff + away_team_grade_diff,
  data = train_data,
  type = "C-classification",
  kernel = "radial",
  cost = 10,
  probability = TRUE         # Enable probability predictions
)

test_data$predicted_prob <- predict(svm_model_ts, newdata = test_data, probability = TRUE)
test_data$predicted_win <- ifelse(attr(test_data$predicted_prob, "probabilities")[,2] > 0.5, 1, 0)

confusion_matrix <- confusionMatrix(
  factor(test_data$predicted_win),
  factor(test_data$home_team_win)
)
print(confusion_matrix)

######## Ensemble model #########

# Logistic Regression Predictions
test_data$predicted_prob_logistic_ts <- predict(logistic_model_ts, newdata = test_data, type = "response")

# Naive Bayes Predictions
test_data$predicted_prob_nb_ts <- predict(naive_bayes_model_ts, newdata = test_data, type = "raw")[, 2]

# SVM Predictions
test_data$predicted_prob_svm_ts <- predict(svm_model_ts, newdata = test_data, probability = TRUE)
test_data$predicted_prob_svm_ts <- attr(test_data$predicted_prob_svm_ts, "probabilities")[, 2]

prob_ints = test_data$predicted_prob_logistic_ts * test_data$predicted_prob_nb_ts * test_data$predicted_prob_svm_ts

#### XGBoost

ensemble_data <- data.frame(
  logistic_ts = test_data$predicted_prob_logistic_ts,
  naive_bayes_ts = test_data$predicted_prob_nb_ts,
  svm_ts = test_data$predicted_prob_svm_ts,
  prob_int = prob_ints,
  actual = as.numeric(test_data$total_win)
)

# Step 2: Convert the data to a DMatrix (the required data structure for XGBoost)
X <- as.matrix(ensemble_data[, c("logistic_ts", "naive_bayes_ts", "svm_ts", "prob_int")])
y <- ensemble_data$actual

dtrain <- xgb.DMatrix(data = X, label = y)

# params
xgb_params <- list(
  objective = "binary:logistic",  # Binary classification
  eval_metric = "logloss",        # Evaluation metric: log loss
  max_depth = 5,                  # Increased depth for more complex interactions
  eta = 0.01,                     # Lower learning rate for finer adjustments
  subsample = 0.8,                # Subsampling ratio
  colsample_bytree = 0.7,         # Column sampling ratio per tree
  min_child_weight = 1,           # Regularization
  gamma = 0.1,                    # Minimum loss reduction
  lambda = 1,                     # L2 regularization
  alpha = 0,                      # L1 regularization
  scale_pos_weight = sum(y == 0) / sum(y == 1)
)

# cross validation
n_folds <- 5
cv_results <- xgb.cv(
  params = xgb_params,
  data = dtrain,
  nrounds = 500,              # Number of boosting rounds
  nfold = n_folds,            # Number of folds for cross-validation
  verbose = 1,                # Set to 1 for more detailed logging
  showsd = TRUE,              # Whether to show standard deviation of evaluation metric across folds
  stratified = TRUE,          # Whether to stratify folds based on label distribution
  early_stopping_rounds = 25, # Stop if performance doesn't improve for 10 rounds
  maximize = FALSE,           # Whether the metric should be maximized or minimized
)
best_nrounds <- cv_results$best_ntreelimit

# Train the final XGBoost model
xgb_model_ts <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = best_nrounds,  # Use the best number of rounds from cross-validation
  watchlist = list(train = dtrain),
  verbose = 0              # Suppress training output
)

# Make predictions on the test data using the trained XGBoost model
ensemble_data$final_prob_xgb <- predict(xgb_model_ts, newdata = X)

# Convert predicted probabilities to binary predictions (win/loss)
ensemble_data$final_prediction_xgb <- ifelse(ensemble_data$final_prob_xgb > 0.5, 1, 0)

# Evaluate the model using a confusion matrix
confusion_matrix_xgb <- confusionMatrix(
  factor(ensemble_data$final_prediction_xgb),
  factor(ensemble_data$actual)
)
print(confusion_matrix_xgb)

# Extract the log loss for the last boosting round
final_logloss <- cv_results$evaluation_log$test_logloss_mean[nrow(cv_results$evaluation_log)]
print(final_logloss)

# Use the glm function to fit a logistic regression model for Platt scaling
platt_model_ts <- glm(
  actual ~ final_prob_xgb,      # Logistic regression on XGBoost probabilities
  data = ensemble_data,
  family = binomial(link = "logit")
)

ensemble_data$calibrated_prob_platt <- predict(platt_model_ts, newdata = ensemble_data, type = "response")

log_loss <- function(actual, predicted) {
  epsilon <- 1e-15  # Small value to avoid log(0)
  predicted <- pmin(pmax(predicted, epsilon), 1 - epsilon)
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

# Calculate log loss for Platt-calibrated model
log_loss_platt <- log_loss(ensemble_data$actual, ensemble_data$calibrated_prob_platt)
print(log_loss_platt)

get_calibration_data <- function(predicted_probs, actuals, n_bins = 10) {
  data <- data.frame(
    predicted_probs = predicted_probs,
    actuals = actuals
  )
  
  # Bin the probabilities into n_bins (e.g., 10)
  data$bin <- cut(data$predicted_probs, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)
  
  # Calculate the mean predicted probability and the actual event rate per bin
  calibration_data <- data %>%
    group_by(bin) %>%
    summarise(
      mean_predicted_prob = mean(predicted_probs),
      actual_event_rate = mean(actuals)
    )
  
  return(calibration_data)
}

# Calibration data for XGBoost probabilities
calibration_data_xgb <- get_calibration_data(
  predicted_probs = ensemble_data$final_prob_xgb,
  actuals = ensemble_data$actual
)

# Calibration data for Platt-calibrated probabilities
calibration_data_platt <- get_calibration_data(
  predicted_probs = ensemble_data$calibrated_prob_platt,
  actuals = ensemble_data$actual
)

# Plot calibration curve
ggplot() +
  # XGBoost model calibration
  geom_line(data = calibration_data_xgb, aes(x = mean_predicted_prob, y = actual_event_rate, color = "XGBoost"), size = 1) +
  
  # Platt model calibration
  geom_line(data = calibration_data_platt, aes(x = mean_predicted_prob, y = actual_event_rate, color = "Platt Scaling"), size = 1) +
  
  # Add perfect calibration line (y = x)
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  
  # Plot labels
  labs(x = "Mean Predicted Probability", y = "Actual Event Rate",
       title = "Calibration Plot",
       color = "Model") +
  
  # Adjust plot appearance
  theme_minimal() +
  scale_color_manual(values = c("XGBoost" = "darkblue", "Platt Scaling" = "red"))






