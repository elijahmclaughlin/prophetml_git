####### cfb model testing

# Simple logistic model
logistic_model_home_cfb <- glm(
  home_team_win ~ home_team_grade + away_team_grade + avg_home_grade + avg_away_grade + home_offense + away_offense + home_defense + away_defense
  + home_offense_away_defense_interaction + home_grade_away_grade_interaction + home_team_grade_diff + away_team_grade_diff,
  data = cfb_viable, 
  family = binomial
)
summary(logistic_model_home_cfb)
exp(coef(logistic_model_home_cfb))

# splitting into training and testing
train_data <- cfb_viable %>% filter(season < 2023)
test_data <- cfb_viable %>% filter(season >= 2023)

# predictions on the test set
test_data$predicted_prob <- predict(logistic_model_home_cfb, newdata = test_data, type = "response")
test_data$predicted_win <- ifelse(test_data$predicted_prob > 0.5, 1, 0)
head(test_data)

# Confusion Matrix
confusion_matrix <- confusionMatrix(
  factor(test_data$predicted_win), 
  factor(test_data$home_team_win)
)
print(confusion_matrix)

# accuracy by week
accuracy_by_week <- test_data %>%
  group_by(week) %>%
  summarize(
    correct_predictions = sum(home_team_win == predicted_win),
    total_games = n(),
    accuracy = correct_predictions / total_games
  )

# to view the accuracy by week
print(accuracy_by_week)

ggplot(accuracy_by_week, aes(x = week, y = accuracy)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Prediction Accuracy by Week for 2023 Regular Season",
    x = "Week",
    y = "Accuracy"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

#### Naive Bayes ####
naive_bayes_model_home_cfb <- naiveBayes(
  home_team_win ~  home_team_grade + away_team_grade + avg_home_grade + avg_away_grade + home_offense + away_offense + home_defense + away_defense
  + home_offense_away_defense_interaction + home_grade_away_grade_interaction + home_team_grade_diff + away_team_grade_diff,
  data = train_data
)

test_data$predicted_prob_nb <- predict(naive_bayes_model_home_cfb, newdata = test_data, type = "raw")[, 2]

test_data$predicted_win_nb <- ifelse(test_data$predicted_prob_nb > 0.5, 1, 0)

head(test_data)

# confusion matrix
confusion_matrix <- confusionMatrix(
  factor(test_data$predicted_win_nb), 
  factor(test_data$home_team_win)
)
print(confusion_matrix)

### SVM ###

svm_model_home_cfb <- svm(
  home_team_win ~ home_team_grade + away_team_grade + avg_home_grade + avg_away_grade + home_offense + away_offense + home_defense + away_defense
  + home_offense_away_defense_interaction + home_grade_away_grade_interaction + home_team_grade_diff + away_team_grade_diff,
  data = train_data,
  type = "C-classification",
  kernel = "radial",
  cost = 10,
  probability = TRUE
)

test_data$predicted_prob <- predict(svm_model_home_cfb, newdata = test_data, probability = TRUE)
test_data$predicted_win <- ifelse(attr(test_data$predicted_prob, "probabilities")[,2] > 0.5, 1, 0)

confusion_matrix <- confusionMatrix(
  factor(test_data$predicted_win),
  factor(test_data$home_team_win)
)
print(confusion_matrix)

########## ensemble model code vvvvv
# logistic reg predictions
test_data$predicted_prob_logistic_home <- predict(logistic_model_home_cfb, newdata = test_data, type = "response")

# Naive Bayes Predictions
test_data$predicted_prob_nb_home <- predict(naive_bayes_model_home_cfb, newdata = test_data, type = "raw")[, 2]

# SVM Predictions
test_data$predicted_prob_svm_home <- predict(svm_model_home_cfb, newdata = test_data, probability = TRUE)
test_data$predicted_prob_svm_home <- attr(test_data$predicted_prob_svm_home, "probabilities")[, 2]

# other features
test_data$mean_prob <- (test_data$predicted_prob_logistic_home + test_data$predicted_prob_nb_home + test_data$predicted_prob_svm_home) / 3
test_data$log_nb_prob_int <- test_data$predicted_prob_logistic_home * test_data$predicted_prob_nb_home
test_data$svm_rf_prob_int <- test_data$predicted_prob_svm_home * test_data$predicted_prob_rf_home

#### XGBoost Ensemble Model

ensemble_data <- data.frame(
  logistic = test_data$predicted_prob_logistic_home,
  nb = test_data$predicted_prob_nb_home,
  svm = test_data$predicted_prob_svm_home,
  mean_prob = test_data$mean_prob,
  actual = as.numeric(test_data$home_team_win)
)

# Step 2: Convert the data to a DMatrix (the required data structure for XGBoost)
X <- as.matrix(ensemble_data[, c("logistic", "nb", "svm", "mean_prob")])
y <- ensemble_data$actual

dtrain <- xgb.DMatrix(data = X, label = y)

# params
xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 6,
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
  nrounds = 1000,
  nfold = n_folds,
  verbose = 1,
  showsd = TRUE,
  stratified = TRUE,
  early_stopping_rounds = 750,
  maximize = FALSE,
)
best_nrounds <- cv_results$best_iteration

# train final XGBoost model
xgb_model_cfb <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = list(train = dtrain),
  verbose = 0
)

# Make predictions on the test data using the trained XGBoost model
ensemble_data$final_prob_xgb_cfb <- predict(xgb_model_cfb, newdata = X)

# Convert predicted probabilities to binary predictions (win/loss)
ensemble_data$final_prediction_xgb_cfb <- ifelse(ensemble_data$final_prob_xgb_cfb > 0.5, 1, 0)

# Evaluate the model using a confusion matrix
confusion_matrix_xgb <- confusionMatrix(
  factor(ensemble_data$final_prediction_xgb_cfb),
  factor(ensemble_data$actual)
)
print(confusion_matrix_xgb)

# log loss for the last boosting round
final_logloss <- cv_results$evaluation_log$test_logloss_mean[nrow(cv_results$evaluation_log)]
print(final_logloss)

# using the glm function to fit a logistic regression model for Platt scaling
platt_model_cfb <- glm(
  actual ~ final_prob_xgb_cfb,
  data = ensemble_data,
  family = binomial(link = "logit")
)

ensemble_data$calibrated_prob_platt <- predict(platt_model_cfb, newdata = ensemble_data, type = "response")

log_loss <- function(actual, predicted) {
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
  predicted_probs = ensemble_data$final_prob_xgb_cfb,
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

# ensuring XGBoost predictions are stored in ensemble_data
ensemble_data$final_prediction_xgb_cfb <- ifelse(ensemble_data$final_prob_xgb_cfb > 0.5, 1, 0)

# adding week data to ensemble_data & calculating accuracy by week using ensemble XGBoost model
ensemble_data$week <- test_data$week
accuracy_by_week_xgb <- ensemble_data %>%
  group_by(week) %>%
  summarize(
    correct_predictions = sum(final_prediction_xgb_cfb == actual),  # Use XGBoost predictions
    total_predictions = n(),
    accuracy = correct_predictions / total_predictions
  )
print(accuracy_by_week_xgb, n=22)

# XGBoost weekly accuracy plot
ggplot(accuracy_by_week_xgb, aes(x = week, y = accuracy)) +
  geom_line() +
  geom_point() +
  labs(title = "XGBoost Model Accuracy by Week", x = "Week", y = "Accuracy") +
  theme_minimal()

####### points predictions

# home_score using linear regression
home_score_model_cfb <- lm(home_score ~ home_team_grade + away_team_grade + home_offense + away_offense + avg_home_grade + avg_away_grade, 
                       data = points_pred_cfb)
summary(home_score_model_cfb)

# away_score
away_score_model_cfb <- lm(away_score ~ home_team_grade + away_team_grade + home_offense + away_offense + avg_home_grade + avg_away_grade, 
                       data = points_pred_cfb)
summary(away_score_model_cfb)

# Predict home_score on the test dataset
test_data$predicted_home_score <- predict(home_score_model_cfb, newdata = test_data)
test_data$predicted_away_score <- predict(away_score_model_cfb, newdata = test_data)

# Calculate MAE for home_score
mae_home_score <- mean(abs(test_data$predicted_home_score - test_data$home_score))
mae_away_score <- mean(abs(test_data$predicted_away_score - test_data$away_score))

# Calculate RMSE for home_score
rmse_home_score <- sqrt(mean((test_data$predicted_home_score - test_data$home_score)^2))
rmse_away_score <- sqrt(mean((test_data$predicted_away_score - test_data$away_score)^2))

# evaluation metrics
print(paste("MAE for home score: ", mae_home_score))
print(paste("MAE for away score: ", mae_away_score))
print(paste("RMSE for home score: ", rmse_home_score))
print(paste("RMSE for away score: ", rmse_away_score))






