load("ml_dataset.RData")

# Split data
set.seed(42)
train_idx <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[train_idx, ]
test_data <- df[-train_idx, ]

# Train logistic regression
log_model <- glm(response ~ . -student -item, data = train_data, family = "binomial")

# Predict on test set
test_probs <- predict(log_model, newdata = test_data, type = "response")
test_preds <- ifelse(test_probs > 0.5, 1, 0)

# Accuracy
acc <- mean(test_preds == test_data$response)
cat("Test accuracy:", round(acc * 100, 2), "%\n")

# Save model for use in MLCAT simulation
save(log_model, file = "ml_model.RData")