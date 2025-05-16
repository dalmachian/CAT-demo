library(catR)
library(caret)

data(tcals)
item_bank <- as.matrix(tcals[, 1:4])

set.seed(123)
n_students <- 3  # SMALL number for debugging
abilities <- rnorm(n_students)
responses <- genPattern(th = abilities, it = item_bank)

# Create mock training data
features <- data.frame()
for (i in 1:5) {  # only first 5 items to keep fast
  for (t in abilities) {
    p <- 1 / (1 + exp(-item_bank[i,1] * (t - item_bank[i,2])))
    features <- rbind(features, data.frame(
      ability = t,
      a = item_bank[i,1],
      b = item_bank[i,2],
      c = item_bank[i,3],
      d = item_bank[i,4],
      alignment = abs(t - item_bank[i,2]),
      response = rbinom(1, 1, p)
    ))
  }
}
ml_model <- train(as.factor(response) ~ ., data = features, method = "rf")

resultsRF <- data.frame(true_theta = numeric(n_students), est_theta = numeric(n_students), sem = numeric(n_students))

# Run loop
for (s in 1:n_students) {
  theta <- abilities[s]
  answered <- c()
  response_pattern <- c()
  est_theta <- 0
  
  for (i in 1:5) {
    remaining <- setdiff(1:nrow(item_bank), answered)
    pred_probs <- numeric(length(remaining))
    
    for (j in seq_along(remaining)) {
      item_id <- remaining[j]
      features_test <- data.frame(
        ability = est_theta,
        a = item_bank[item_id,1],
        b = item_bank[item_id,2],
        c = item_bank[item_id,3],
        d = item_bank[item_id,4],
        alignment = abs(est_theta - item_bank[item_id,2])
      )
      pred_probs[j] <- predict(ml_model, features_test, type = "prob")[,2]
    }
    
    selected <- remaining[which.min(abs(pred_probs - 0.5))]
    response <- responses[s, selected]
    answered <- c(answered, selected)
    response_pattern <- c(response_pattern, response)
    
    est_theta <- eapEst(item_bank[answered, ], response_pattern)
    sem <- eapSem(est_theta, item_bank[answered, ], response_pattern)
    
    if (sem < 0.3) break
  }
  
  resultsRF$true_theta[s] <- theta
  resultsRF$est_theta[s] <- est_theta
  resultsRF$sem[s] <- sem
}

print("Done!")
print(resultsRF)
