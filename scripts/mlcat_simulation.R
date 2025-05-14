#new

library(catR)
library(caret)

load("item_bank.RData")
load("student_responses.RData")
load("ml_model.RData")

n_students <- 100
results <- data.frame(true_theta = numeric(n_students), est_theta = numeric(n_students))

# one-hot encoding setup (ensure all dummies exist)
dummy <- dummyVars("~ subgroup", data = item_bank)
subgroup_levels <- colnames(predict(dummy, newdata = item_bank))

for (s in 1:n_students) {
  theta <- abilities[s]
  answered <- c()
  history <- data.frame()
  current_est <- 0
  
  for (i in 1:20) {
    remaining_items <- setdiff(1:nrow(item_bank), answered)
    
    candidates <- lapply(remaining_items, function(j) {
      item <- item_bank[j, 1:4]
      group <- item_bank$subgroup[j]
      
      x <- data.frame(
        a = item$a,
        b = item$b,
        c = item$c,
        d = item$d,
        alignment = current_est - item$b
      )
      
      # subgroup dummies
      for (col in subgroup_levels) x[[col]] <- 0
      
      group_col <- paste0("subgroup.", group)
      if (group_col %in% subgroup_levels) x[[group_col]] <- 1
      
      # predict probability of correct response
      x$student <- 0
      x$item <- 0     # dummy value for now, retrain after removing later
      prob <- predict(log_model, newdata = x, type = "response")
      return(c(item = j, prob = prob))
    })
    
    candidates_df <- as.data.frame(do.call(rbind, candidates))
    candidates_df$distance <- abs(candidates_df$prob - 0.5)
    chosen_item <- candidates_df[which.min(candidates_df$distance), "item"]
    
    answered <- c(answered, chosen_item)
    
    # simulate response w 3PL
    item_params <- as.numeric(item_bank[chosen_item, 1:4])
    p <- item_params[3] + (1 - item_params[3]) / (1 + exp(-1.7 * item_params[1] * (theta - item_params[2])))
    response <- rbinom(1, 1, p)
    
    # update history and ability estimate
    history <- rbind(history, data.frame(a = item_params[1], b = item_params[2], c = item_params[3], response = response))
    current_est <- thetaEst(it = as.matrix(history[,1:3]), x = history$response, model = "3PL", method = "EAP")
  }
  
  results$true_theta[s] <- theta
  results$est_theta[s] <- current_est
}

# get RMSE
rmse_mlcat <- sqrt(mean((results$true_theta - results$est_theta)^2))
cat("MLCAT RMSE:", round(rmse_mlcat, 4), "\n")

save(results, file = "mlcat_results.RData")
