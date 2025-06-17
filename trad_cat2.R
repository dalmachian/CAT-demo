# trad_cat2.R
# traditional CAT simulation with MFI, genPattern, and subgroups for content balancing
library(catR)

data(tcals)
item_bank <- as.matrix(tcals[, 1:4])  # a, b, c, d columns (4PL)

set.seed(123)
n_students <- 100
abilities <- rnorm(n_students)  # true thetas
# generate response patterns
responses <- genPattern(th = abilities, it = item_bank)

# subgroup mapping (for content balancing)
subgroup_map <- list(
  "Audio1" = 1:12,
  "Audio2" = 13:33,
  "Written1" = 34:46,
  "Written2" = 47:63,
  "Written3" = 64:85
)
items_per_group <- 4  # 5 groups × 4 items = 20 items total

# set up result
results <- data.frame(
  student = 1:n_students,
  true_theta = abilities,
  est_theta = NA,
  mse = NA,
  sem = NA
)
results$questions <- vector("list", n_students)
results$theta_trace <- vector("list", n_students)
results$info_trace <- vector("list", n_students)

# simulation loop
for (s in 1:n_students) {
  true_theta <- abilities[s]
  current_est <- 0 # initial estimate set to null
  answered <- c()
  responses_s <- c()
  item_ids <- c()
  theta_history <- c()
  info_history <- c()
  
  for (group in names(subgroup_map)) {
    group_items <- subgroup_map[[group]]
    for (i in 1:items_per_group) {
      available <- setdiff(group_items, answered)
      
      if (length(available) == 0) {
        warning(paste("Student", s, "- no available items left in group", group))
        next
      }
      
      # "defensive try-catch" for item selection
      selected <- tryCatch({
        next_result <- nextItem(
          itemBank = item_bank,
          theta = current_est,
          out = answered,
          criterion = "MFI", # maximum fisher information
          nAvailable = available
        )
        next_result$item  # extract selected item
      }, error = function(e) {
        warning(paste("nextItem failed for student", s, ":", e$message))
        return(NA)  # return NA so we can skip it
      })
      
      if (is.na(selected)) break  # skip to next item if selection failed
      
      answered <- c(answered, selected)
      item_ids <- c(item_ids, selected)
      
      # use response generated before (with genPattern)
      r <- responses[s, selected]
      responses_s <- c(responses_s, r)
      
      # update ability and record info
      current_est <- eapEst(it = item_bank[answered, ], x = responses_s)
      info_val <- Ii(th = current_est, it = item_bank[selected, , drop = FALSE])$Ii
      
      theta_history <- c(theta_history, current_est)
      info_history <- c(info_history, info_val)
    }
  }
  
  # final SEM
  sem_final <- eapSem(thEst = current_est, it = item_bank[answered, ], x = responses_s)
  
  # save student results
  results$est_theta[s] <- current_est
  results$mse[s] <- (current_est - true_theta)^2
  results$sem[s] <- sem_final
  results$questions[[s]] <- item_ids
  results$theta_trace[[s]] <- theta_history
  results$info_trace[[s]] <- info_history
}

saveRDS(results, "results_traditional_cat.rds")
write.csv(
  results[, c("student", "true_theta", "est_theta", "mse", "sem")],
  "results_traditional_cat.csv",
  row.names = FALSE
)

cat("Mean MSE:", mean(results$mse), "\n")
