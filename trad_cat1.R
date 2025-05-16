library(catR)
library(caret)

# load item bank
data(tcals)
item_bank <- as.matrix(tcals[, 1:4])  # a, b, c, d columns (4PL)

# simulate abilities (theta) for 100 students
set.seed(123)
n_students <- 100
abilities <- rnorm(n_students)

# generate response patterns
responses <- genPattern(th = abilities, it = item_bank)

# set up result
results <- data.frame(true_theta = numeric(n_students), est_theta = numeric(n_students), sem = numeric(n_students))


for (s in 1:n_students) {
  answered <- c()
  response_pattern <- c()
  est_theta <- 0  # initial estimate set to null
  
  for (i in 1:20) {
    remaining_items <- setdiff(1:nrow(item_bank), answered)
    
    # item selection using MFI
    next_item_info <- nextItem(itemBank = item_bank, theta = est_theta, out = answered, criterion = "MFI")
    selected <- next_item_info$item
    response <- responses[s, selected]
    
    answered <- c(answered, selected)
    response_pattern <- c(response_pattern, response)
    
    # estimate ability
    est_theta <- eapEst(item_bank[answered, ], response_pattern)
    
    # end early if standard error measure is low (maybe no need for this)
    sem <- eapSem(est_theta, item_bank[answered, ], response_pattern)
    if (sem < 0.3) break
  }
  
  results$true_theta[s] <- abilities[s]
  results$est_theta[s] <- est_theta
  results$sem[s] <- sem
}

head(results)
summary(results$est_theta - results$true_theta)
