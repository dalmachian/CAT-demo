# CAT simulation using TCALS + EAP estimation

library(catR)
load("item_bank.RData")

true_theta <- 0.5
item_matrix <- as.matrix(item_bank[,1:4])

simulateResponse <- function(theta, item) {
  a <- item[1]
  b <- item[2]
  c <- item[3]
  p <- c + (1 - c) / (1 + exp(-1.7 * a * (theta - b)))
  rbinom(1, 1, p)
}

selected_items <- c()
responses <- c()
theta_est <- 0

for (i in 1:20) {
  next <- nextItem(itemBank = item_matrix, theta = theta_est, out = selected_items)
  item_num <- next$item
  response <- simulateResponse(true_theta, item_matrix[item_num, ])
  selected_items <- c(selected_items, item_num)
  responses <- c(responses, response)
  theta_est <- thetaEst(it = item_matrix[selected_items, ], x = responses, method = "EAP")
}

cat("final est θ:", round(theta_est, 3), "\n")
cat("items administered:", selected_items, "\n")
cat("responses:", responses, "\n")