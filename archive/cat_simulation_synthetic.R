# basic CAT simulation with generated item bank

library(catR)

# generate 3PL item bank with required 4 columns
a_params <- runif(50, 0.5, 2)      # discrimination
b_params <- runif(50, -3, 3)       # difficulty
c_params <- runif(50, 0, 0.25)     # guessing
d_dummy  <- rep(0, 50)             # unused fourth column
item_bank <- cbind(a_params, b_params, c_params, d_dummy)

# simulated test taker with an ability value theta
true_theta <- 0.5

# create a manual response simulator using the 3PL model
simulateResponse <- function(theta, item) {
  a <- item[1]
  b <- item[2]
  c <- item[3]
  p <- c + (1 - c) / (1 + exp(-1.7 * a * (theta - b)))
  rbinom(1, 1, p)
}

# select 20 items adaptively
selected_items <- c()
responses <- c()
theta_est <- 0

for (i in 1:20) {
  next_item <- nextItem(itemBank = item_bank, theta = theta_est, out = selected_items)
  item_num <- next_item$item
  item <- item_bank[item_num, ]
  response <- simulateResponse(true_theta, item)
  selected_items <- c(selected_items, item_num)
  responses <- c(responses, response)
  theta_est <- thetaEst(it = item_bank[selected_items, ], x = responses, method = "EAP")
  cat("Item:", i, "- selected:", item_num, "- response:", response, "- est theta:", round(theta_est, 2), "\n")
}

#final output
cat("\nFinal estimated ability:", round(theta_est, 3), "\n")
cat("Items administered:", selected_items, "\n")
cat("Response pattern:", responses, "\n")
