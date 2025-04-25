# comparison of ability estimate and RMSE for cat vs mlcat

library(catR)
library (mirt)
library(mirtCAT)
library(rpart)
library(ggplot2)

#setwd()

set.seed(123)
n_items <- 100
a <- runif(n_items, 0.8, 2.5)         # discrimination
b <- runif(n_items, -3, 3)           # difficulty
c <- runif(n_items, 0, 0.25)         # guessing
#d <- rep(1, n_items)                # upper asymptote
#read what randomcat accepts
item_bank <- cbind(a, b, c)


# simulate examinee data
N <- 1000
theta_true <- rnorm(N)  # true abilities

# preallocate result vectors
theta_cat <- numeric(N)
n_items_used <- numeric(N)

# trad CAT, with known and fixed "theta_true"
for (i in 1:N) {
  res <- randomCAT(itemBank = item_bank, 
                   model = NULL, # doesnt accept 3PL for some reason?????
                   start = list(nrItems = 1),
                   stop = list(rule = "precision", thr = 0.3),
                   trueTheta = theta_true[i])
  
  if (!is.null(res$theta) && length(res$theta) == 1) {
    theta_cat[i] <- res$theta
    n_items_used[i] <- length(res$administeredItem)
  } else {
    theta_cat[i] <- NA  # mark as missing
    n_items_used[i] <- NA
  }
  
}

rmse_cat <- sqrt(mean((theta_cat - theta_true)^2, na.rm = TRUE))

# MLCAT, fit decision tree to improve theta prediction
features <- data.frame(
  theta_est = theta_cat,
  total_items = n_items_used,
  theta_true = theta_true
)

# fit decision tree model
tree_model <- rpart(theta_true ~ theta_est + total_items, data = features)
features$theta_ml <- predict(tree_model, newdata = features)
rmse_mlcat <- sqrt(mean((features$theta_ml - features$theta_true)^2))

# visualisation
df <- data.frame(
  Method = rep(c("CAT", "MLCAT"), each = N),
  Estimate = c(theta_cat, features$theta_ml),
  True = rep(theta_true, 2)
)

# scatterplot
comparison_plot <- ggplot(df, aes(x = True, y = Estimate, color = Method)) +
  geom_point(alpha = 0.3) +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "θ Estimates: CAT vs ML-enhanced CAT",
    x = "True θ", y = "Estimated θ"
  )

ggsave("theta_estimates_comparison.png", plot = comparison_plot, width = 8, height = 6)
print(comparison_plot)


rmse_cat_text <- paste("RMSE - Traditional CAT:", round(rmse_cat, 4))
rmse_mlcat_text <- paste("RMSE - ML-enhanced CAT:", round(rmse_mlcat, 4))

message("MSE - Traditional CAT: ", round(rmse_cat, 4))
message("MSE - ML-enhanced CAT: ", round(rmse_mlcat, 4))