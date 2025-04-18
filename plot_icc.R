#Item Characteristic Curves (ICCs) Plot

library(catR)
library (mirt)
library(mirtCAT)
library(rpart)
library(ggplot2)

#setwd("if needed")

#simulate a 3PL item bank for 100 items
set.seed(123)
n_items <- 100
a <- runif(n_items, 0.8, 2.5)         # discrimination
b <- runif(n_items, -3, 3)           # difficulty
c <- runif(n_items, 0, 0.25)         # guessing
d <- rep(1, n_items)                # upper asymptote (default 1 in 3PL)
item_bank <- cbind(a, b, c, d)

# plot first 5 item characteristic curves (manually)
theta_vals <- seq(-4, 4, length.out = 300)
icc_data <- data.frame()

for (i in 1:5) {
  a_i <- item_bank[i, 1]
  b_i <- item_bank[i, 2]
  c_i <- item_bank[i, 3]
  
  p_theta <- c_i + (1 - c_i) / (1 + exp(-1.7 * a_i * (theta_vals - b_i)))
  
  icc_data <- rbind(icc_data, data.frame(
    theta = theta_vals,
    probability = p_theta,
    item = paste0("Item ", i)
  ))
}

icc_plot <- ggplot(icc_data, aes(x = theta, y = probability, color = item)) +
  geom_line(size = 1.2) +
  labs(title = "Item Characteristic Curves (ICCs)", x = expression(theta), y = "P(correct)") +
  theme_minimal()

print(icc_plot)

#save to file
ggsave("item_plot.png", plot = icc_plot, width = 8, height = 6)