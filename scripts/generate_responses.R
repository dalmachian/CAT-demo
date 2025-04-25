# simulate ability level and responses

library(catR)
load("item_bank.RData")

set.seed(123)
n_students <- 500
abilities <- rnorm(n_students, mean = 0, sd = 1)

# extract the 4PL item parameters only
item_matrix <- as.matrix(item_bank[,1:4])

# generate 500x85 matrix for responses
response_matrix <- genPattern(th = abilities, it = item_matrix)

save(response_matrix, abilities, file = "student_responses.RData")