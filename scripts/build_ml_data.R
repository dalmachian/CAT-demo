# long format ML dataset

load("student_responses.RData")
load("item_bank.RData")

student_ids <- rep(1:nrow(response_matrix), each = ncol(response_matrix))
item_ids <- rep(1:ncol(response_matrix), times = nrow(response_matrix))
responses <- as.vector(t(response_matrix))
theta <- rep(abilities, each = ncol(response_matrix))

# item features
item_feats <- item_bank[item_ids, 1:4]
groups <- item_bank$subgroup[item_ids]

# recreate subgroup dummies
dummy <- dummyVars("~ subgroup", data = item_bank)
group_dummies <- data.frame(predict(dummy, newdata = data.frame(subgroup = groups)))

# combine features
df <- data.frame(
  student = student_ids,
  item = item_ids,
  response = responses,
  theta = theta,
  item_feats,
  group_dummies,
  alignment = theta - item_feats$b
)

save(df, file = "ml_dataset.RData")
