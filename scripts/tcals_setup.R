#TCALS setup

library(catR)
library(caret)

data(tcals)

# rename columns for clarity
item_bank <- data.frame(
  a = tcals[,1], # discrimination (slope of the ICC)
  b = tcals[,2], # difficulty (location on ability scale)
  c = tcals[,3], # pseudo-guessing (lower asymptote)
  d = tcals[,4], # "inattention" (upper asymptote, usually around 1)
  subgroup = as.factor(tcals[,5]) # subgroup (for content balancing)
)

# one-hot encode subgroups for ML use
dummy <- dummyVars("~ subgroup", data = item_bank)
subgroup_encoded <- data.frame(predict(dummy, newdata = item_bank))

# item matrix for ML use
items_ml <- cbind(item_bank[,1:4], subgroup_encoded)

save(item_bank, items_ml, file = "item_bank.RData")