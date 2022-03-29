
# dummy row
x <- c("a","b","c","d","e")

# get indices
indices = 1:length(x)

# create combinations and combine
uniq_indices <- combn(indices,2)
uniq_indices_rev <- combn(rev(indices),2)
df <- data.frame(uniq_indices,uniq_indices_rev)