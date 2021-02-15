cal_recall <- function(x, y){
  # x, y as matrix/vector
  # x is the ANN, y is the true NN (baseline)
  x <- as.vector(x)
  y <- as.vector(y)
  tmp <- x %in% y
  recall <- sum(tmp) / length(tmp) 
  return(recall)
}

# for(i in 1:N){
#   recall[i] <- cal_recall(x = e@nn.idx[,-1][i,], y = ann[,-1][i,])
# }
# recall
