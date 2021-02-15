# This function is used to calculate the quality measures and recall rate 
# Input: embedding result from dimRed::embed(), nn.idx is the true nearest neighbor index from k-d trees
# Output: ann_table of 9 columns for different quality measures
# Example: calc_ann_table(e, nn.idx=truenn)
calc_ann_table <- function(e, nn.idx){
  N <- nrow(e@org.data)
  ann_table <- dr_quality(X=e@org.data, Y=e@data@data, K=e@pars$knn, nn.idx = nn.idx)$quality
  # calculate recall row wise
  mat_recall <- NA
  for(j in 1:N){
    mat_recall[j] <- cal_recall(x = e@nn.idx[,-1][j,], y = nn.idx[j,])
  }
  ann_table$recall <- mean(mat_recall)
  return(ann_table)
}

# # test
# library(dimRed)
# Jmisc::sourceAll(here::here("R/sources")) 
# 
# sr <- loadDataSet("Swiss Roll")
# sr_isomap <- embed(sr, .method = "annIsomap")
# X <- sr_isomap@org.data
# # Y <- sr_isomap@data@data
# pars <- sr_isomap@pars
# nn.idx <- RANN::nn2(data = X, query = X, k = pars$knn + 1, eps = 0,
#                    treetype = "kd", searchtype = "standard")$nn.idx[, -1]
# calc_ann_table(e=sr_isomap, nn.idx = nn.idx)
