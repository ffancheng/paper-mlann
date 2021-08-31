rm(list = ls())
library(tidyverse)
library(dimRed)
library(pryr)
Jmisc::sourceAll("R/sources")
set.seed(1)
N <- 10000
# scen <- 1 # If running within R uncomment this. This will only run first scenario
scen <- as.numeric(commandArgs()[[6]]) # If running batch job uncomment this, array=1-804

message("Started at: ", Sys.time())
train <- readRDS("data/mnist_test10k.rds")
train <- train[1:N, ]
mem_used()

# for each method, change 2 lines with #
D <- 2
K <- 20
perplexity <- 7 # (K/3)
theta <- 0.5
min_dist <- 0.1
n_epochs <- 200

# True NN
if(N < 10000) {
  mnist_nn <- RANN::nn2(train, query = train, k = K+1, treetype = "kd", searchtype = "standard", eps = 0)$nn.idx[, -1]
} else {
  mnist_nn <- readRDS("data/mnist_test10k_truenn_K20.rds")
}

ml <- c("annIsomap", "annLaplacianEigenmaps", "annHLLE", "annLLE", "anntSNE", "annUMAP")
# annmethod <- c("kdtree", "annoy", "hnsw")
# params <- c("epsilon", "ntrees", "nlinks")
# (simtable <- tidyr::expand_grid(ml, data.frame(annmethod, params) ))

epsilon <- seq(0, 5, 0.1) # kdtree
ntrees <- seq(2, 100, 2) #2:100, # Annoy
nlinks <- seq(2, 200, 2) #2:200 # HNSW
simtable <- expand_grid(ml = ml, 
                        rbind(
                          expand_grid(annmethod = "kdtree", param = "epsilon", value = epsilon),
                          expand_grid(annmethod = "annoy", param = "ntrees", value = ntrees),
                          expand_grid(annmethod = "hnsw", param = "nlinks", value = nlinks) )) 
simtable
# 51+50+100 = 201

(simj <- simtable[scen,]) # Extract row of table
method <- simj$ml 
annmethod <- simj$annmethod %>% as.character() # In the cluster, these two columns are factors, need to convert
param_name <- simj$param %>% as.character()
par <- simj$value

(ann_table <- tidyr::expand_grid(par = par, time = NA, qps = NA, 
                                 knn_time = 0, dijkstra_time = 0, eigen_time = 0, recall = 0))
n_table <- ncol(ann_table)
mem_used()

message(Sys.time(), "\t", param_name, "=", par)
if(N < 10000) ntimes <- 1 else ntimes <- 10
suppressMessages(
  compute <- microbenchmark::microbenchmark(
    {e <- replicate(n = ntimes,
                    dimRed::embed(train, .method = method, knn = K, annmethod = annmethod,
                                  perplexity = perplexity, theta = theta, # tSNE
                                  min_dist = min_dist, n_epochs = n_epochs, input = "data", # UMAP
                                  eps = par,
                                  nt = par,
                                  nlinks = par, ef.construction = 500,
                                  ndim = D, .mute = c("output")))
    },
    times = ntimes, 
    unit = "s"
  )
)
ann_table$time <- sum(compute$time*1e-9) / ntimes # summary(compute)$median
ann_table$qps <- nrow(train) / ann_table$time 

steps.time <- matrix(NA, ntimes, 3)
for (j in 1:ntimes) {
  steps.time[j, ] <- e[[j]]@running.time
}
ann_table[, 4:6] <- t(apply(steps.time, 2, median))
# steps_time <- e@running.time
# ann_table[i, 4:6] <- t(steps_time)

e1 <- e[[1]]
# append 13 quality measures including optimal K
# embedding are the same, so the qualities don't change. Only running time varies. 
ann_table[, (n_table+1):(n_table+8)] <- dr_quality(X=train, Y=e1@data@data, K, nn.idx = mnist_nn)$quality 

# calculate recall row wise
mat_recall <- NA
for(j in 1:N){
  mat_recall[j] <- cal_recall(x = e1@nn.idx[,-1][j,], y = mnist_nn[j,])
}
ann_table$recall <- mean(mat_recall)

ann_table
ann_table[,7:15]
saveRDS(ann_table, file = paste0("annmnist/", "anntable_", method, "_", annmethod, "_", par, "_MNISTtest_", N,".rds"))

# save the list containing all embedding results, each replicated ntimes
saveRDS(e, file = paste0("annmnist/embedding/", "embedding_", method, "_", annmethod, "_", par, "_MNISTtest_", N,".rds"))

message("Finished at: ", Sys.time())
mem_used()
