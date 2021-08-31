library(rhdf5)
library(tidyverse)

# read the data
mnist <- "data/mnist-784-euclidean.hdf5"
file.size(mnist)
# h5ls(mnist)
# h5ls(mnist, all = TRUE)
# h5ls(mnist, recursive = 2)
# # h5dump is similar to h5ls
# h5dump(mnist, load = FALSE)

M <- h5dump(mnist)
str(M)
# List of 4
# $ distances: num [1:100, 1:10000] 677 794 863 865 895 ...
# $ neighbors: int [1:100, 1:10000] 53843 38620 16186 27059 47003 14563 44566 15260 40368 36395 ...
# $ test     : num [1:784, 1:10000] 0 0 0 0 0 0 0 0 0 0 ...
# $ train    : num [1:784, 1:60000] 0 0 0 0 0 0 0 0 0 0 ...
class(M)
H5close()

# subset of train
N <- 60000
# train <- t(M$train[,1:N]) 
# # train[1:10, 1:10]
# dim(train)

saveRDS(t(M$train), file = paste0("data/mnist_train", N/1000, "k.rds"))
saveRDS(t(M$test), file = paste0("data/mnist_test", 10000/1000, "k.rds"))
saveRDS(t(M$neighbors), file = paste0("data/mnist_neighbors", 10000/1000, "k.rds"))
