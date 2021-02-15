rm(list=ls())
library(data.table)
library(dtplyr)
library(tidyverse)
library(ggplot2)
library(dimRed)
library(pryr)
Jmisc::sourceAll("R/sources")
set.seed(1)
nid <- 10
ntow <- 336
scen <- 4
# scen <- as.numeric(commandArgs()[[6]]) 

# load(paste0('data/spdemand_', nid, 'id', ntow, 'tow.rda'))
load("data/spdemand_3639id336tow.rda")
train <- spdemand %>% 
  lazy_dt() %>% 
  filter(id <= sort(unique(spdemand[,id]))[nid],
         tow <= ntow) %>% 
  dplyr::select(-id, -tow) %>% 
  as.data.table() 
mem_used()
mem_change(rm(spdemand))
N <- nrow(train)

# Parameters fixed
D <- 5
K <- 20

# True NN
mnist_nn <- RANN::nn2(train, query = train, k = K+1, treetype = "kd", searchtype = "standard", eps = 0)$nn.idx

param_value <- list(
  epsilon = seq(0, 10, 0.2), # kdtree
  ntrees = seq(2,100,2), #2:100, # Annoy
  nlinks = seq(2,200,4) #2:200 # HNSW
)

methods <- c("annIsomap", "annLaplacianEigenmaps", "annHLLE")#[-3]
annmethods <- c("kdtree", "annoy", "hnsw")
params <- c("epsilon", "ntrees", "nlinks")
(simtable <- tidyr::expand_grid(methods, data.frame(annmethods, params)))

(simj <- simtable[scen,]) # Extract row of table
method <- simj$methods 
annmethod <- simj$annmethods %>% as.character() # In the cluster, these two columns are factors, need to convert
para <- simj$params %>% as.character()

switch(annmethod,
       "kdtree" = {assign(para, param_value$epsilon)},
       "annoy" = {assign(para, param_value$ntrees)},
       "hnsw" = {assign(para, param_value$nlinks)}
)

(ann_table <- tidyr::expand_grid(para = get(para), time = NA, qps = NA, 
                                 knn_time = 0, dijkstra_time = 0, eigen_time = 0, recall = 0))
n_table <- ncol(ann_table)
mem_used()
# i=10
for(i in sample(1:nrow(ann_table), nrow(ann_table))) {
  annparam <- ann_table[i,]
  par <- annparam$para 
  message(Sys.time(), " par=", par)
  suppressMessages(
    compute <- microbenchmark::microbenchmark(
      e <- dimRed::embed(train, .method = method, knn = K, annmethod = annmethod,
                         eps = par,
                         nt = par,
                         nlinks = par, ef.construction = 500,
                         ndim = D, .mute = c("output")),
      times = 5, 
      unit = "s"
    )
  )
  ann_table$time[i] <- summary(compute)$median # compute$time*1e-9
  steps_time <- e@running.time
  ann_table$knn_time[i] <- steps_time[1]
  ann_table$dijkstra_time[i] <- steps_time[2]
  ann_table$eigen_time[i] <- steps_time[3]
  ann_table$qps[i] <- nrow(train) / ann_table$time[i] 
  # calculate recall row wise, is.element()?
  mat_recall <- NA
  for(j in 1:N){
    mat_recall[j] <- cal_recall(x = e@nn.idx[,-1][j,], y = mnist_nn[,-1][j,])
  }
  ann_table$recall[i] <- mean(mat_recall)
  # append 13 quality measures including optimal K
  ann_table[i, (n_table+1):(n_table+13)] <- dr_quality(e)$quality 
}
names(ann_table)[1] <- para

ann_table
mem_used()
save(ann_table, file = paste0("electricity/", "anntable_", annmethod, "_", method, "_electricity_", nid, "id", ntow, "tow.RData"))


# load(paste0("ann/", "anntable_", annmethod, "_", method, "_mnist_test_", nrow(train)/1000 ,"k.RData"))

# p <- ggplot(ann_table, aes(x = time, y = recall, color = get(para))) +
#   geom_line(size = 0.5) +
#   geom_point(size=1) + 
#   labs(y = "Recall", x = "Computation time (second)", color = para) 
# p
# ggsave(paste0("electricity/", "anntable_", annmethod, "_", method, "_electricity_", nid, "id", ntow, "tow_recall.pdf"), p, width = 10, height = 6)
# 
# 
# p1 <- ggplot(ann_table, aes(x = time, y = M_T, color = get(para))) +
#   geom_line(size = 0.5) +
#   geom_point(size=1) + 
#   labs(y = "M_T", x = "Computation time (second)", color = para) 
# p1
# ggsave(paste0("electricity/", "anntable_", annmethod, "_", method, "_electricity_", nid, "id", ntow, "tow_MT.pdf"), p1, width = 10, height = 6)
# 
# 
# 
# kdtime <- ann_table[, c(1:2, 4:6, 9:15, 7)]
# kd_df <- gather(kdtime, key = Step, value = Time, -c(epsilon))
# kd_df$Step <- factor(kd_df$Step, levels = c(names(kdtime)[-c(1, 13)], "recall"))
# p2 <- kd_df %>% ggplot(aes(x=epsilon, y=Time)) +
#   geom_line() +
#   facet_wrap(~Step, ncol = 4, scales = "free") +
#   labs(y = "Quality measures                                 Computation time")
# p2
# ggsave(paste0("electricity/", "anntable_", annmethod, "_", method, "_electricity_", nid, "id", ntow, "tow_time+qualities.pdf"), p2, width = 10, height = 6)


message("Finished at: ", Sys.time())
mem_used()
