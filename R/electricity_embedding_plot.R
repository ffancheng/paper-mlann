# This script contains the code for plotting for smart meter data
# To generate the loaded data `data/spdemand_3639id336tow.rda`, please run `source("data/electricity_02_quantiles_case1id1003.R")`
# To generate the loaded data `data/spdemand_3639id_336tow_201length.rda`, please run `source("data/electricity_02_quantiles_case3alladd.R")`
rm(list=ls())
library(data.table)
library(dtplyr)
library(tidyverse)
library(dimRed)
library(ggplot2)
library(patchwork)
library(hdrcde)
library(igraph)
library(MASS)
library(ggraph)
library(pryr)
library(colorspace)
library(caret)
library(tictoc)
Jmisc::sourceAll(here::here("R/sources")) 
set.seed(1)
eps <- 1
nt <- 50
annmethod <- "kdtree" # for ANN
ntimes <- 10

###------------------------------------------------------
# For electricity_02_quantiles_case1id1003.R for single household ID 1003, run the following section
nid <- 1
ntow <- 336
len <- 201
filename <- paste0(nid, "id_", ntow, "tow_", len, "length")
load(paste0('data/spdemand_3639id_336tow_201length.rda')) # (3639*336)*203
train <- spdemand %>%
  lazy_dt() %>%
  # filter(id <= sort(unique(spdemand[,id]))[nid],
  #        tow <= ntow) %>%
  filter(id == 1003, tow <= ntow) %>%
  dplyr::select(-c(id, tow)) %>%
  as.data.table()
# mem_change(rm(spdemand))
N <- nrow(train)
ids <- spdemand %>% dtplyr::lazy_dt() %>% pull(id) %>% unique() %>% sort()
###------------------------------------------------------


###------------------------------------------------------
# For electricity_02_quantiles_case2allnotow.R for all 3639 households, run this section
nid <- 3639
ntow <- 336
len <- 201
filename <- paste0(nid, "id_notow_", len, "length")
load(paste0('data/spdemand_', filename, '.rda')) # 3639*(336*201+1)
train <- spdemand[, !"id"]
N <- nrow(train)
ids <- spdemand %>% dtplyr::lazy_dt() %>% pull(id) %>% unique() %>% sort()
###------------------------------------------------------


###------------------------------------------------------
# For electricity_02_quantiles_case3alladd.R for all 3639 households, run this section
nid <- 3639
ntow <- 336
len <- 201
filename <- paste0(nid, "id_", ntow, "tow_", len, "length")
load(paste0('data/spdemand_', filename, '.rda')) # 3639*(336*201+1)
train <- spdemand[, !"id"]#[1:100,]
N <- nrow(train)
ids <- spdemand %>% dtplyr::lazy_dt() %>% pull(id) %>% unique() %>% sort()
###------------------------------------------------------


# ###-------------------------------------------------
# ### plot typical households (run once)
# ###-------------------------------------------------
# # Plot the electricity demand for two sample IDs, 1003 and 1539
# # load("data/DT.rda")
# # head(DT)
# # DT2id <- DT[id == 1003 | id == 1539,]
# # save(DT2id, file = "data/DT2id.eda")
# load("data/DT2id.rda")
# DT2id
# summary(DT2id)
# p <- DT2id %>%
#   ggplot(aes(x = day, y = demand, group = id)) +
#   geom_line() +
#   facet_grid(id ~ .) +
#   labs(x = "Days", y = "Demand (kWh)")
# ggsave("paper/figures/smartmeter.png", p, width = 8, height = 6)
# plotly::ggplotly(p, width = 600, height = 400)



###-------------------------------------------------
### KNN graph
###-------------------------------------------------
# Parameters fixed
# D <- 2
# K <- 20 
eps <- 1 # 5, 10
pars <- list(knn = 20,
            eps = 0, 
            ndim = 2,
            get_geod = FALSE,
            annmethod = "kdtree", 
            nt = 50, 
            search.k = 500, 
            nlinks = 16, 
            ef.construction = 200, 
            distance = c("euclidean", "manhattan")[1]) # "euclidean" for 1 ID, "manhattan" for all ids

knng0 <- makeKNNgraph(x = train, k = pars$knn, eps = pars$eps, 
                     annmethod = pars$annmethod, 
                     nt = pars$nt, nlinks = pars$nlinks, 
                     ef.construction = pars$ef.construction,
                     distance = pars$distance)#$g
knng <- knng0$g
igraph::is.connected(knng)
# plot(knng)

# k-d trees with eps=1
anng0 <- makeKNNgraph(x = train, k = pars$knn, eps = eps, 
                     annmethod = pars$annmethod, 
                     nt = pars$nt, nlinks = pars$nlinks, 
                     ef.construction = pars$ef.construction,
                     distance = pars$distance)#$g
anng <- anng0$g
igraph::is.connected(anng)
# plot(anng)
all.equal(knng, anng)

# Annoy with nt
nt <- 50
search.k <- nt
pars <- list(knn = 20,
            eps = 0, 
            ndim = 2,
            get_geod = FALSE,
            annmethod = "annoy", 
            nt = nt, 
            search.k = search.k,
            nlinks = 16, 
            ef.construction = 200, 
            distance = c("euclidean", "manhattan")[1])
anng_annoy0 <- makeKNNgraph(x = train, k = pars$knn, eps = pars$eps, 
                           annmethod = pars$annmethod, 
                           nt = pars$nt, search.k = pars$search.k, nlinks = pars$nlinks, 
                           ef.construction = pars$ef.construction,
                           distance = pars$distance)#$g
anng_annoy <- anng_annoy0$g
igraph::is.connected(anng_annoy)
# plot(anng_annoy)
all.equal(knng, anng_annoy)

# neighborhood graph NN vs ANN
# igraph
pknn <- ggraph(knng) +
  geom_edge_link(colour = 'grey60') +
  geom_node_point() +
  geom_node_text(aes(label = V(knng)), colour = blues9[7],
                 hjust = -.4, check_overlap = FALSE) 
# + 
#   theme(legend.position = "none")
# +
#   labs(title = "Exact NN graph")
pann <- ggraph(anng) +
  geom_edge_link(colour = 'grey60') +
  geom_node_point() +
  geom_node_text(aes(label = V(anng)), colour = blues9[7],
                 hjust = -.4, check_overlap = FALSE)
pannannoy <- ggraph(anng_annoy) +
  geom_edge_link(colour = 'grey60') +
  geom_node_point() +
  geom_node_text(aes(label = V(anng)), colour = blues9[7],
                 hjust = -.4, check_overlap = FALSE)

pnn <- (pknn + pann + pannannoy) &
  theme(plot.title=element_text(hjust=0.5))
# knng20_3639id336tow.png
ggsave(paste0("paper/figures/knng20_", filename, ".png"), pnn, width=18, height=6, dpi=500)

recall_kd <- NULL
for(i in 1:N){
  recall_kd[i] <- cal_recall(x = knng0$nn2res$nn.idx[,-1][i,], y = anng0$nn2res$nn.idx[,-1][i,])
}
recall_kd %>% mean() 
# 0.9769345 for Euclidean distance, ID1003
# 0.9965925 for Manhattan distance, eps=1
recall_annoy <- NULL
for(i in 1:N){
  recall_annoy[i] <- cal_recall(x = knng0$nn2res$nn.idx[,-1][i,], y = anng_annoy0$nn2res$nn.idx[,-1][i,])
}
recall_annoy %>% mean() 
# 0.9922619 for Euclidean distance, ID1003
# 0.973042 for Manhattan distance, nt=50

### True NN for calculating qualities
truenn <- RANN::nn2(train, query = train, k = pars$knn+1, treetype = "kd", searchtype = "standard", eps = 0)$nn.idx[, -1]

###-------------------------------------------------
### Isomap
###-------------------------------------------------
method0 <- "Isomap"
method <- paste0("ann", method0)

isomapNNtime <- microbenchmark::microbenchmark(
  {isomapnn <- dimRed::embed(train, .method = method, knn = pars$knn, 
                             annmethod = "kdtree",
                             eps = pars$eps,
                             nt = pars$nt,
                             nlinks = pars$nlinks, ef.construction = 500,
                             ndim = pars$ndim, .mute = c("output"))},
  times=ntimes, 
  unit = "s"
)

isomapANNtime <- microbenchmark::microbenchmark(
  {isomapann <- dimRed::embed(train, .method = method, knn = pars$knn, 
                              annmethod = "kdtree",
                              eps = eps,
                              nt = pars$nt,
                              nlinks = pars$nlinks, ef.construction = 500,
                              ndim = pars$ndim, .mute = c("output"))}, # , distance = pars$distance
  times=ntimes, 
  unit = "s"
)

isomapANNtime_annoy <- microbenchmark::microbenchmark(
  {isomapann_annoy <- dimRed::embed(train, .method = method, knn = pars$knn, 
                             annmethod = "annoy",
                             eps = pars$eps,
                             nt = pars$nt, search.k = pars$search.k,
                             nlinks = pars$nlinks, ef.construction = 500,
                             ndim = pars$ndim, .mute = c("output"))},
  times=ntimes, 
  unit = "s"
)

(isomapNNmed <- summary(isomapNNtime)$median) # 14.0103s
(isomapANNmed <- summary(isomapANNtime)$median) # 13.27944s
(isomapANNmed_annoy <- summary(isomapANNtime_annoy)$median) # 11.95033
(isomapNNmed - isomapANNmed)/isomapNNmed # 0.05216596
(isomapNNmed - isomapANNmed_annoy)/isomapNNmed # 0.1470329


Y_isomap <- isomapnn@data@data %>% as.data.frame()
Y_annisomap <- isomapann@data@data %>% as.data.frame()
Y_annisomap_annoy <- isomapann_annoy@data@data %>% as.data.frame()


# par(mfrow=c(1,2))
# plot(Y_isomap)
# plot(Y_annisomap)
all.equal(Y_isomap, Y_annisomap)
all.equal(Y_annisomap, Y_annisomap_annoy)

# Calculate embedding quality measures
ann_table_isomapnn <- calc_ann_table(e=isomapnn, nn.idx=truenn)
ann_table_isomapann <- calc_ann_table(e=isomapann, nn.idx=truenn)
ann_table_isomapann_annoy <- calc_ann_table(e=isomapann_annoy, nn.idx=truenn)

all.equal(ann_table_isomapnn, ann_table_isomapann_annoy)
ann_table_isomap <- bind_rows(ann_table_isomapnn, ann_table_isomapann, ann_table_isomapann_annoy) %>% 
  mutate(time = c(isomapNNmed, isomapANNmed, isomapANNmed_annoy))
row.names(ann_table_isomap) <- c("Exact NN", "k-d trees", "Annoy")
ann_table_isomap
#               M_T       M_C      LCMC       Qnx        W_n
# Exact NN  0.9845084 0.9480643 0.2743871 0.2798846 0.01380198
# k-d trees 0.9845654 0.9486318 0.2764893 0.2819868 0.01376920
# Annoy     0.9844317 0.9477084 0.2741260 0.2796235 0.01390426
#               W_nu    Procrustes       Rnx    recall     time
# Exact NN  0.001657145  0.5584798 0.2759038 1.0000000 14.01030
# k-d trees 0.001660732  0.5569013 0.2780177 0.8920445 13.27944
# Annoy     0.001669152  0.5593708 0.2756413 0.9939819 11.95033
save(isomapnn, isomapann, isomapann_annoy, ann_table_isomap, pars, eps, nt, isomapNNtime, isomapANNtime, isomapANNtime_annoy, 
     # Y_isomap, Y_annisomap, Y_annisomap_annoy, 
     file = paste0('data/electricityplot_', method0, "_kdtree_eps", eps, "_", filename, '.rda') # for k-d trees
     # file = paste0('data/electricityplot/electricityplot_', method0, "_", annmethod, "_nt", nt, "_", filename, '.rda') # for Annoy
     )


###-------------------------------------------------
### LLE
###-------------------------------------------------
method0 <- "LLE"
method <- paste0("ann", method0)

lleNNtime <- microbenchmark::microbenchmark(
  {llenn <- embed(train, .method = method, knn = pars$knn,
               annmethod = "kdtree",
               eps = pars$eps,
               nt = pars$nt,
               nlinks = pars$nlinks, ef.construction = 500,
               ndim = pars$ndim, distance = pars$distance, .mute = c("output"))},
  times=ntimes, 
  unit = "s"
)

lleANNtime <- microbenchmark::microbenchmark(
  {lleann <- embed(train, .method = method, knn = pars$knn,
                annmethod = "kdtree",
                eps = eps,
                nt = pars$nt,
                nlinks = pars$nlinks, ef.construction = 500,
                ndim = pars$ndim, distance = pars$distance, .mute = c("output"))},
  times=ntimes, 
  unit = "s"
)

lleANNtime_annoy <- microbenchmark::microbenchmark(
  {lleann_annoy <- embed(train, .method = method, knn = pars$knn,
                annmethod = "annoy",
                eps = eps,
                nt = pars$nt, search.k = pars$search.k,
                nlinks = pars$nlinks, ef.construction = 500,
                ndim = pars$ndim, distance = pars$distance, .mute = c("output"))},
  times=ntimes, 
  unit = "s"
)
Y_lle <- llenn@data@data %>% as.data.frame()
Y_annlle <- lleann@data@data %>% as.data.frame()
Y_annlle_annoy <- lleann_annoy@data@data %>% as.data.frame()

(lleNNmed <- summary(lleNNtime)$median)
(lleANNmed <- summary(lleANNtime)$median)
(lleANNmed_annoy <- summary(lleANNtime_annoy)$median)
(lleNNmed - lleANNmed)/lleNNmed
(lleNNmed - lleANNmed_annoy)/lleNNmed
# par(mfrow=c(1,2))
# plot(Y_lle)
# plot(Y_annlle)
all.equal(Y_lle, Y_annlle)

# Calculate embedding quality measures
ann_table_llenn <- calc_ann_table(e=llenn, nn.idx=truenn)
ann_table_lleann <- calc_ann_table(e=lleann, nn.idx=truenn)
ann_table_lleann_annoy <- calc_ann_table(e=lleann_annoy, nn.idx=truenn)
ann_table_lle <- bind_rows(ann_table_llenn, ann_table_lleann, ann_table_lleann_annoy) %>% 
  mutate(time = c(lleNNmed, lleANNmed, lleANNmed_annoy))
row.names(ann_table_lle) <- c("Exact NN", "k-d trees", "Annoy")
ann_table_lle
# M_T       M_C      LCMC       Qnx        W_n
# Exact NN  0.9692678 0.9105383 0.1919331 0.1974306 0.02703690
# k-d trees 0.9330203 0.8194326 0.1094104 0.1149079 0.05755084
# Annoy     0.9611444 0.9059509 0.1730268 0.1785243 0.03374718
# W_nu Procrustes       Rnx    recall     time
# Exact NN  0.001643739  0.6183496 0.1929941 0.6369332 21.62321
# k-d trees 0.001564043  0.6876028 0.1100152 0.6378675 23.53195
# Annoy     0.001594595  0.6432459 0.1739833 0.6413438 23.32004
all.equal(ann_table_llenn, ann_table_lleann)

save(llenn, lleann, lleann_annoy, ann_table_lle, pars, eps, lleNNtime, lleANNtime, lleANNtime_annoy,
     # Y_lle, Y_annlle, Y_annlle_annoy, 
     file = paste0('data/electricityplot_', method0, "_kdtree_eps", eps, "_", filename, '.rda') # for k-d trees
     # file = paste0('data/electricityplot/electricityplot_', method0, "_", annmethod, "_nt", nt, "_", filename, '.rda') # for Annoy
     )


###-------------------------------------------------
### Laplacian Eigenmap
###-------------------------------------------------
method0 <- "LaplacianEigenmaps"
method <- paste0("ann", method0)

leNNtime <- microbenchmark::microbenchmark(
  {lenn <- embed(train, .method = method, knn = pars$knn,
              annmethod = "kdtree",
              eps = pars$eps,
              nt = pars$nt,
              nlinks = pars$nlinks, ef.construction = 500,
              ndim = pars$ndim, distance = pars$distance, .mute = c("output"))},
  times=ntimes, 
  unit = "s"
)

leANNtime <- microbenchmark::microbenchmark(
  {leann <- embed(train, .method = method, knn = pars$knn,
               annmethod = "kdtree",
               eps = eps,
               nt = pars$nt,
               nlinks = pars$nlinks, ef.construction = 500,
                 ndim = pars$ndim, distance = pars$distance, .mute = c("output"))},
  times=ntimes, 
  unit = "s"
)

leANNtime_annoy <- microbenchmark::microbenchmark(
  {leann_annoy <- embed(train, .method = method, knn = pars$knn,
               annmethod = "annoy",
               eps = eps,
               nt = pars$nt, search.k = pars$search.k,
               nlinks = pars$nlinks, ef.construction = 500,
                 ndim = pars$ndim, distance = pars$distance, .mute = c("output"))},
  times=ntimes, 
  unit = "s"
)
Y_le <- lenn@data@data %>% as.data.frame()
Y_annle <- leann@data@data %>% as.data.frame()
Y_annle_annoy <- leann_annoy@data@data %>% as.data.frame()

all.equal(Y_le, Y_annle)
# par(mfrow=c(1,2))
# plot(Y_le)
# plot(Y_annle)

(leNNmed <- summary(leNNtime)$median)
(leANNmed <- summary(leANNtime)$median)
(leANNmed_annoy <- summary(leANNtime_annoy)$median)
(leNNmed - leANNmed)/leNNmed
(leNNmed - leANNmed_annoy)/leNNmed
# 
# Calculate embedding quality measures
ann_table_lenn <- calc_ann_table(e=lenn, nn.idx=truenn)
ann_table_leann <- calc_ann_table(e=leann, nn.idx=truenn)
ann_table_leann_annoy <- calc_ann_table(e=leann_annoy, nn.idx=truenn)
ann_table_le <- bind_rows(ann_table_lenn, ann_table_leann, ann_table_leann_annoy) %>% 
  mutate(time = c(leNNmed, leANNmed, leANNmed_annoy))
row.names(ann_table_le) <- c("Exact NN", "k-d trees", "Annoy")
ann_table_le
# M_T       M_C      LCMC       Qnx        W_n
# Exact NN  0.9569960 0.9158685 0.1283442 0.1338417 0.03923086
# k-d trees 0.9569505 0.9153464 0.1277808 0.1332784 0.03926850
# Annoy     0.9554578 0.9082919 0.1208284 0.1263259 0.04064037
# W_nu Procrustes       Rnx    recall     time
# Exact NN  0.001547460  0.7281344 0.1290537 0.6369332 4.336943
# k-d trees 0.001554613  0.7283728 0.1284872 0.6378675 3.260841
# Annoy     0.001552364  0.7294293 0.1214963 0.6413438 4.535429
all.equal(ann_table_lenn, ann_table_leann)

save(lenn, leann, leann_annoy, ann_table_le, pars, eps, leNNtime, leANNtime, leANNtime_annoy, 
     # Y_le, Y_annle, Y_annle_annoy, 
     file = paste0('data/electricityplot_', method0, "_kdtree_eps", eps, "_", filename, '.rda') # for k-d trees
     # file = paste0('data/electricityplot/electricityplot_', method0, "_", annmethod, "_nt", nt, "_", filename, '.rda') # for Annoy
     )

###-------------------------------------------------
### Hessian LLE
###-------------------------------------------------
# knn >= 5 for HLLE
method0 <- "HLLE"
method <- paste0("ann", method0)

hlleNNtime <- microbenchmark::microbenchmark(
  {hllenn <- embed(train, .method = method, knn = pars$knn,
                annmethod = "kdtree",
                eps = pars$eps,
                nt = pars$nt,
                nlinks = pars$nlinks, ef.construction = 500,
                ndim = pars$ndim, distance = pars$distance, .mute = c("output"))},
  times=ntimes, 
  unit = "s"
)

hlleANNtime <- microbenchmark::microbenchmark(
  {hlleann <- embed(train, .method = method, knn = pars$knn,
                 annmethod = "kdtree",
                 eps = eps,
                 nt = pars$nt,
                 nlinks = pars$nlinks, ef.construction = 500,
                 ndim = pars$ndim, distance = pars$distance, .mute = c("output"))},
  times=ntimes, 
  unit = "s"
)

hlleANNtime_annoy <- microbenchmark::microbenchmark(
  {hlleann_annoy <- embed(train, .method = method, knn = pars$knn,
                 annmethod = "annoy",
                 eps = eps,
                 nt = pars$nt, search.k = pars$search.k,
                 nlinks = pars$nlinks, ef.construction = 500,
                 ndim = pars$ndim, distance = pars$distance, .mute = c("output"))},
  times=ntimes, 
  unit = "s"
)
Y_hlle <- hllenn@data@data %>% as.data.frame()
Y_annhlle <- hlleann@data@data %>% as.data.frame()
Y_annhlle_annoy <- hlleann_annoy@data@data %>% as.data.frame()

all.equal(Y_annhlle, Y_annhlle_annoy)
# par(mfrow=c(1,2))
# plot(Y_hlle)
# plot(Y_annhlle)
# Y_hlle %>% filter(HLLE2 > 0.1) # three outliers on the top-right corner
# which(Y_hlle$HLLE2 > 0.1)
# train[which(Y_hlle$HLLE2 > 0.1),]

(hlleNNmed <- summary(hlleNNtime)$median)
(hlleANNmed <- summary(hlleANNtime)$median)
(hlleANNmed_annoy <- summary(hlleANNtime_annoy)$median)
(hlleNNmed - hlleANNmed)/hlleNNmed
(hlleNNmed - hlleANNmed_annoy)/hlleNNmed
# Calculate embedding quality measures
ann_table_hllenn <- calc_ann_table(e=hllenn, nn.idx=truenn)
ann_table_hlleann <- calc_ann_table(e=hlleann, nn.idx=truenn)
ann_table_hlleann_annoy <- calc_ann_table(e=hlleann_annoy, nn.idx=truenn)
ann_table_hlle <- bind_rows(ann_table_hllenn, ann_table_hlleann, ann_table_hlleann_annoy) %>% 
  mutate(time = c(hlleNNmed, hlleANNmed, hlleANNmed_annoy))
row.names(ann_table_hlle) <- c("Exact NN", "k-d trees", "Annoy")
ann_table_hlle
# M_T       M_C       LCMC        Qnx       W_n
# Exact NN  0.8856043 0.8939552 0.12956705 0.13506458 0.1117472
# k-d trees 0.9187908 0.8054756 0.06619799 0.07169552 0.0777555
# Annoy     0.9187908 0.8054756 0.06619799 0.07169552 0.0777555
# W_nu Procrustes        Rnx    recall     time
# Exact NN  0.001496074  0.7815187 0.13028329 0.6369332 7.416225
# k-d trees 0.001515136  0.7918193 0.06656393 0.6378675 6.269016
# Annoy     0.001515136  0.7918193 0.06656393 0.6378675 6.313746
all.equal(ann_table_hlleann, ann_table_hlleann_annoy)

save(hllenn, hlleann, hlleann_annoy, ann_table_hlle, pars, eps, hlleNNtime, hlleANNtime, hlleANNtime_annoy, 
     # Y_hlle, Y_annhlle, Y_annhlle_annoy, 
     file = paste0('data/electricityplot_', method0, "_kdtree_eps", eps, "_", filename, '.rda') # for k-d trees
     # file = paste0('data/electricityplot/electricityplot_', method0, "_", annmethod, "_nt", nt, "_", filename, '.rda') # for Annoy
     )

# Y_hlle %>% filter(HLLE2 > 0.1) # three outliers on the top-right corner
# which(Y_hlle$HLLE2 > 0.1) # 296 298 300, Sunday morning 4-5am
# train[which(Y_hlle$HLLE2 > 0.1)[1],] %>% plot(1:201, ., type = "l") 




# Summarise all four ML methods results
# First load all rda files in data/electricityplot/
### Quality measures
measure <- "M_T"
trustworthiness_1id <- bind_cols(
  Isomap = ann_table_isomap[,measure],
  LLE = ann_table_lle[,measure],
  "Laplacian Eigenmaps" = ann_table_le[,measure],
  "Hessian LLE" = ann_table_hlle[,measure]
) %>% as.data.frame()
rownames(trustworthiness_1id) <- c("Exact NN", "ANN k-d trees", "ANN Annoy")
trustworthiness_1id
save(trustworthiness_1id, file = paste0('data/electricityplot_trustworthiness_nt', nt, "_", filename, '.rda'))

### Running time
measure <- "time"
time_allid <- bind_cols(
  Isomap = ann_table_isomap[,measure],
  LLE = ann_table_lle[,measure],
  "Laplacian Eigenmaps" = ann_table_le[,measure],
  "Hessian LLE" = ann_table_hlle[,measure]
) %>% as.data.frame()
rownames(time_allid) <- c("Exact NN", "ANN k-d trees", "ANN Annoy")
time_allid
save(time_allid, file = paste0('data/electricityplot_time_nt', nt, "_", filename, '.rda'))

# Isomap      LLE Laplacian Eigenmaps Hessian LLE
# Exact NN      2.148260 2.925752            2.161146    1.648907
# ANN k-d trees 1.992215 2.898951            2.159031    1.591074
# ANN Annoy     2.141497 3.090594            2.418598    1.621195


# save.image(file = "fixsearchk.rda")
# load("fixsearchk.rda")
# trustworthiness_1id
# time_allid



###-------------------------------------------------
### plotting
###-------------------------------------------------
# load four plotting data files in data/electricityplot_Isomap_eps1_1id336tow.rda
# embedding plot
par(mfrow=c(2,2))
plot(Y_isomap)
plot(Y_lle)
plot(Y_le)
plot(Y_hlle)

# KNN
p1 <- Y_isomap %>% 
  ggplot(aes(x=ISO1, y=ISO2)) + 
  geom_point() 
p2 <- Y_lle %>% 
  ggplot(aes(x=LLE1, y=LLE2)) + 
  geom_point() 
p3 <- Y_le %>% 
  ggplot(aes(x=LEIM1, y=LEIM2)) + 
  geom_point() 
p4 <- Y_hlle %>% 
  ggplot(aes(x=HLLE1, y=HLLE2)) + 
  geom_point()
# ANN
pa1 <- Y_annisomap %>% 
  ggplot(aes(x=ISO1, y=ISO2)) + 
  geom_point() 
pa2 <- Y_annlle %>% 
  ggplot(aes(x=LLE1, y=LLE2)) + 
  geom_point() 
pa3 <- Y_annle %>% 
  ggplot(aes(x=LEIM1, y=LEIM2)) + 
  geom_point() 
pa4 <- Y_annhlle %>% 
  ggplot(aes(x=HLLE1, y=HLLE2)) + 
  geom_point() 
# ANN - Annoy
paa1 <- Y_annisomap_annoy %>% 
  ggplot(aes(x=ISO1, y=ISO2)) + 
  geom_point() 
paa2 <- Y_annlle_annoy %>% 
  ggplot(aes(x=LLE1, y=LLE2)) + 
  geom_point() 
paa3 <- Y_annle_annoy %>% 
  ggplot(aes(x=LEIM1, y=LEIM2)) + 
  geom_point() 
paa4 <- Y_annhlle_annoy %>% 
  ggplot(aes(x=HLLE1, y=HLLE2)) + 
  geom_point() 

# embedding
# (p1 + p2) / (p3 + p4) | (pa1 + pa2) / (pa3 + pa4)
# (p1 / pa1) | (p2 / pa2) | (p3 / pa3) | (p4 / pa4)

emb <- ((p1 + labs(title = "Isomap")) / pa1 / paa1) | 
  ((p2 + labs(title = "LLE")) / pa2 / paa2) | 
  ((p3 + labs(title = "Laplacian Eigenmaps")) / pa3 / paa3) | 
  ((p4 + labs(title = "Hessian LLE")) / pa4 / paa4) 
# &
#   theme(plot.title = element_text(hjust=0.5, size = 12))
# embedding_compare.png
ggsave(paste0("paper/figures/embedding_compare_allids", filename, ".png"), emb, width=10, height=6)

# color time patterns

# # 1) day of week
# week <- spdemand[,c('id', 'tow')][, dow := .(as.factor(ceiling(tow/48)))]
# # emb <- cbind(week, Y, Y_lapeig, Y_hlle)
# # emb %>% 
# #   ggplot(aes(x=LLE1, y=LLE2, color=dow)) + 
# #     geom_point() 
# # scale_color_brewer(palette="Dark2")
# # scale_color_brewer(palette="Accent")
# 
# e1 <- cbind(week, Y_annisomap)
# e2 <- cbind(week, Y_annlle)
# e3 <- cbind(week, Y_annle)
# e4 <- cbind(week, Y_annhlle)
# 
# pe1 <- e1 %>% 
#   ggplot(aes(x=ISO1, y=ISO2, color=dow)) + 
#   geom_point() 
# pe2 <- e2 %>% 
#   ggplot(aes(x=LLE1, y=LLE2, color=dow)) + 
#   geom_point() 
# pe3 <- e3 %>% 
#   ggplot(aes(x=LEIM1, y=LEIM2, color=dow)) + 
#   geom_point() +
#   theme(legend.position = "none")
# pe4 <- e4 %>% 
#   ggplot(aes(x=HLLE1, y=HLLE2, color=dow)) + 
#   geom_point() +
#   theme(legend.position = "none")
# 
# # embedding
# (pe1 + pe2) / 
#   (pe3 + pe4)


# 2) time of day
ID <- 1003
maxid <- sort(unique(spdemand[,id]))[nid] # change id == ID to id <= maxid
period <- spdemand[id == ID & tow <= ntow ,c('id', 'tow')][, tod := .(tow - 48*(ceiling(tow/48) - 1))]

t1 <- cbind(period, Y_isomap)
t2 <- cbind(period, Y_lle)
t3 <- cbind(period, Y_le)
t4 <- cbind(period, Y_hlle)

ta1 <- cbind(period, Y_annisomap)
ta2 <- cbind(period, Y_annlle)
ta3 <- cbind(period, Y_annle)
ta4 <- cbind(period, Y_annhlle)

taa1 <- cbind(period, Y_annisomap_annoy)
taa2 <- cbind(period, Y_annlle_annoy)
taa3 <- cbind(period, Y_annle_annoy)
taa4 <- cbind(period, Y_annhlle_annoy)

todcolor <- colorspace::scale_color_continuous_sequential(
  palette = "viridis",
  breaks = c(0, 12, 24, 36, 48),
  labels=c("00:00", "06:00", "12:00", "18:00", "24:00"),
  name="Time of day",
  guide=guide_colorbar(barwidth = 10))

pt1 <- t1 %>%
  ggplot(aes(x=ISO1, y=ISO2, color=tod)) +
  geom_point() +
  todcolor
pt2 <- t2 %>%
  ggplot(aes(x=LLE1, y=LLE2, color=tod)) +
  geom_point() +
  todcolor
pt3 <- t3 %>%
  ggplot(aes(x=LEIM1, y=LEIM2, color=tod)) +
  geom_point() +
  todcolor
pt4 <- t4 %>%
  ggplot(aes(x=HLLE1, y=HLLE2, color=tod)) +
  geom_point() +
  todcolor

pta1 <- ta1 %>%
  ggplot(aes(x=ISO1, y=ISO2, color=tod)) +
  geom_point() +
  todcolor
pta2 <- ta2 %>%
  ggplot(aes(x=LLE1, y=LLE2, color=tod)) +
  geom_point() +
  todcolor
pta3 <- ta3 %>%
  ggplot(aes(x=LEIM1, y=LEIM2, color=tod)) +
  geom_point() +
  todcolor
pta4 <- ta4 %>%
  ggplot(aes(x=HLLE1, y=HLLE2, color=tod)) +
  geom_point() +
  todcolor

ptaa1 <- taa1 %>%
  ggplot(aes(x=ISO1, y=ISO2, color=tod)) +
  geom_point() +
  todcolor
ptaa2 <- taa2 %>%
  ggplot(aes(x=LLE1, y=LLE2, color=tod)) +
  geom_point() +
  todcolor
ptaa3 <- taa3 %>%
  ggplot(aes(x=LEIM1, y=LEIM2, color=tod)) +
  geom_point() +
  todcolor
ptaa4 <- taa4 %>%
  ggplot(aes(x=HLLE1, y=HLLE2, color=tod)) +
  geom_point() +
  todcolor

(pt1 + pt2) /
  (pt3 + pt4) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom')
# tod_1id336tow.png

tod <- (((pt1 + labs(title = "Isomap")) / pta1 / ptaa1) |
          ((pt2 + labs(title = "LLE")) / pta2 / ptaa2) |
          ((pt3 + labs(title = "Laplacian Eigenmaps")) / pta3 / ptaa3) |
          ((pt4 + labs(title = "Hessian LLE")) / pta4 / ptaa4)) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom')

# tod <- ((pt1 / pta1) | (pt2 / pta2) | (pt3 / pta3) | (pt4 / pta4)) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = 'bottom') &
#   guides(color=guide_legend(nro=w=3,byrow=TRUE)) &
#   labs(color="Time of day")
tod
# tod_compare_1id336tow.png
ggsave(paste0("paper/figures/tod_compare_kdtreeannoy_", filename, ".png"), tod, width=10, height=6)




###-------------------------------------------------
### high density region plot
###-------------------------------------------------
# library(hdrcde)
nout <- 10
levels <- c(1, 20, 40, 60, 80, 99)
ph1 <- hdrscatterplot(x = Y_isomap[,1], y = Y_isomap[,2], levels, noutliers = nout, label = ids)
ph2 <- hdrscatterplot(x = Y_lle[,1], y = Y_lle[,2], levels, noutliers = nout, label = ids)
ph3 <- hdrscatterplot(x = Y_le[,1], y = Y_le[,2], levels, noutliers = nout, label = ids)
ph4 <- hdrscatterplot(x = Y_hlle[,1], y = Y_hlle[,2], levels, noutliers = nout, label = ids)

pha1 <- hdrscatterplot(x = Y_annisomap[,1], y = Y_annisomap[,2], levels, noutliers = nout, label = ids)
pha2 <- hdrscatterplot(x = Y_annlle[,1], y = Y_annlle[,2], levels, noutliers = nout, label = ids)
pha3 <- hdrscatterplot(x = Y_annle[,1], y = Y_annle[,2], levels, noutliers = nout, label = ids)
pha4 <- hdrscatterplot(x = Y_annhlle[,1], y = Y_annhlle[,2], levels, noutliers = nout, label = ids)

phaa1 <- hdrscatterplot(x = Y_annisomap_annoy[,1], y = Y_annisomap_annoy[,2], levels, noutliers = nout, label = ids)
phaa2 <- hdrscatterplot(x = Y_annlle_annoy[,1], y = Y_annlle_annoy[,2], levels, noutliers = nout, label = ids)
phaa3 <- hdrscatterplot(x = Y_annle_annoy[,1], y = Y_annle_annoy[,2], levels, noutliers = nout, label = ids)
phaa4 <- hdrscatterplot(x = Y_annhlle_annoy[,1], y = Y_annhlle_annoy[,2], levels, noutliers = nout, label = ids)

s <- (((ph1 + labs(title = "Isomap", x="ISO1", y="ISO2")) / 
         (pha1 + labs(x="ISO1", y="ISO2")) /
         (phaa1 + labs(x="ISO1", y="ISO2")))  | 
    ((ph2 + labs(title = "LLE", x="LLE1", y="LLE2")) / 
       (pha2 + labs(x="LLE1", y="LLE2")) /
       (phaa2 + labs(x="ISO1", y="ISO2"))) | 
    ((ph3 + labs(title = "Laplacian Eigenmaps", x="LE1", y="LE2")) / 
       (pha3 + labs(x="LE1", y="LE2")) /
       (phaa3 + labs(x="ISO1", y="ISO2"))) | 
    ((ph4 + labs(title = "Hessian LLE", x="HLLE1", y="HLLE2")) / 
       (pha4 + labs(x="HLLE1", y="HLLE2")) /
       (phaa4 + labs(x="ISO1", y="ISO2")))) + 
  plot_layout(guides = "collect") &
  guides(color=guide_legend(nrow=1,byrow=TRUE)) &
  theme(legend.position = 'bottom') 
s

# ((ph1 / pha1) | (ph2 / pha2) | (ph3 / pha3) | (ph4 / pha4)) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = 'bottom') &
#   labs(x = "", y = "")
## hdrXX_compare_1id336tow.png
# ggsave(paste0("paper/figures/hdr10_compare4ml_kdtreeannoy_1id", filename, ".png"), s, width=10, height=8)
ggsave(paste0("paper/figures/hdr10_compare4ml_kdtreeannoy_allids_nt", nt, "_", filename, ".png"), s, width=10, height=8)

# save(Y_isomap, Y_lle, Y_le, Y_hlle, Y_annisomap, Y_annlle, Y_annle, Y_annhlle, pars, eps, 
#      file = paste0('data/electricityplot_eps1_', filename, '.rda'))


hdr_isomap <- ((ph1 + labs(x="ISO1", y="ISO2")) | 
                 (pha1 + labs(x="ISO1", y="ISO2")) | 
                 (phaa1 + labs(x="ISO1", y="ISO2"))) + 
  plot_layout(guides = "collect") &
  guides(color=guide_legend(nrow=1,byrow=TRUE)) &
  theme(legend.position = 'bottom') 
hdr_isomap
ggsave(paste0("paper/figures/hdr10_compareisomap_kdtreeannoy_nt", nt, "_", filename, ".png"), hdr_isomap, width=10, height=4)

# hdr_lle <- ((ph2 + labs(x="LLE1", y="LLE2")) | (pha2 + labs(x="LLE1", y="LLE2"))) + 
#   plot_layout(guides = "collect") &
#   guides(color=guide_legend(nrow=1,byrow=TRUE)) &
#   theme(legend.position = 'bottom') 
# hdr_lle
# ggsave(paste0("paper/figures/hdr10_comparelle_", filename, ".png"), hdr_lle, width=10, height=6)

# rm(train)
# rm(spdemand)
# save.image(file = "data/electricityplot_allobjects_1id336tow.rda")



# For single household all tows
###-------------------------------------------------
## plot typical and anomalous tow
###-------------------------------------------------
# The most typical ones are those with Region=="1" in yellow.
# The most anomalous ones are those with Region==">99" in black. 
anomaly_nn <- ph1$data %>%  # View()
  rownames_to_column("tow") %>% 
  rename(ISO1 = Y_isomap...1., ISO2 = Y_isomap...2.) %>% 
  mutate(tow = as.numeric(tow), 
         dow = ceiling(tow/48),
         tod = (tow - 48*floor(tow/48))/2,
         method = "nn") %>% 
  filter(Region %in% c(">99", "1"))

anomaly_kdtree <- pha1$data %>%  # View()
  rownames_to_column("tow") %>% 
  rename(ISO1 = Y_annisomap...1., ISO2 = Y_annisomap...2.) %>%
  mutate(tow = as.numeric(tow), 
         dow = ceiling(tow/48),
         tod = (tow - 48*floor(tow/48))/2,
         method = "kdtree") %>% 
  filter(Region %in% c(">99", "1"))

anomaly_annoy <- phaa1$data %>%  # View()
  rownames_to_column("tow") %>% 
  rename(ISO1 = Y_annisomap_annoy...1., ISO2 = Y_annisomap_annoy...2.) %>%
  mutate(tow = as.numeric(tow), 
         dow = ceiling(tow/48),
         tod = (tow - 48*floor(tow/48))/2,
         method = "annoy") %>% 
  filter(Region %in% c(">99", "1"))

bind_rows(anomaly_nn, anomaly_kdtree, anomaly_annoy)
# tow       ISO1        ISO2  Region dow  tod method
# 1   24 -0.1695829 0.2909404      1   1 12.0     nn
# 2   77 -0.1179890 0.3179422      1   2 14.5     nn
# 3   81 -0.1458833 0.2921770      1   2 16.5     nn
# 4  310 -0.1476710 0.3262820      1   7 11.0     nn
# 5   38  0.9525183 0.4212948    >99   1 19.0     nn
# 6  134  1.0237164 0.3636394    >99   3 19.0     nn
# 7  182  0.9442243 0.4780956    >99   4 19.0     nn
# 8  327  1.0776950 0.2679600    >99   7 19.5     nn
# 9   24 -0.1865536 0.2911935      1   1 12.0 kdtree
# 10  77 -0.1344318 0.3245840      1   2 14.5 kdtree
# 11  81 -0.1611130 0.2947367      1   2 16.5 kdtree
# 12 310 -0.1664895 0.3296634      1   7 11.0 kdtree
# 13 134  1.0078732 0.3874026    >99   3 19.0 kdtree
# 14 135  1.1850625 0.1991039    >99   3 19.5 kdtree
# 15 327  1.0640752 0.3013102    >99   7 19.5 kdtree
# 16 328  1.1531247 0.1952406    >99   7 20.0 kdtree
# 17  24 -0.1585714 0.2859572      1   1 12.0  annoy
# 18  30 -0.1534337 0.2466920      1   1 15.0  annoy
# 19 222 -0.2069617 0.2873975      1   5 15.0  annoy
# 20 310 -0.1330758 0.3084639      1   7 11.0  annoy
# 21 134  1.0927996 0.3916109    >99   3 19.0  annoy
# 22 182  0.9507067 0.4667087    >99   4 19.0  annoy
# 23 326  0.9436023 0.4970019    >99   7 19.0  annoy
# 24 327  1.1539380 0.2926905    >99   7 19.5  annoy

# Typical: Wed 19:00, tow 134, 327
# Anomalys: Mon 12:00, tow 24, 310

# Plot the electricity demand distribution for two time periods, index 134 and 24
load("data/DT.rda")
head(DT)
DT_1id <- DT[id == "1003",]
tows <- c(134, 24, 327, 310) # single household, compare tow
DT2tow <- DT_1id[tow %in% tows,][, -"id"]
DT2tow
ptow <- DT2tow %>% 
  as_tibble() %>% 
  mutate(tow = as.factor(tow),
         typical = ifelse(tow %in% c(24, 310), TRUE, FALSE)) %>% 
  ggplot(aes(x=demand, group=tow, fill=typical)) + 
  geom_histogram(
    aes(y=..density..), # Histogram with density instead of count on y-axis
    binwidth=.1,
    colour="grey40", alpha=.2, # position="density"
  ) + 
  geom_density(alpha=.3) +  # Overlay with transparent density plot
  scale_fill_manual(values = c("black", "orange")) +
  facet_wrap(~tow, nrow = 2,
             labeller = as_labeller(c("24"="Monday 12pm", "134"="Wednesday 7pm", "310"="Sunday 11am", "327"="Sunday 11pm"))) +
  labs(x = "Electricity demand", y = "Density", fill = "Time of week") + 
  theme(legend.position = "None")
ptow
ggsave("paper/figures/electricity_compare2tow_1id336tow.png", ptow, width=10, height=6)


# # Plot pmf
# load("data/spdemand_3639id_336tow_201length.rda")
# spd2tow <- spdemand[id == "1003" & (tow %in% tows),][, -"id"]
# spd2tow
# pdtow <- spd2tow %>%
#   gather(key = "p", value = "prob", -tow) %>%
#   mutate(p = as.numeric(str_remove(p, "p"))) %>%
#   ggplot(aes(x = p, y = prob, group = tow, col = factor(tow))) +
#   geom_line(aes(linetype = factor(tow))) +
#   # facet_grid(tow ~ .) +
#   labs(x = "", y = "Probability", col = "Time of week", linetype = "Time of week")
# pdtow





# For all households
# id index 451 and 2425 to compare
###-------------------------------------------------
### plot typical households (run once)
###-------------------------------------------------
# Plot the electricity demand for two sample IDs, index 451 and 2425
library(tidyverse)
# load("data/DT.rda")
# head(DT)
# ids <- DT %>% dtplyr::lazy_dt() %>% pull(id) %>% unique() %>% sort()
# # DT2id <- DT[id %in% c(ids[c(485, 1280)], 1003),] # 4273 typical
# id2 <- ids[c(2057, 1280, 169)]
# DT2id <- DT[id %in% id2,] # 1321 typical
# save(DT2id, file = "data/electricityplot_DTcompare3id.rda")
load("data/electricityplot_DTcompare3id.rda")
DT2id
summary(DT2id)
p <- DT2id %>%
  ggplot(aes(x = day, y = demand, group = id)) +
  geom_line() +
  facet_grid(id ~ .) +
  labs(x = "Days", y = "Demand (kWh)")
p
ggsave("paper/figures/electricity_compare3id.png", p, width = 8, height = 6)

# Plot the electricity demand distribution for two anomalous households, index 481 and 1280,  and 1 typical household, index 169
# load("data/DT.rda")
# head(DT)
# DT_1id <- DT[id == "1003",]
# tows <- c(134, 24, 327, 310) # single household, compare tow
# DT2tow <- DT_1id[tow %in% tows,][, -"id"]
DT2id
p3ids <- DT2id %>% 
  as_tibble() %>% 
  mutate(id = as.factor(id),
         typical = ifelse(id == unique(DT2id$id)[1], TRUE, FALSE)) %>% 
  ggplot(aes(x=demand, group=id, fill=typical)) + 
  geom_histogram(
    aes(y=..density..), # Histogram with density instead of count on y-axis
    binwidth=.1,
    colour="grey40", alpha=.2, # position="density"
  ) + 
  geom_density(alpha=.3) +  # Overlay with transparent density plot
  scale_fill_manual(values = c("black", "orange")) +
  facet_wrap(~id, ncol = 1, scales = "free_y", strip.position="right") + # ,labeller = as_labeller(c("4669"="ID 4669", "3243"="ID 3243", "1321"="ID 1321"))
  labs(x = "Demand (kWh)", y = "Density") + 
  theme(legend.position = "None")
p3ids

# wrap_plots(p, p3ids)
pcomp <- cowplot::plot_grid(
  p, p3ids,
  align = "h", axis = "tb",
  nrow = 1, rel_widths = c(2, 1)
)
pcomp
ggsave("paper/figures/electricity_compare2id_isomap.png", pcomp, width = 8, height = 6)
# plotly::ggplotly(p, width = 600, height = 400)







###-------------------------------------------------
### plot 3 typical households distributions
###-------------------------------------------------
# # Or: plot the distribution of 336 tows for each household over 1.5 years
# # cde branch of gghdr github package, +geom_hdr_boxplot(aes(x=tow, y=demand))
# # remotes::install_github("ropenscilabs/gghdr@cde")
# remotes::install_github("ffancheng/gghdr") # add nxmargin argument for number of boxplots
# library(ggplot2)
# library(gghdr)
# ggplot(faithful, aes(y = eruptions)) +
#   geom_hdr_boxplot()
# ggplot(faithful, aes(x = waiting, y = eruptions)) +
#   geom_hdr_boxplot(nxmargin=20)
# ggplot(faithful) +
#   geom_point(aes(x = eruptions, y = waiting)) + 
#   geom_hdr_rug(aes(x = eruptions), prob = c(0.99, 0.5), fill = "blue")
# ggplot(data = faithful, aes(x = waiting, y=eruptions)) +
#   geom_point(aes(colour = hdr_bin(x = waiting, y = eruptions))) +
#   scale_colour_viridis_d(direction = -1) 
# 
# # hdrcde examples
# library(hdrcde)
# hdr.boxplot(faithful$eruptions)
# faithful.cde <- cde(faithful$waiting, faithful$eruptions,
#                     x.name="Waiting time", y.name="Duration time")
# plot(faithful.cde)
# plot(faithful.cde,xlab="Waiting time",ylab="Duration time",plot.fn="hdr")
# hdrscatterplot(faithful$waiting, faithful$eruptions)
# 
# 
# hdrcde for 3 ids
library(tidyverse)
library(hdrcde)
load("data/electricityplot_DTcompare3id.rda")
cde2id <-
  DT2id %>%
    as_tibble() %>%
    mutate(tow = as.integer(tow))


# id.cde0 <- cde(cde2id$tow, cde2id$demand, x.margin = 1:336,              x.name = "Time of week period", y.name = "Demand")
# Or equivalent nxmargin=336
id.cde <- cde(cde2id$tow, cde2id$demand, nxmargin = 336,
              x.name = "Time of week period", y.name = "Demand")
plot(id.cde)
plot(id.cde, plot.fn="hdr")

par(mfrow=c(3,1))
for (i in 1:3) {
  cdeplot <- filter(as_tibble(cde2id), id == unique(cde2id$id)[i])
  cde(cdeplot$tow, cdeplot$demand, x.margin = 1:336) %>%
    plot(xlab="Time of week period", ylab="Demand", plot.fn="hdr")
}


# modified gghdr for 3 ids
remotes::install_github("ffancheng/gghdr") # add nxmargin argument for number of boxplots
library(ggplot2)
library(gghdr)
load("data/electricityplot_DTcompare3id.rda")
cde2id <- 
  DT2id %>% 
  as_tibble() %>% 
  mutate(tow = as.integer(tow),
         dow = str_replace_all(dow, c("1"="Monday", "2"="Tuesday", "3" = "Wednesday", "4" = "Thursday", "5" = "Friday", "6" = "Saturday", "7" = "Sunday")), 
         dow = factor(dow, levels =c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
# cde2id <- cde2id[c(1:100, 25726+(1:100)),]
# # plot against 336 time of the week
# p_idtow <- cde2id %>%
#   ggplot(aes(x=tow, y=demand)) +
#   geom_hdr_boxplot(nxmargin=336, fill = "blue") +
#   facet_wrap(~id, ncol = 1) +
#   theme(legend.position = 'bottom') +
#   labs(x = "Time of week", y = "Demand (kWh)", fill = "Probability", colour = "Probability")
# ggsave("paper/figures/electricity_gghdr_3id_336tow.png", width = 8, height = 6)
# plot against 48 period of day and facet by 7 day of the week
p <- cde2id %>%
  ggplot(aes(x=period, y=demand)) +
  geom_hdr_boxplot(nxmargin=48, fill = "blue")
p_iddow <- p +
  scale_x_continuous(breaks = c(0, 12*(1:4))) +
  facet_grid(id~dow, 
             # labeller = as_labeller(c("1"="Monday", "2"="Tuesday", "3" = "Wednesday", "4" = "Thursday", "5" = "Friday", "6" = "Saturday", "7" = "Sunday"))
             ) +
  # theme(strip.text.x = element_text(size = 5)) + 
  labs(x = "Time of week", y = "Demand (kWh)", fill = "Probability", colour = "Probability") + # Legend title not changing?
  # scale_color_grey(name = "Prob") +
  # guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.position = 'bottom')
p_iddow
ggsave("paper/figures/electricity_gghdr_3id_7dow.png", p_iddow, width = 12, height = 8)




# Plot boxplot manually
Probability <- c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)
boxdata <-
  cde2id %>% 
  # filter(id==4669) %>%
  group_by(id, period, dow) %>% 
  summarise(q = quantile(demand, prob = Probability, type=8)) %>%
  mutate(name = rep(paste0("Q", 1:length(Probability)), n_distinct(id, period))) %>% 
  pivot_wider(names_from = name, values_from = q)
boxdata
boxcols <- gghdr:::darken_fill(col = rep("blue", length(Probability)), prob = Probability)
p_box <- boxdata %>% 
  ggplot() +
  geom_rect(aes(ymin=Q1, ymax=Q7, xmin=period-0.2, xmax=period+0.2), color = boxcols[7]) +
  geom_rect(aes(ymin=Q2, ymax=Q6, xmin=period-0.2, xmax=period+0.2), color = boxcols[6]) +
  geom_rect(aes(ymin=Q3, ymax=Q5, xmin=period-0.2, xmax=period+0.2), color = boxcols[5]) +
  geom_rect(aes(ymin=Q4, ymax=Q4, xmin=period-0.5, xmax=period+0.5), color = "black", alpha = 1) +
  # geom_line(aes(x=period, y=Q2), alpha = 0.5) +
  scale_x_continuous(breaks = c(0, 12*(1:4))) + 
  labs(x = "Time of week", y = "Demand (kWh)", color = "Probability") +
  facet_grid(id~dow, scales = "fixed") + 
  scale_color_manual(name = "Probability",
                     values = c("99.0%"=boxcols[7], "95.0%"=boxcols[6], "50.0%"=boxcols[5]), 
                     labels = c("99.0%", "95.0%", "50.0%"))
p_box
ggsave("paper/figures/electricity_hdrbox_3id_7dow.png", p_box, width = 12, height = 8)





###-------------------------------------------------
## specificity and sensitivity
###-------------------------------------------------
# xtab_set <- function(A, B){
#   both <- union(A,B)
#   KNN <- both %in% A %>% factor(levels=c(TRUE, FALSE))
#   ANN <- both %in% B %>% factor(levels=c(TRUE, FALSE))
#   return(table(KNN, ANN))
# }
# # set.seed(1)
# # A <- sample(letters[1:20],10,replace=TRUE)
# # B <- sample(letters[1:20],10,replace=TRUE)
# # xtab_set(A,B)
# # #        inB
# # # inA     FALSE TRUE
# # #   FALSE     0    5
# # #   TRUE      6    3
# 
# ht1 <- xtab_set(A=ph1$plot_env$outliers, B=pha1$plot_env$outliers)
# ht2 <- xtab_set(A=ph2$plot_env$outliers, B=pha2$plot_env$outliers)
# ht3 <- xtab_set(A=ph3$plot_env$outliers, B=pha3$plot_env$outliers)
# ht4 <- xtab_set(A=ph4$plot_env$outliers, B=pha4$plot_env$outliers)
# 
# library(caret)
# specificity(ht1) # Specificity is always 0, because the outlier union appears at least in one set
# sensitivity(ht1)
# sensitivity(ht2)
# sensitivity(ht3)
# sensitivity(ht4)
# 
# # For ID 1003
# # > sensitivity(ht1)
# # [1] 0.45
# # > sensitivity(ht2)
# # [1] 0.4
# # > sensitivity(ht3)
# # [1] 0.6
# # > sensitivity(ht4)
# # [1] 0.55


