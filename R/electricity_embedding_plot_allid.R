# Archived script. Use electricity_embedding_plot.R for all cases. 
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
Jmisc::sourceAll(here::here("R/sources")) 
set.seed(1)

###------------------------------------------------------
# For all 3639 households, run this section
nid <- 3639
ntow <- 336
len <- 201
filename <- paste0(nid, "id_", ntow, "tow_", len, "length")
load(paste0('data/spdemand_', filename, '.rda'))
train <- spdemand[, !"id"]#[1:100,]
N <- nrow(train)
# train0 <- train
# train <- train0[1:100,]
###------------------------------------------------------


###-------------------------------------------------
### KNN graph
###-------------------------------------------------
# Parameters fixed
# D <- 2
# K <- 20 
eps <- 5
annmethod <- "kdtree"

pars = list(knn = 20,
            eps = 0, 
            ndim = 2,
            get_geod = FALSE,
            annmethod = "kdtree", 
            nt = 50, 
            nlinks = 16, 
            ef.construction = 200, 
            distance = c("euclidean", "manhattan")[2])

knng <- makeKNNgraph(x = train, k = pars$knn, eps = pars$eps, 
                     annmethod = pars$annmethod, 
                     nt = pars$nt, nlinks = pars$nlinks, 
                     ef.construction = pars$ef.construction,
                     distance = pars$distance)$g
igraph::is.connected(knng)
# plot(knng)

anng <- makeKNNgraph(x = train, k = pars$knn, eps = eps, 
                     annmethod = pars$annmethod, 
                     nt = pars$nt, nlinks = pars$nlinks, 
                     ef.construction = pars$ef.construction,
                     distance = pars$distance)$g
igraph::is.connected(anng)
# plot(anng)
all.equal(knng, anng)

# # neighborhood graph NN vs ANN
# # igraph
# pknn <- ggraph(knng) +
#   geom_edge_link(colour = 'grey35') +
#   geom_node_point() +
#   geom_node_text(aes(label = V(knng)), colour = blues9[7],
#                  hjust = -.4, check_overlap = FALSE)
# # +
# #   labs(title = "Exact NN graph")
# pann <- ggraph(anng) +
#   geom_edge_link(colour = 'grey35') +
#   geom_node_point() +
#   geom_node_text(aes(label = V(anng)), colour = blues9[7],
#                  hjust = -.4, check_overlap = FALSE)
# # +
# #   labs(title = "Approximate NN graph")
# pnn <- (pknn + pann) &
#   theme(plot.title=element_text(hjust=0.5))
# # knng20_3639id336tow.png
# ggsave(paste0("paper/figures/knng20_", filename, ".png"), pnn, width=12, height=6)




# ###-------------------------------------------------
# ### Isomap
# ###-------------------------------------------------
# # eps <- 10
# method0 <- "Isomap"
# method <- paste0("ann", method0)
# 
# isomapNNtime <- microbenchmark::microbenchmark(
#   {isomapnn <- embed(train, .method = method, knn = pars$knn, 
#                      annmethod = annmethod,
#                      eps = pars$eps,
#                      nt = pars$nt,
#                      nlinks = pars$nlinks, ef.construction = 500,
#                      ndim = pars$ndim, .mute = c("output"))},
#   times=1, 
#   unit = "s"
# )
# 
# isomapANNtime <- microbenchmark::microbenchmark(
#   {isomapann <- embed(train, .method = method, knn = pars$knn, 
#                       annmethod = annmethod,
#                       eps = eps,
#                       nt = pars$nt,
#                       nlinks = pars$nlinks, ef.construction = 500,
#                       ndim = pars$ndim, .mute = c("output"))}, # , distance = pars$distance
#   times=1, 
#   unit = "s"
# )
# 
# isomapNNmed <- summary(isomapNNtime)$median
# isomapANNmed <- summary(isomapANNtime)$median
# (isomapNNmed - isomapANNmed)/isomapNNmed
# 
# Y_isomap <- isomapnn@data@data %>% as.data.frame()
# Y_annisomap <- isomapann@data@data %>% as.data.frame()
# 
# # par(mfrow=c(1,2))
# # plot(Y_isomap)
# # plot(Y_annisomap)
# # all.equal(Y_isomap, Y_annisomap)


# ###-------------------------------------------------
# ### LLE
# ###-------------------------------------------------
# method0 <- "LLE"
# method <- paste0("ann", method0)
# 
# Y_lle <- embed(train, .method = method, knn = pars$knn,
#                annmethod = annmethod,
#                eps = pars$eps,
#                nt = pars$nt,
#                nlinks = pars$nlinks, ef.construction = 500,
#                ndim = pars$ndim, distance = pars$distance, .mute = c("output"))@data@data %>% as.data.frame()
# 
# Y_annlle <- embed(train, .method = method, knn = pars$knn,
#                   annmethod = annmethod,
#                   eps = eps,
#                   nt = pars$nt,
#                   nlinks = pars$nlinks, ef.construction = 500,
#                   ndim = pars$ndim, distance = pars$distance, .mute = c("output"))@data@data %>% as.data.frame()
# 
# par(mfrow=c(1,2))
# plot(Y_lle)
# plot(Y_annlle)
# all.equal(Y_lle, Y_annlle)
# 
# ###-------------------------------------------------
# ### Laplacian Eigenmap
# ###-------------------------------------------------
# method0 <- "LaplacianEigenmaps"
# method <- paste0("ann", method0)
# 
# Y_le <- embed(train, .method = method, knn = pars$knn, 
#               annmethod = annmethod,
#               eps = pars$eps,
#               nt = pars$nt,
#               nlinks = pars$nlinks, ef.construction = 500,
#               ndim = pars$ndim, distance = pars$distance, .mute = c("output"))@data@data %>% as.data.frame()
# 
# Y_annle <- embed(train, .method = method, knn = pars$knn, 
#                  annmethod = annmethod,
#                  eps = eps,
#                  nt = pars$nt,
#                  nlinks = pars$nlinks, ef.construction = 500,
#                  ndim = pars$ndim, distance = pars$distance, .mute = c("output"))@data@data %>% as.data.frame()
# 
# # par(mfrow=c(1,2))
# # plot(Y_le)
# # plot(Y_annle)
# 
# ###-------------------------------------------------
# ### Hessian LLE
# ###-------------------------------------------------
# # knn >= 5 for HLLE
# method0 <- "HLLE"
# method <- paste0("ann", method0)
# 
# Y_hlle <- embed(train, .method = method, knn = pars$knn, 
#                 annmethod = annmethod,
#                 eps = pars$eps,
#                 nt = pars$nt,
#                 nlinks = pars$nlinks, ef.construction = 500,
#                 ndim = pars$ndim, distance = pars$distance, .mute = c("output"))@data@data %>% as.data.frame()
# 
# Y_annhlle <- embed(train, .method = method, knn = pars$knn, 
#                    annmethod = annmethod,
#                    eps = eps,
#                    nt = pars$nt,
#                    nlinks = pars$nlinks, ef.construction = 500,
#                    ndim = pars$ndim, distance = pars$distance, .mute = c("output"))@data@data %>% as.data.frame()
# 
# # par(mfrow=c(1,2))
# # plot(Y_hlle)
# # plot(Y_annhlle)
# Y_hlle %>% filter(HLLE2 > 0.1) # three outliers on the top-right corner
# which(Y_hlle$HLLE2 > 0.1)
# train[which(Y_hlle$HLLE2 > 0.1),]

# save(Y_lle, Y_annlle, knng, anng, pars, eps, 
#      file = paste0('data/electricityplot_', method0, "_", filename, '.rda'))


###-------------------------------------------------
### plotting
###-------------------------------------------------
# # embedding plot
# par(mfrow=c(2,2))
# plot(Y_isomap)
# plot(Y_lle)
# plot(Y_le)
# plot(Y_hlle)

# # KNN
# p1 <- Y_isomap %>% 
#   ggplot(aes(x=ISO1, y=ISO2)) + 
#   geom_point() 
# 
# p2 <- Y_lle %>%
#   ggplot(aes(x=LLE1, y=LLE2)) +
#   geom_point()
# 
# p3 <- Y_le %>% 
#   ggplot(aes(x=LEIM1, y=LEIM2)) + 
#   geom_point() 
# 
# p4 <- Y_hlle %>% 
#   ggplot(aes(x=HLLE1, y=HLLE2)) + 
#   geom_point()
# 
# # ANN
# pa1 <- Y_annisomap %>% 
#   ggplot(aes(x=ISO1, y=ISO2)) + 
#   geom_point() 
# 
# pa2 <- Y_annlle %>%
#   ggplot(aes(x=LLE1, y=LLE2)) +
#   geom_point()
# 
# pa3 <- Y_annle %>% 
#   ggplot(aes(x=LEIM1, y=LEIM2)) + 
#   geom_point() 
# 
# pa4 <- Y_annhlle %>% 
#   ggplot(aes(x=HLLE1, y=HLLE2)) + 
#   geom_point() 

# # embedding
# # (p1 + p2) / (p3 + p4) | (pa1 + pa2) / (pa3 + pa4)
# # (p1 / pa1) | (p2 / pa2) | (p3 / pa3) | (p4 / pa4)
# 
# emb <- ((p1 + labs(title = "Isomap")) / pa1) | 
#   ((p2 + labs(title = "LLE")) / pa2) | 
#   ((p3 + labs(title = "Laplacian Eigenmaps")) / pa3) | 
#   ((p4 + labs(title = "Hessian LLE")) / pa4) 
# # &
# #   theme(plot.title = element_text(hjust=0.5, size = 12))
# 
# # embedding_compare.png
# ggsave(paste0("paper/figures/embedding_compare", filename, ".png"), emb, width=10, height=6)



###-------------------------------------------------
### high density region plot
###-------------------------------------------------
# library(hdrcde)
nout <- 10
levels <- c(1, 20, 40, 60, 80, 99)
# ph1 <- hdrscatterplot(x = Y_isomap[,1], y = Y_isomap[,2], levels, noutliers = nout)
ph2 <- hdrscatterplot(x = Y_lle[,1], y = Y_lle[,2], levels, noutliers = nout)
# ph3 <- hdrscatterplot(x = Y_le[,1], y = Y_le[,2], levels, noutliers = nout)
# ph4 <- hdrscatterplot(x = Y_hlle[,1], y = Y_hlle[,2], levels, noutliers = nout) 
# 
# pha1 <- hdrscatterplot(x = Y_annisomap[,1], y = Y_annisomap[,2], levels, noutliers = nout)
pha2 <- hdrscatterplot(x = Y_annlle[,1], y = Y_annlle[,2], levels, noutliers = nout)
# pha3 <- hdrscatterplot(x = Y_annle[,1], y = Y_annle[,2], levels, noutliers = nout)
# pha4 <- hdrscatterplot(x = Y_annhlle[,1], y = Y_annhlle[,2], levels, noutliers = nout) 

# (pha1 + pha2) / 
#   (pha3 + pha4) + 
#   plot_layout(guides = "collect") + 
#   plot_annotation(tag_levels = 'A') & 
#   labs(x = "", y = "")
# ggsave("paper/figures/hdr10_1id336tow.png", width=8, height=6)


# hdr10 <- ((ph1 / pha1) | (ph2 / pha2) | (ph3 / pha3) | (ph4 / pha4)) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = 'bottom') &
#   labs(x = "", y = "")
# hdrXX_compare_1id336tow.png
# ggsave(paste0("paper/figures/hdr10_compare", method0, "_", filename, ".png"), hdr10, width=10, height=6)

# save(Y_isomap, Y_lle, Y_le, Y_hlle, Y_annisomap, Y_annlle, Y_annle, Y_annhlle,
#      file = paste0('data/electricityplot_', filename, '.rda'))

# (((ph1 + labs(title = "Isomap", x="ISO1", y="ISO2")) / (pha1 + labs(x="ISO1", y="ISO2")))  | 
#     ((ph2 + labs(title = "LLE", x="LLE1", y="LLE2")) / (pha2 + labs(x="LLE1", y="LLE2")))| 
#     ((ph3 + labs(title = "Laplacian Eigenmaps", x="LE1", y="LE2")) / (pha3 + labs(x="LE1", y="LE2"))) | 
# ((ph4 + labs(x="HLLE1", y="HLLE2")) | (pha4 + labs(x="HLLE1", y="HLLE2"))) + 
#   plot_layout(guides = "collect") &
#   guides(color=guide_legend(nrow=1,byrow=TRUE)) &
#   theme(legend.position = 'bottom')



# hdr_isomap <- ((ph1 + labs(title = "", x="ISO1", y="ISO2")) | (pha1 + labs(x="ISO1", y="ISO2")))  +
#   plot_layout(guides = "collect") &
#   guides(color=guide_legend(nrow=1,byrow=TRUE)) &
#   theme(legend.position = 'bottom') 

hdr_lle <- ((ph2 + labs(x="LLE1", y="LLE2")) | (pha2 + labs(x="LLE1", y="LLE2"))) + 
  plot_layout(guides = "collect") &
  guides(color=guide_legend(nrow=1,byrow=TRUE)) &
  theme(legend.position = 'bottom') 
hdr_lle
ggsave(paste0("paper/figures/hdr10_compare", method0, "_", filename, ".png"), hdr_lle, width=10, height=6)

# hdr_hlle <- ((ph4 + labs(x="HLLE1", y="HLLE2")) | (pha4 + labs(x="HLLE1", y="HLLE2"))) + 
#   plot_layout(guides = "collect") &
#   guides(color=guide_legend(nrow=1,byrow=TRUE)) &
#   theme(legend.position = 'bottom') 
# hdr_hlle
# ggsave(paste0("paper/figures/hdr10_comparehlle_", filename, ".png"), hdr_hlle, width=10, height=6)




# rm(train)
# rm(spdemand)
# save.image(file = "data/electricityplot_allobjects.rda")
message("Finished at: ", Sys.time())



###-----------------------------------------------------------------
## plotting all households
###-----------------------------------------------------------------
# Load the embedding for all households
nid <- 3639
ntow <- 336
len <- 201
filename <- paste0(nid, "id_", ntow, "tow_", len, "length")
load(paste0('data/spdemand_', filename, '.rda')) # 3639*(336*201+1)
train <- spdemand[, !"id"]
N <- nrow(train)

exactnn <- RANN.L1::nn2(train, query = train, k = pars$knn+1, treetype = "kd", searchtype = "standard", eps = 0)$nn.idx[, -1]
save(exactnn, file = "data/electricityplot_exactnn_allids.rda")

# calculate quality measures
# hdr plots for comparing NN and ANN

# load("data/electricityplot_Isomap_eps1_3639id_336tow_201length.rda")
# load("data/electricityplot_LLE_eps1_3639id_336tow_201length.rda")
# load("data/electricityplot_HLLE_eps1_3639id_336tow_201length.rda")
# load("data/electricityplot_LaplacianEigenmaps_eps1_3639id_336tow_201length.rda")
load("data/electricityplot_eps1_3639id_336tow_201length.rda")

ann_table <- as.data.frame(matrix(NA, nrow=8, ncol=8))
ann_table[1,] <- dr_quality(X=train, Y=Y_isomap, K = pars$knn, nn.idx = exactnn)$quality 
# ann_table[2,] <- dr_quality(X=train, Y=Y_annisomap, K = pars$knn, nn.idx = exactnn)$quality 
ann_table[3,] <- dr_quality(X=train, Y=Y_lle, K = pars$knn, nn.idx = exactnn)$quality 
# ann_table[4,] <- dr_quality(X=train, Y=Y_annlle, K = pars$knn, nn.idx = exactnn)$quality 
ann_table[5,] <- dr_quality(X=train, Y=Y_le, K = pars$knn, nn.idx = exactnn)$quality 
# ann_table[6,] <- dr_quality(X=train, Y=Y_annle, K = pars$knn, nn.idx = exactnn)$quality 
ann_table[7,] <- dr_quality(X=train, Y=Y_hlle, K = pars$knn, nn.idx = exactnn)$quality 
# ann_table[8,] <- dr_quality(X=train, Y=Y_annhlle, K = pars$knn, nn.idx = exactnn)$quality 








# id index 451 and 2425 to compare
###-------------------------------------------------
### plot typical households (run once)
###-------------------------------------------------
# Plot the electricity demand for two sample IDs, index 451 and 2425
library(tidyverse)
load("data/DT.rda")
head(DT)
ids <- DT %>% dtplyr::lazy_dt() %>% pull(id) %>% unique() %>% sort()
# DT2id <- DT[id %in% c(ids[c(485, 1280)], 1003),] # 4273 typical
id2 <- ids[c(2057, 1280, 169)]
DT2id <- DT[id %in% id2,] # 1321 typical
save(DT2id, file = "data/electricityplot_DTcompare2id.eda")
load("data/electricityplot_DTcompare2id.rda")
DT2id
summary(DT2id)
p <- DT2id %>%
  ggplot(aes(x = day, y = demand, group = id)) +
  geom_line() +
  facet_grid(id ~ .) +
  labs(x = "Days", y = "Demand (kWh)")
p

# Plot the electricity demand distribution for two anomalous households, index 481 and 1280,  and 1 typical household, index 169
load("data/DT.rda")
head(DT)
# DT_1id <- DT[id == "1003",]
# tows <- c(134, 24, 327, 310) # single household, compare tow
# DT2tow <- DT_1id[tow %in% tows,][, -"id"]
DT2id
p3ids <- DT2id %>% 
  as_tibble() %>% 
  mutate(id = as.factor(id),
         typical = ifelse(id == id2[3], TRUE, FALSE)) %>% 
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
ggsave("paper/figures/electricity_compare2id_isomap.png", pcomp, width = 8, height = 6)
# plotly::ggplotly(p, width = 600, height = 400)
