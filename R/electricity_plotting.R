rm(list=ls())
library(data.table)
library(dtplyr)
library(dplyr)
library(dimRed)
library(ggplot2)
library(patchwork)
library(hdrcde)
library(igraph)
library(MASS)
library(ggraph)

load('nn_embed_function.rda')
# load('neighbors_30id30tow.rda')
# load('neighbors_100id20tow.rda')
load('neighbors_1id336tow.rda')

ls()
knn <- 20
eps <- 0.5
# dimRedMethodList()
knng <- dimRed:::makeKNNgraph(x = indata, k = knn, eps = eps)
is.connected(knng)

###-------------------------------------------------
### LLE
###-------------------------------------------------
Y <- nn_embed(indata, knn = knn, eps = eps, ndim = 2)
# Eigenvalues: 7.256722e-04 2.425676e-07 3.105155e-17
# the situation when G is invertible. system is computationally singular: reciprocal condition number = 1.97323e-18
# RDRToolbox::LLE(as.matrix(indata), dim = 2, k = knn)

Y_lle <- dimRed::embed(indata, 'LLE', knn = knn)@data@data
all.equal(Y, Y_lle, tolerance = 1e-3)

# microbenchmark::microbenchmark(
# 	Y <- nn_embed(indata, knn=knn, ndim=2),
# 	Y_lle <- dimRed::embed(indata, 'LLE', .mute = c("output"), knn=knn)@data@data,
# 	times = 20
# )
# Unit: milliseconds
# min        lq     mean    median       uq
# 391.62743 406.31373 429.0859 415.10249 443.9645
# 11.91823  12.10109  13.4457  12.56108  13.4038


###-------------------------------------------------
### Laplacian Eigenmap
###-------------------------------------------------
Y_lapeig <- dimRed::embed(indata, 'LaplacianEigenmaps', .mute = c("output"), knn = knn)@data@data
# Eigenvalues: 2.272491e-02 1.374081e-02 6.538586e-17

###-------------------------------------------------
### Hessian LLE
###-------------------------------------------------
# knn >= 5 for HLLE
Y_hlle <- dimRed::embed(indata, 'HLLE', .mute = c("output"), knn = knn)@data@data
# Eigenvalues:  2.851231e-02  1.085024e-02 -7.655314e-17

###-------------------------------------------------
### plotting
###-------------------------------------------------
# load(paste0('SparseSpectralEmbed_', nid=100, 'id', ntow=20, 'tow.rda'))
par(mfrow=c(2,2))
plot(knng)
plot(Y)
plot(Y_lle)
plot(Y_lapeig)
plot(Y_hlle)


# igraph
p <- ggraph(knng) + 
  geom_edge_link(colour = 'grey35') + 
  geom_node_point() + 
  geom_node_text(aes(label = V(knng)), colour = blues9[7],
                 hjust = -.4, check_overlap = FALSE)


# or ggplot
# 1) day of week
week <- spdemand[,c('id', 'tow')][, dow := .(as.factor(ceiling(tow/48)))]
# emb <- cbind(week, Y, Y_lapeig, Y_hlle)
# emb %>% 
#   ggplot(aes(x=LLE1, y=LLE2, color=dow)) + 
#     geom_point() 
# scale_color_brewer(palette="Dark2")
# scale_color_brewer(palette="Accent")

e1 <- cbind(week, Y)
e2 <- cbind(week, Y_lapeig)
e3 <- cbind(week, Y_hlle)

p1 <- e1 %>% 
  ggplot(aes(x=LLE1, y=LLE2, color=dow)) + 
  geom_point() 

p2 <- e2 %>% 
  ggplot(aes(x=LEIM1, y=LEIM2, color=dow)) + 
  geom_point() +
  theme(legend.position = "none")

p3 <- e3 %>% 
  ggplot(aes(x=HLLE1, y=HLLE2, color=dow)) + 
  geom_point() +
  theme(legend.position = "none")

# embedding
(p + p1) / 
  (p2 + p3)

# 2) time of day
period <- spdemand[,c('id', 'tow')][, tod := .(as.factor(tow - 48*(ceiling(tow/48) - 1)))]

t1 <- cbind(period, Y)
pt1 <- t1 %>% 
  ggplot(aes(x=LLE1, y=LLE2, color=tod)) + 
  geom_point() 

t2 <- cbind(period, Y_lapeig)
pt2 <- t2 %>% 
  ggplot(aes(x=LEIM1, y=LEIM2, color=tod)) + 
  geom_point() +
  theme(legend.position = "none")

t3 <- cbind(period, Y_hlle)
pt3 <- t3 %>% 
  ggplot(aes(x=HLLE1, y=HLLE2, color=tod)) + 
  geom_point() +
  theme(legend.position = "none")

(p + pt1) / 
  (pt2 + pt3)

# hdrcde::hdr(Y)

# three methods in one plot
# zz <- reshape2::melt(list(LLE=e1, LaplacianEigenmaps=e2, HessianLLE=e3),
# 			id.vars="D")
# 
# ggplot(zz, aes(x=D, y=value, color=L1)) +
#   geom_point()
#   # + geom_line(data=zz[zz$L1!="LLE", ])
#   # + scale_color_manual("Dataset",
#   #      values = c("LLE" = "darkgreen",
#   #      			  "LaplacianEigenmaps" = "blue",
#   #      			  "HessianLLE" = "red"))

# save(knng, Y, Y_lle, Y_lapeig, Y_hlle, zz, 
# 	file = paste0('SparseSpectralEmbed_', nid, 'id', ntow, 'tow.rda'))

###-------------------------------------------------
### high density region plot
###-------------------------------------------------
h1 <- hdrscatterplot(x = Y[,1], y = Y[,2], noutliers = 20)
h2 <- hdrscatterplot(x = Y_lapeig[,1], y = Y_lapeig[,2], noutliers = 20) +
  theme(legend.position = "none")
h3 <- hdrscatterplot(x = Y_hlle[,1], y = Y_hlle[,2], noutliers = 20) +
  theme(legend.position = "none")

(p + h1) / 
  (h2 + h3)
