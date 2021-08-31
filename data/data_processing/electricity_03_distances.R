# This script is not used currently. 
# The quantile.distance() fucntion can be used to calculate the pairwise distance matrix using the input `qdemand` from data/electricity_02_quantiles_*.R
###-------------------------------------------------
### Calculating Hellinger distance
###-------------------------------------------------
# distance calculation: qdemand.rda -> qdistance.rda
rm(list=ls())
library(data.table)
library(dtplyr)
library(dplyr)

# Load quantile data
nid <- 10
ntow <- 336
# load(paste0('data/qdemand_', nid, 'id', ntow, 'tow.rda'))
# OR
load('data/qdemand_3639id336tow.rda')
qdemand0 <- qdemand
qdemand <- qdemand0 %>%
  lazy_dt() %>%
  filter(tow <= ntow,
         id <= sort(unique(qdemand0[,id]))[nid]) %>% 
  as.data.table()

Qdist <- function(prob, q, p, method='hellinger', unit='log')
{
  # Compute approximate densities
  probs <- sort(unique(qdemand[,prob])) 
  x <- seq(min(q,p),max(q,p), l=201) 
  qpmf <- pmf(x,probs,q)
  ppmf <- pmf(x,probs,p)
  qdist <- suppressMessages(philentropy::distance(rbind(qpmf, ppmf), method, unit))
  # philentrophy::getDistMethods()
  # or:
  # HL <- suppressWarnings(2 * sqrt( 1 - sum(sqrt(ppmf * qpmf))))
  
  return(qdist)
}


# data.table code in jsd_dt is much faster. 
# Use distace() in jsd() instead to calculate all kinds of distances
quantile_distance <- function(qdemand, method='hellinger', unit='log')
{
  idlist <- unique(qdemand[,id])
  nid <- length(idlist)
  dmat <- matrix(0, nrow=nid, ncol=nid)
  rownames(dmat) <- colnames(dmat) <- idlist
  
  for(i in 2:nid)
    for(j in 1:(i-1))
    {
      tmp <- qdemand[id==idlist[i],]
      tmp[, demand2:=qdemand[id==idlist[j],demand]]
      dmat[i,j] <- sum(tmp[, Qdist(prob, demand, demand2, method, unit), by=.(tow)]$V1)
    }
  
  return(as.dist(dmat + t(dmat)))
}
tictoc::tic()
quantile_distance(qdemand)
tictoc::toc()
#          1002     1003
# 1003 17.49831         
# 1004 18.55249  8.19239
# 0.066 sec elapsed


# If written in dtplyr syntax
# quantile_dist <- function(qdemand, method='hellinger', unit='log')
# {
#   idlist <- unique(qdemand[,id])
#   nid <- length(idlist)
#   dmat <- matrix(0, nrow=nid, ncol=nid)
#   rownames(dmat) <- colnames(dmat) <- idlist
#   qdemand <- lazy_dt(qdemand)
#   
#   # i <- 2; j <- 1
#   for(i in 2:nid)
#     for(j in 1:(i-1))
#     {
#       tmp <- as.tibble(qdemand) %>% 
#         spread(key=id, value=demand) %>% 
#         dplyr::select(tow, prob, 
#                demand=as.character(idlist[i]), 
#                demand2=as.character(idlist[j])) %>% 
#         lazy_dt()
#       # OR:
#       # demand2 <- filter(qdemand, id==idlist[j]) %>% 
#       #   mutate(demand2=demand)  %>% 
#       #   select(tow, prob, demand2)
#       # tmp <- filter(qdemand, id==idlist[i]) %>% 
#       #   select(-id) %>% 
#       #   left_join(demand2) 
#       tmp1 <- tmp %>% 
#         group_by(tow) %>% 
#         summarise(js = Qdist(prob, demand, demand2, method, unit)) %>%
#         summarise(js = sum(js)) %>% 
#         as.data.table()
#       dmat[i,j] <- tmp1[,js]
#     }
#   
#   return(as.dist(dmat + t(dmat)))
# }
# tictoc::tic()
# (hl <- quantile_dist(qdemand))
# tictoc::toc()
# # 0.092 sec
# #          1002     1003
# # 1003 17.49831         
# # 1004 18.55249  8.19239
# 
# (js <- quantile_dist(qdemand, method="jensen-shannon"))
# #          1002     1003
# # 1003 5.509944         
# # 1004 6.080849 1.439507
