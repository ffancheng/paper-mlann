# Case 3: consider all households, add up all 336 pairwise tow distribution distances for each households, thus the tow patterns are considered. 
# Question: if we add up the distances to get the pairwise distances, what is the input for manifold learning (it was spdemand with p1-p201 for each distribution)?
# Answer from Tas: If we use Total variation distance (TVD) to measure the distance between two tow distributions, then the TVD of two households is the sum of all 336 TVD distances between tow distributions. 
# Therefore, we will now turn the spdemand.rda from dimension (3639*336)*201 to 3639*(336*201), which means that we take all 336 distributions of a household as a vector now for each row, instead of 336 rows each consisting a vector, i.e. pmf. 

# quantile calculation grouped by `id` and `tow`: DT.rda -> qdemand.rda
# pmf of quantiles: qdemand.rda -> spdemand.rda
rm(list=ls())
library(data.table)
library(dtplyr)
library(dplyr)

# Now restrict to all ids and all dows
nid <- 3639
ntow <- 336
# quantiles and sqrt(pmf) length
prob <- seq(0.01, 0.99, by=0.01)
len <- 201

load("data/DT.rda")
DT0 <- DT
maxid <- sort(unique(DT[,id]))[nid]
DT <- DT[id <= maxid & tow <= ntow,]

###-------------------------------------------------
### Quantile calculation and smoothing
# Estimate quantiles for each household, each dow and half-hour period (i.e. each tow)
###-------------------------------------------------
# For all IDs, smooth across nearby time period (tow). 
# 336 distributions for each ID
# Then compute the Total Variation distance between households. 

# load the saved qdemand.rda directly from the code below
# load(paste0('data/qdemand_', nid, 'id', ntow, 'tow.rda'))
# first create a lazy_dt for dtplyr to work with
dt <- lazy_dt(DT)

qd <- dt %>%
  filter(tow==1 | tow==2 | tow==ntow) %>%
  group_by(id) %>%
  summarise(demand = quantile(demand, prob=prob, type=8)) %>% # prob=0.01-0.99
  mutate(prob = rep(prob, nid)) %>%
  mutate(tow = 1) %>% # keep time
  as.data.table()
qdemand <- qd

for(i in 2:(ntow-1))
{
  qd <- dt %>%
    filter(tow==i-1 | tow==i | tow==i+1) %>%
    group_by(id) %>%
    summarise(demand = quantile(demand, prob=prob, type=8)) %>%
    mutate(tow = i, prob = rep(prob, nid)) %>%
    as.data.table()
  qdemand <- rbindlist(list(qdemand, qd), use.names=TRUE)
}

qd <- dt %>%
  filter(tow==(ntow-1) | tow==ntow | tow==1) %>%
  group_by(id) %>%
  summarise(demand = quantile(demand, prob=prob, type=8)) %>%
  mutate(tow = ntow, prob = rep(prob, nid)) %>%
  as.data.table()
qdemand <- rbindlist(list(qdemand, qd), use.names=TRUE)[order(id)]

save(qdemand, file=paste0('data/qdemand_', nid, 'id', ntow, 'tow.rda'))





###-------------------------------------------------
### Input: quantiles for each id, qdemand
###-------------------------------------------------
# calculate pmf at each x (equally spaced) given quantiles q with probabilities p
pmf <- function(x, p, q)
{
  qcdf <- approx(q,p,xout=x,yleft=0,yright=1,ties=mean)$y 
  qpmf <- c(0,diff(qcdf))
  return(qpmf / sum(qpmf))
}

## each id as one observation, (ntow*len) columns 
# Output: nid=3639 rows, 1+(ntow*len)=67537 cols

# calculates abs(pmf) for each id and each tow
spmf <- function(qdemand, len=201)
{
  nid <- length(unique(qdemand[,id]))
  x <- seq(min(qdemand[,demand]), max(qdemand[,demand]), l=len) 
  spdemand <- qdemand %>% 
    lazy_dt() %>% 
    group_by(id, tow) %>% 
    summarise(sp = pmf(x, prob, demand)) %>% # pmf for total variation distance
    ungroup() %>% 
    mutate(p = rep(paste0('p', 1:len), nid*ntow)) %>%  # add a column to indicate p1:p201
    as.data.table() %>% 
    # dcast(id + tow ~ factor(p, levels=unique(p)), value.var = 'sp') # 1id
    dcast(id ~ tow + factor(p, levels=unique(p)), value.var = 'sp') # all ids

  setnames(spdemand, names(spdemand)[-1], paste0('t', names(spdemand)[-1]))
  
  return(spdemand)
}
(spdemand <- spmf(qdemand))
dim(spdemand)
head(spdemand[,1:5])

save(spdemand, file = paste0('data/spdemand_', nid, 'id_', ntow, "tow_", len, 'length.rda'))
# The object apdemand then goes to R/electricity_embedding_plot.R for embedding and plotting. 


# spdemand <-
# spdemand %>% 
#   # head() %>%
#   melt(measure.vars = paste0('p', 1:len),
#        variable.name = "p", value.name = "sp") %>% 
#   lazy_dt() %>% 
#   mutate(sp = sp^2) %>% 
#   as.data.table() %>% 
#   dcast(id ~ tow + factor(p, levels=unique(p)), value.var = 'sp')
# 
# setnames(spdemand, names(spdemand)[-1], gsub('th', "t", names(spdemand)[-1]))
