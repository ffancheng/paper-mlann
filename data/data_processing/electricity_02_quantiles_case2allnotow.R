# Case 2: consider all households, but with all observations in one distribution for each household (regardless of all time period variables)
# quantile calculation grouped by `id`: DT.rda -> qdemand.rda
# abs(pmf) of quantiles: qdemand.rda -> spdemand.rda
rm(list=ls())
library(data.table)
library(dtplyr)
library(dplyr)

# Now restrict to first all ids and all dows
nid <- 3639
# ntow <- 336 # replace tow with dow to calculate all IDs
# ndow <- 7
# quantiles and sqrt(pmf) length
prob <- seq(0.01, 0.99, by=0.01)
len <- 201


load("data/DT.rda")
# numid <- length(unique(DT[,id])) # 3639
# maxid <- sort(unique(DT[,id]))[nid]
# DT <- DT[id <= maxid & dow <= ndow,]
# dim(DT)
## (nid <- length(unique(DT[,id])))
## (ndow <- length(unique(DT[,dow])))
# saveRDS(DT, file=paste0('data/DT_', nid, 'id', ndow, 'dow.rds'))

# DT <- readRDS(paste0('data/DT_', nid, 'id', ndow, 'dow.rda'))

###-------------------------------------------------
### Quantile calculation
# Estimate quantiles for each customer, each dow and each half-hour period (each tow)
###-------------------------------------------------
# For all IDs, no smoothing across nearby time period (dow/tow). One distribution for each ID
# Then compute the Hellinger distance between IDs. 

# load(paste0('data/qdemand_', nid, 'id_', 'notow.rda'))
# first create a lazy_dt for dtplyr to work with
dt <- lazy_dt(DT)

qd <- dt %>%
  # filter(dow==1 | dow==2 | dow==ndow) %>%
  group_by(id) %>%
  summarise(demand = quantile(demand, prob=prob, type=8)) %>% # prob=0.01-0.99
  # mutate(dow = 1) %>% # keep time 
  mutate(prob = rep(prob, nid)) %>%
  as.data.table()
qdemand <- qd
save(qdemand, file=paste0('data/qdemand_', nid, 'id_', 'notow.rda'))

# for(i in 2:(ndow-1))
# {
#   qd <- dt %>%
#     filter(dow==i-1 | dow==i | dow==i+1) %>%
#     group_by(id) %>%
#     summarise(demand = quantile(demand, prob=prob, type=8)) %>%
#     mutate(dow = i, prob = rep(prob, nid)) %>%
#     as.data.table()
#   qdemand <- rbindlist(list(qdemand, qd), use.names=TRUE)
# }
# 
# qd <- dt %>%
#   filter(dow==(ndow-1) | dow==ndow | dow==1) %>%
#   group_by(id) %>%
#   summarise(demand = quantile(demand, prob=prob, type=8)) %>%
#   mutate(dow = ndow, prob = rep(prob, nid)) %>%
#   as.data.table()
# qdemand <- rbindlist(list(qdemand, qd), use.names=TRUE)[order(id)]

# save(qdemand, file=paste0('data/qdemand_', nid, 'id', ndow, 'dow.rda'))





###-------------------------------------------------
### Input: \sqrt{pmf} for each id
###-------------------------------------------------
# calculate pmf at each x (equally spaced) given quantiles q with probabilities p
pmf <- function(x, p, q)
{
  qcdf <- approx(q,p,xout=x,yleft=0,yright=1,ties=mean)$y 
  qpmf <- c(0,diff(qcdf))
  return(qpmf / sum(qpmf))
}

## each (id, dow) as one observation, nid*ndow rows 
# Output: nid*ndow= 3*10 rows, (2+len)=203 cols

# calculates \sqrt{pmf} for each id and each dow
spmf <- function(qdemand, len=201)
{
  # len <- 201
  nid <- length(unique(qdemand[,id]))
  x <- seq(min(qdemand[,demand]), max(qdemand[,demand]), l=len) 
  spdemand <- qdemand %>% 
    lazy_dt() %>% 
    group_by(id) %>% 
    summarise(sp = abs(pmf(x, prob, demand))) %>% # abs() for total variation distance
    ungroup() %>% 
    mutate(p = rep(paste0('p', 1:len), nid)) %>%  # add a column to indicate p1:p201
    as.data.table() %>% 
    dcast(id ~ factor(p, levels=unique(p)), value.var = 'sp')
  
  return(spdemand)
}
(spdemand <- spmf(qdemand))
dim(spdemand)
head(spdemand[,1:5])

saveRDS(spdemand, file = paste0('data/spdemand_', nid, 'id_notow_', len, 'length.rds'))
# The object spdemand then goes to R/electricity_embedding_plot.R for embedding and plotting. 
