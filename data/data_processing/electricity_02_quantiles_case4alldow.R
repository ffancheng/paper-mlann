# not using case 4
# Case 4: consider all households, but with all observations in ndow=7 distribution for each household (regardless of the time of day (tod=48) variables). 
# This ends up having 7*3639=25473 distribtutions and points in the embedding plot. 
# quantile calculation: DT.rda -> qdemand.rda
# sqrt(pmf) of quantiles: qdemand.rda -> spdemand.rda
rm(list=ls())
library(data.table)
library(dtplyr)
library(dplyr)

# Now restrict to first all ids and all dows
nid <- 3639
# ntow <- 336 # replace tow with dow to calculate all IDs
ndow <- 7
# quantiles and sqrt(pmf) length
prob <- seq(0.01, 0.99, by=0.01)
len <- 101


load("data/DT.rda")
# numid <- length(unique(DT[,id])) # 3639
maxid <- sort(unique(DT[,id]))[nid]
DT <- DT[id <= maxid & dow <= ndow,]
# dim(DT)
## (nid <- length(unique(DT[,id])))
## (ndow <- length(unique(DT[,dow])))
saveRDS(DT, file=paste0('data/DT_', nid, 'id', ndow, 'dow.rds'))

# DT <- readRDS(paste0('data/DT_', nid, 'id', ndow, 'dow.rda'))

###-------------------------------------------------
### Quantile calculation
# Estimate quantiles for each customer, each dow and each half-hour period (each dow)
###-------------------------------------------------

# load(paste0('data/qdemand_', nid, 'id', ndow, 'dow.rda'))
# first create a lazy_dt for dtplyr to work with
dt <- lazy_dt(DT)

qd <- dt %>%
  filter(dow==1 | dow==2 | dow==ndow) %>%
  group_by(id) %>%
  summarise(demand = quantile(demand, prob=prob, type=8)) %>%
  mutate(dow = 1, prob = rep(prob, nid)) %>%
  as.data.table()
qdemand <- qd

for(i in 2:(ndow-1))
{
  qd <- dt %>%
    filter(dow==i-1 | dow==i | dow==i+1) %>%
    group_by(id) %>%
    summarise(demand = quantile(demand, prob=prob, type=8)) %>%
    mutate(dow = i, prob = rep(prob, nid)) %>%
    as.data.table()
  qdemand <- rbindlist(list(qdemand, qd), use.names=TRUE)
}

qd <- dt %>%
  filter(dow==(ndow-1) | dow==ndow | dow==1) %>%
  group_by(id) %>%
  summarise(demand = quantile(demand, prob=prob, type=8)) %>%
  mutate(dow = ndow, prob = rep(prob, nid)) %>%
  as.data.table()
qdemand <- rbindlist(list(qdemand, qd), use.names=TRUE)[order(id)]

save(qdemand, file=paste0('data/qdemand_', nid, 'id', ndow, 'dow.rda'))



# similar code in data.table
# For smoothing over neighbouring half-hours, 
# we need to loop
# change the dow to 336, 335 respectively
# qd <- DT[dow==1 | dow==2 | dow==10, 
#   lapply(.SD, function(x){quantile(x,prob=prob,type=8)}), 
#   by=.(id), .SDcols="demand"]
# qd[, dow:=1]
# qd[, prob:=rep(prob, nid)]
# qdemand <- qd
# for(i in 2:9)
# {
#   qd <- DT[dow==(i-1) | dow==i | dow==(i+1), 
#        lapply(.SD, function(x){quantile(x,prob=prob,type=8)}), 
#        by=.(id), .SDcols="demand"]
#   qd[,dow:=i]
#   qd[, prob:=rep(prob, nid)]
#   qdemand <- rbind(qdemand,qd)
# }
# qd <-DT[dow==9 | dow==10 | dow==1, 
#        lapply(.SD, function(x){quantile(x,prob=prob,type=8)}), 
#        by=.(id), .SDcols="demand"]
# qd[, dow:=10]
# qd[, prob:=rep(prob, nid)]
# qdemand <- rbind(qdemand, qd)

# save(qdemand, file='./data/qdemand_3id10dow_datatable.rda')





###-------------------------------------------------
### Input: \sqrt{pmf} for each id and each dow
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
spmf <- function(qdemand)
{
  # len <- 201
  nid <- length(unique(qdemand[,id]))
  ndow <- length(unique(qdemand[,dow]))
  x <- seq(min(qdemand[,demand]), max(qdemand[,demand]), l=len) 
  # spdemand <- 
  #     qdemand[, .(sp = sqrt(pmf(x, prob, demand))), by=.(id, dow)][, p:=rep(paste0('p', 1:len), nid*ndow)] %>% 
  #     dcast(id + dow ~ p, value.var = 'sp')
  spdemand <- qdemand %>% 
    lazy_dt() %>% 
    group_by(id, dow) %>%
    summarise(sp = sqrt(pmf(x, prob, demand))) %>% # sqrt() for hellinger distance
    ungroup() %>% 
    mutate(p = rep(paste0('p', 1:len), nid*ndow)) %>%  # add a column to indicate p1:p201
    as.data.table() %>% 
    dcast(id + dow ~ factor(p, levels=unique(p)), value.var = 'sp')
  
  return(spdemand)
}
(spdemand <- spmf(qdemand))
dim(spdemand)
head(spdemand[,1:5])

save(spdemand, file = paste0('data/spdemand_', nid, 'id', ndow, 'dow_', len, 'length.rda'))
