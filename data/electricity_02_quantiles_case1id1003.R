# Case 1: considering one household ID 1003, with 48*7=336 distributions
# quantile calculation: DT.rda -> qdemand.rda
# sqrt(pmf) of quantiles: qdemand.rda -> spdemand.rda
#
# The output from this script is a subset of all households and all tows. For plotting, just use R/electricity_embedding_plot.R. 

# rm(list=ls())
library(data.table)
library(dtplyr)
library(dplyr)

# Now restrict to first `nid` ids and `ntow` tows
nid <- 3639
ntow <- 336
# quantiles and sqrt(pmf) length
prob <- seq(0.01, 0.99, by=0.01)
len <- 201


load("data/DT.rda")
# numid <- length(unique(DT[,id])) # 3639
maxid <- sort(unique(DT[,id]))[nid]
DT <- DT[id <= maxid & tow <= ntow,]
# dim(DT)
## (nid <- length(unique(DT[,id])))
## (ntow <- length(unique(DT[,tow])))
# save(DT, file=paste0('DT_', nid, 'id', ntow, 'tow.rda'))

# load(paste0('DT_', nid, 'id', ntow, 'tow.rda'))

###-------------------------------------------------
### Quantile calculation
# Estimate quantiles for each customer, each dow and each half-hour period (each tow)
###-------------------------------------------------

# first create a lazy_dt for dtplyr to work with
dt <- lazy_dt(DT)

qd <- dt %>% 
  filter(tow==1 | tow==2 | tow==ntow) %>%
  group_by(id) %>%
  summarise(demand = quantile(demand, prob=prob, type=8)) %>%
  mutate(tow = 1, prob = rep(prob, nid)) %>% 
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

# code in data.table
# For smoothing over neighbouring half-hours, 
# we need to loop
# change the tow to 336, 335 respectively
# qd <- DT[tow==1 | tow==2 | tow==10, 
#   lapply(.SD, function(x){quantile(x,prob=prob,type=8)}), 
#   by=.(id), .SDcols="demand"]
# qd[, tow:=1]
# qd[, prob:=rep(prob, nid)]
# qdemand <- qd
# for(i in 2:9)
# {
#   qd <- DT[tow==(i-1) | tow==i | tow==(i+1), 
#        lapply(.SD, function(x){quantile(x,prob=prob,type=8)}), 
#        by=.(id), .SDcols="demand"]
#   qd[,tow:=i]
#   qd[, prob:=rep(prob, nid)]
#   qdemand <- rbind(qdemand,qd)
# }
# qd <-DT[tow==9 | tow==10 | tow==1, 
#        lapply(.SD, function(x){quantile(x,prob=prob,type=8)}), 
#        by=.(id), .SDcols="demand"]
# qd[, tow:=10]
# qd[, prob:=rep(prob, nid)]
# qdemand <- rbind(qdemand, qd)

# save(qdemand, file='./data/qdemand_3id10tow_datatable.rda')



# load(paste0('qdemand_',nid,'id',ntow,'tow.rda'))

###-------------------------------------------------
### Input: \sqrt{pmf} for each id and each tow
###-------------------------------------------------
# calculate pmf at each x (equally spaced) given quantiles q with probabilities p
pmf <- function(x, p, q)
{
  qcdf <- approx(q,p,xout=x,yleft=0,yright=1,ties=mean)$y 
  qpmf <- c(0,diff(qcdf))
  return(qpmf / sum(qpmf))
}

## each (id, tow) as one observation, nid*ntow rows 
# Output: nid*ntow= 3*10 rows, (2+len)=203 cols

# calculates \sqrt{pmf} for each id and each tow
spmf <- function(qdemand)
{
  # len <- 201
  nid <- length(unique(qdemand[,id]))
  ntow <- length(unique(qdemand[,tow]))
  x <- seq(min(qdemand[,demand]), max(qdemand[,demand]), l=len) 
  # spdemand <- 
  #     qdemand[, .(sp = sqrt(pmf(x, prob, demand))), by=.(id, tow)][, p:=rep(paste0('p', 1:len), nid*ntow)] %>% 
  #     dcast(id + tow ~ p, value.var = 'sp')
  spdemand <- qdemand %>% 
    lazy_dt() %>% 
    group_by(id, tow) %>%
    summarise(sp = sqrt(pmf(x, prob, demand))) %>%
    ungroup() %>% 
    mutate(p = rep(paste0('p', 1:len), nid*ntow)) %>%  # add a column to indicate p1:p201
    as.data.table() %>% 
    dcast(id + tow ~ factor(p, levels=unique(p)), value.var = 'sp')
  
  return(spdemand)
}
(spdemand <- spmf(qdemand))
dim(spdemand)
head(spdemand[,1:5])

save(spdemand, file = paste0('data/spdemand_', nid, 'id', ntow, 'tow.rda'))
