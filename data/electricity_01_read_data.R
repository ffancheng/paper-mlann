# raw data (Data.xtx) -> DT.rda
source("install_packages.R")
library(data.table)

# Clean workspace
rm(list=ls())

# Read combined file
tmp <- fread(file.path("SmartMeterData/Data.txt"))

# Consider only residential data
info <- readxl::read_excel("SmartMeterData/SME and Residential allocations.xlsx")
residential <- as.matrix(info[info[,2] == 1,1]) 
# 4225 resid, 485 SME, 1735 others
k <- is.element(tmp$V1, residential)
tmp <- tmp[k,]

# Find day and period
day <- as.numeric(substr(as.character(tmp[, V2]),1,3))
period <- as.numeric(substr(as.character(tmp[, V2]),4,5))
dow <- (day + 2) %% 7 + 1
doy <- day %% 365 + 1
# DOW=1 corresponds to Monday, 7 to Sunday

DT <- data.table(cbind(tmp[, V1], day, dow, doy, period, tmp[, V3]))
setnames(DT, colnames(DT), c("id", "day", "dow", "doy", "period", "demand"))
summary(DT)

# Remove periods 49 and 50
DT <- subset(DT, period <= 48)

# Compute time of week
DT[, tow := (48*(DT[,dow] - 1) + DT[,period])]

# Keep only ids where we have complete data
keepids <- table(DT[,id]) >= 25726   # deleted 586 ids
keepids <- as.numeric(names(keepids)[keepids])
keep <- is.element(DT[,id], keepids)
DT <- DT[keep,]

# Clean up temporary files
rm(tmp,day,dow,doy,period,info,k,keepids,keep,maxid,residential)

save(DT, file='DT.rda')

# Now restrict to first 3 ids and 10 tows
# load('DT.rda')
# # numid <- length(unique(DT[,id])) # 3639
# maxid <- sort(unique(DT[,id]))[100]
# DT <- DT[id <= maxid & tow <= 20,]
# dim(DT)
# (nid <- length(unique(DT[,id])))
# (ntow <- length(unique(DT[,tow])))

# save(DT, file=paste0('DT_', nid, 'id', ntow, 'tow.rda'))
