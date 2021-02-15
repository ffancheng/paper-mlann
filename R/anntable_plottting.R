# This script is for plotting the anntable result from ml_benchmark.R or ann_electricity.R
library(tidyverse)

i <- 1
dataname <- c("mnist_test", "electricity")[i]
if(i == 1) {
  N <- 10000} else{
    nid <- 10
    ntow <- 336
  }


# The following function can load all files and update the variables for each simulation scenario (1-9). 
# Input: i in 1:9
# Output: ann_table_i, p_recall_i, p_MT_i, p_time_i
loadfiles <- function(scen = 1){
  if(!(scen %in% 1:12)) stop("The scenario number must be an interger between 1 and 12.")
  
  methods <- c("annIsomap", "annLaplacianEigenmaps", "annHLLE", "annLLE")
  annmethods <- c("kdtree", "annoy", "hnsw")
  params <- c("epsilon", "ntrees", "nlinks")
  (simtable <- tidyr::expand_grid(methods, data.frame(annmethods, params)))
  
  (simj <<- simtable[scen,]) # Extract row of table
  method <<- simj$methods 
  annmethod <<- simj$annmethods %>% as.character() # In the cluster, these two columns are factors, need to convert
  para <<- simj$params %>% as.character()
  
  switch(dataname,
         "mnist_test" = {filename <<- paste0("ann/anntable_", annmethod, "_", method, "_mnist_test_", N/1000 ,"k") },
         "electricity" = {filename <<- paste0("electricity/anntable_", annmethod, "_", method, "_electricity_", nid, "id", ntow, "tow") }
  )
  load(here::here(paste0(filename, ".RData")))
  ann_table <<- ann_table
}


# trace(loadfiles, exit = function().last_env <<- parent.frame())
# untrace(loadfiles)
# loadfiles(scen = 1)
# ls(.last_env)
# get("ann_table", .last_env)
# sapply(ls(.last_env), function(x) get(x), simplify=F, USE.NAMES=T)


# Three plots are created based on ann_table: parameter against recall/M_T/time facetting

anntablePlots <- function(ann_table = ann_table, para = para, scen = 1){
  ### Plotting
  # recall
  p_recall <<- ggplot(ann_table, aes(x = time, y = recall, color = get(para))) +
    geom_line(size = 0.5) +
    geom_point(size=1) + 
    labs(y = "Recall", x = "Computation time (second)", color = para) 
  ggsave(paste0(filename, "_recall.pdf"), p_recall, width = 10, height = 6)
  
  # M_T
  p_MT <<- ggplot(ann_table, aes(x = time, y = M_T, color = get(para))) +
    geom_line(size = 0.5) +
    geom_point(size=1) + 
    labs(x = "Computation time (second)", y = "Trustworthness M_T", color = para) 
  ggsave(paste0(filename, "_MT.pdf"), p_MT, width = 10, height = 6)
  
  # computation time and qualities against parameter
  kdtime <- ann_table[, c(1:2, 4:6, 9:15, 7)]
  kd_df <- gather(kdtime, key = Step, value = Time, -1)
  kd_df$Step <- factor(kd_df$Step, levels = c(names(kdtime)[-c(1, 13)], "recall"))
  p_time <<- kd_df %>% 
    ggplot(aes(x = get(para), y = Time)) +
    geom_line() +
    facet_wrap(~Step, ncol = 4, scales = "free") +
    labs(x = para, y = "Quality measures                                 Computation time")
  ggsave(paste0(filename, "_time.pdf"), p_time, width = 10, height = 6)
}




# The compare plots are created after running all three scenerios for each annmethod. 
ComparePlots <- function(scen = 3){
  if(!(scen %in% c(3, 6, 9))) stop("The scenario number must be 3, 6 or 9.")
  if(!exists(paste0("ann_table_", scen-1:2))) stop("All three scenerios for the annmethod should be run before using ComparePlots().")
  
  ## ----compareMT, echo=FALSE----
  p_comp_MT <<- ggplot(get(paste0("ann_table_", scen-2)), aes(x = time, y = M_T)) +
    geom_line(data = get(paste0("ann_table_", scen-2)), size = 0.5, linetype = "longdash", aes(color = "kdtree")) +
    geom_line(data = get(paste0("ann_table_", scen-1)), size = 0.5, linetype = "twodash", aes(color = "annoy")) +
    geom_line(data = get(paste0("ann_table_", scen)), size = 0.5, linetype = "solid", aes(color = "hnsw")) +
    labs(x = "Computation time (second)", y = "Trustworthness M_T", 
         title = "High and to the left is better") +
    scale_color_manual(name = "Algorithms",
                       values = c(kdtree = "#00BFC4", annoy = "#7CAE00", hnsw = "#F8766D")) +
    geom_smooth(data = get(paste0("ann_table_", scen-2)), size = 0.5, linetype = "longdash", aes(x=time, y=M_T, color = "kdtree"), method = "loess", se = TRUE) + 
    geom_smooth(data = get(paste0("ann_table_", scen-1)), size = 0.5, linetype = "twodash", aes(x=time, y=M_T, color = "annoy"), method = "loess", se = TRUE) +
    geom_smooth(data = get(paste0("ann_table_", scen)), size = 0.5, linetype = "solid", aes(x=time, y=M_T, color = "hnsw"), method = "loess", se = TRUE)
  
  ggsave(paste0(stringr::str_remove(filename, paste0("_", annmethod)), "_compareMT.pdf"), p_comp_MT, width = 10, height = 6)
  
  
  ## ----compareWn, echo=FALSE, message=FALSE----
  p_comp_Wn <<- ggplot(get(paste0("ann_table_", scen-2)), aes(x = time, y = W_n)) +
    geom_line(data = get(paste0("ann_table_", scen-2)), size = 0.5, linetype = "longdash", aes(color = "kdtree")) +
    geom_line(data = get(paste0("ann_table_", scen-1)), size = 0.5, linetype = "twodash", aes(color = "annoy")) +
    geom_line(data = get(paste0("ann_table_", scen)), size = 0.5, linetype = "solid", aes(color = "hnsw")) +
    labs(x = "Computation time (second)", y = "Mean Relative Rank Error W_n", 
         title = "Low and to the left is better") +
    scale_color_manual(name = "Algorithms",
                       values = c(kdtree = "#00BFC4", annoy = "#7CAE00", hnsw = "#F8766D")) +
    geom_smooth(data = get(paste0("ann_table_", scen-2)), size = 0.5, linetype = "longdash", aes(x=time, y=W_n, color = "kdtree"), method = "loess", se = TRUE) + 
    geom_smooth(data = get(paste0("ann_table_", scen-1)), size = 0.5, linetype = "twodash", aes(x=time, y=W_n, color = "annoy"), method = "loess", se = TRUE) +
    geom_smooth(data = get(paste0("ann_table_", scen)), size = 0.5, linetype = "solid", aes(x=time, y=W_n, color = "hnsw"), method = "loess", se = TRUE)
  
  ggsave(paste0(stringr::str_remove(filename, paste0("_", annmethod)), "_compareWn.pdf"), p_comp_Wn, width = 10, height = 6)
}



###------------------------------------------------
### Plots for Rmd report
###------------------------------------------------
# lo <- 1:4
# tmp <- NULL
# for (i in lo) {
#   loadfiles(i)
#   tmp <- rbind(tmp, ann_table)
# }
# ann_table <- tmp

# Isomap
## ----kdtreeplot, echo=FALSE----
for (scen in 1:3) {
  loadfiles(scen)
  assign(paste0("ann_table_", scen), ann_table) %>% print()
  switch(annmethod,
         "kdtree" = {assign(para, ann_table$epsilon)},
         "annoy" = {assign(para, ann_table$ntrees)},
         "hnsw" = {assign(para, ann_table$nlinks)}
  )
  
  anntablePlots(ann_table, para, scen)
  assign(paste0("p_recall_", scen), p_recall) #%>% plot()
  assign(paste0("p_MT_", scen), p_MT) %>% plot()
  assign(paste0("p_time_", scen), p_time) %>% plot()
}

ComparePlots(scen = 3)
assign(paste0("p_comp_MT_", method), p_comp_MT) %>% plot()
assign(paste0("p_comp_Wn_", method), p_comp_Wn) %>% plot()


# LE
for (scen in 4:6) {
  loadfiles(scen)
  assign(paste0("ann_table_", scen), ann_table) %>% print()
  switch(annmethod,
         "kdtree" = {assign(para, ann_table$epsilon)},
         "annoy" = {assign(para, ann_table$ntrees)},
         "hnsw" = {assign(para, ann_table$nlinks)}
  )
  
  anntablePlots(ann_table, para, scen)
  assign(paste0("p_recall_", scen), p_recall) #%>% plot()
  assign(paste0("p_MT_", scen), p_MT) %>% plot()
  assign(paste0("p_time_", scen), p_time) %>% plot()
}

ComparePlots(scen = 6)
assign(paste0("p_comp_MT_", method), p_comp_MT) %>% plot()
assign(paste0("p_comp_Wn_", method), p_comp_Wn) %>% plot()


# HLLE
for (scen in 7:9) {
  loadfiles(scen)
  assign(paste0("ann_table_", scen), ann_table) %>% print()
  switch(annmethod,
         "kdtree" = {assign(para, ann_table$epsilon)},
         "annoy" = {assign(para, ann_table$ntrees)},
         "hnsw" = {assign(para, ann_table$nlinks)}
  )
  
  anntablePlots(ann_table, para, scen)
  assign(paste0("p_recall_", scen), p_recall) #%>% plot()
  assign(paste0("p_MT_", scen), p_MT) %>% plot()
  assign(paste0("p_time_", scen), p_time) %>% plot()
}

ComparePlots(scen = 9)
assign(paste0("p_comp_MT_", method), p_comp_MT) %>% plot()
assign(paste0("p_comp_Wn_", method), p_comp_Wn) %>% plot()

