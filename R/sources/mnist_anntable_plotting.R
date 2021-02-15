# # This script is for plotting the anntable result from ann_mnist.R or ann_electricity.R
###------------------------------------------------
### Set up for Rmd report
###------------------------------------------------
# rm(list=ls())
# library(tidyverse)
# 
# i <- 1
# dataname <- c("MNISTtest", "electricity")[i]
# if(i == 1) {
#   N <- 10000
#   folder <- "annmnist/"
# } else{
#   nid <- 30
#   ntow <- 336
#   folder <- paste0("annelectricity/", nid, "id", ntow, "tow/")
# }

# simtable <- expand_grid(method = c("annIsomap", "annLaplacianEigenmaps", "annHLLE", "annLLE"),
#                         annmethod = c("kdtree", "annoy", "hnsw"))
# wholetable <- simtable %>%
#   group_by(method, annmethod) %>%
#   nest() %>%
#   mutate(ann_table = map(data, ~ combineanntable(method, annmethod)))

# ann_table <- list()
# for (i in 1:nrow(simtable)) {
#   ann_table[[i]] <- wholetable[i,] %>%
#     unnest(ann_table, names_repair = tidyr_legacy) %>%
#     ungroup() %>%
#     dplyr::select(-data, -method1, -annmethod1)
# }



###------------------------------------------------
### tool functions
###------------------------------------------------

combineanntable <- function(method = c("annIsomap", "annLaplacianEigenmaps", "annHLLE", "annLLE"), 
                      annmethod = c("kdtree", "annoy", "hnsw")){
  # Combine all ann_table from with different parameter values for each combination of manifold learning method (method) and ANN method (annmethod)
  # Input: method, annmethod
  # Output: ann_table
  # Example: combineanntable(method = "annIsomap", annmethod = "kdtree")
  method <- match.arg(method)
  annmethod <- match.arg(annmethod)
  epsilon <- seq(0, 5, 0.1) # kdtree
  ntrees <- seq(4, 100, 2) #2:100, # Annoy
  nlinks <- seq(2, 200, 2) #2:200 # HNSW
  par <- switch (annmethod,
    "kdtree" = {par <- epsilon},
    "annoy" = {par <- ntrees},
    "hnsw" = {par <- nlinks}
  )
  switch(dataname,
         "MNISTtest" = {filename <- here::here(paste0(folder, "anntable_", method, "_", annmethod, "_", par, "_", dataname, "_", N,".rds")) },
         "electricity" = {filename <- here::here(paste0(folder, "anntable_", method, "_", annmethod, "_", par, "_", dataname, "_", nid, "id", ntow, "tow.rds")) }
  )
  
  ann_table <- filename %>% 
    # list.files(pattern = ".rds") %>%
    purrr::map(readRDS) %>% 
    bind_rows() %>% 
    mutate(method = method, annmethod = annmethod) %>% 
    relocate(method, annmethod) %>% 
    mutate(W_n = 1 - W_n, W_nu = 1 - W_nu, Procrustes = 1 - Procrustes)
  return(ann_table)
}

# filtering function - turns outliers into NAs to be removed
# https://stackoverflow.com/questions/59140960/remove-outliers-and-reduce-ylim-appropriately-for-each-facet-in-ggplot2
filter_lims <- function(x, low=1, up=5){
  l <- boxplot.stats(x)$stats[low]
  u <- boxplot.stats(x)$stats[up]
  
  for (i in 1:length(x)){
    x[i] <- ifelse(x[i]>l & x[i]<u, x[i], NA)
  }
  return(x)
}


###------------------------------------------------
### Plotting functions
###------------------------------------------------

# Three plots are created based on ann_table: parameter against recall/M_T/time faceting

anntablePlots <- function(ann_table, measure = c(M_T, M_C, LCMC, Qnx, W_n, W_nu, Procrustes, Qlocal, Qglobal, Rnx, mean_Rnx, auc_lnK_Rnx)){
  # Plot ANN parameter against three quality measures, recall, trustworthiness and computation time
  # Input: combined ann_table from function combineanntable(), measure name from colnames(ann_table)
  # Output: three ggplot p_recall, p_MT, p_time and three saved pdf files
  # Example: anntablePlots(combineanntable(method = "annIsomap", annmethod = "kdtree"), measure = M_T)
  # require(rlang)
  
  # recall
  filename <- paste0(folder, dataname, "_", ann_table$method[1], "_", ann_table$annmethod[1])
  p_recall <- ggplot(ann_table, aes(x = knn_time, y = recall, color = par)) +
    geom_line(size = 0.7) +
    geom_point(size=1) + 
    labs(y = "Recall rate", x = "Computation time (seconds)") 
  
  # measure vs time with color in parameter
  p_measure <- ggplot(ann_table, aes(x = time, y = {{measure}}, color = par)) +
    geom_line(size = 0.7) +
    geom_point(size=1) + 
    labs(x = "Computation time (seconds)", y = rlang::enquo(measure))
  
  # computation time and qualities against parameter
  kdtime <- ann_table[, c(1:2, 4:6, 9:15, 7)+2]
  kd_df <- gather(kdtime, key = Step, value = Time, -1)
  kd_df$Step <- factor(kd_df$Step, levels = c(names(kdtime)[-c(1, 13)], "recall"))
  p_par <- kd_df %>% 
    ggplot(aes(x = par, y = Time)) +
    geom_line() +
    facet_wrap(~Step, ncol = 4, scales = "free") +
    labs(x = "ANN parameter", y = "Quality measures                                 Computation time")
  
  return(list(p_recall=p_recall, p_measure=p_measure, p_par=p_par))
}



recallplot <- function(method = c("annIsomap", "annLaplacianEigenmaps", "annHLLE", "annLLE"),
                       annmethod = c("kdtree", "annoy", "hnsw")){
  # Plot recall rate against computation time for three ANN methods, k-d trees, Annoy and HNSW
  # Input: manifold learning method and annmethod to be compared
  # Output: a ggplot comparing recall rate for all `annmethod`
  # Example: recallplot(method = "annIsomap", annmethod = c("kdtree", "annoy", "hnsw"))

  method <- match.arg(method)
  if(is.null(annmethod)) annmethod <- c("kdtree", "annoy", "hnsw")
  ann_table <- purrr::map2(method, annmethod, combineanntable) %>% 
    dplyr::bind_rows()
  
  # xintercept for true NN time for a vertical line
  nntime <- ann_table %>% 
    dplyr::select(par, knn_time) %>% 
    filter(par==0) %>% 
    dplyr::select(knn_time) %>% 
    as.numeric()
  
  # compare recall rate
  p_recall <- ann_table %>% 
    dplyr::select(method, annmethod, knn_time, recall) %>% 
    filter(knn_time <= nntime + 5) %>% # remove all points on the right of the true NN vertical line
    pivot_longer(cols = !c(method, annmethod, knn_time),
                 names_to = "measures",
                 values_to = "value") %>% 
    mutate(measures = fct_inorder(measures)) %>% 
    ggplot(aes(x = knn_time, y = value, group = measures)) +
    geom_point(aes(color = annmethod, group = measures, shape = annmethod), size = 4) + 
    # geom_line(aes(color = annmethod, group = annmethod, linetype = annmethod), size = 0.7) + 
    # geom_smooth(aes(color = annmethod, group = annmethod, linetype = annmethod), size = 0.7, method = "loess", se = TRUE, method.args = list(degree=1)) + 
    geom_vline(xintercept = nntime, linetype = 2) + # add KNN vertical line
    guides(color=guide_legend(keywidth = 2, keyheight = 1)) + 
    theme(legend.position = "right") +
    scale_color_hue(labels = c("Annoy", "HNSW", "k-d trees")) + 
    scale_shape_discrete(labels = c("Annoy", "HNSW", "k-d trees")) + 
    labs(x = "Nearest neighbor searching time (second)", y = "Recall rate", color = "ANN algorithms", shape = "ANN algorithms") # , linetype = "ANN algorithms"
  
  return(p_recall)
}



measureplot <- function(method = c("annIsomap", "annLaplacianEigenmaps", "annHLLE", "annLLE"),
                        measure = c(M_T, M_C, LCMC, Qnx, W_n, W_nu, Procrustes, Qlocal, Qglobal, Rnx, mean_Rnx, auc_lnK_Rnx),
                        annmethod = c("kdtree", "annoy", "hnsw"),
                        rm.outliers = FALSE, 
                        keep.smooth = FALSE){
  # Plot one quality measure against computation time for three ANN methods, k-d trees, Annoy and HNSW
  # Input: manifold learning method, `measure` name without quotes, which ANN method to be compared
  # Output: a ggplot comparing `measure` for three ANN methods
  # Example: measureplot(method = "annIsomap", measure = M_T)
  
  method <- match.arg(method)
  if(is.null(annmethod)) annmethod <- c("kdtree", "annoy", "hnsw")
  
  ann_table <- purrr::map2(method, annmethod, combineanntable) %>% 
    dplyr::bind_rows() 
  ann_table1 <- ann_table%>% 
    dplyr::select(method, annmethod, time, {{measure}})
  ylab <- rlang::enquo(measure)
  # ylab <- dplyr::case_when(measure=="M_T" ~ "Trustworthiness",
  #                          measure=="M_C" ~ "Continuity",
  #                          measure=="LCMC" ~ "LCMC",
  #                          measure=="Qnx" ~ "Co-ranking",
  #                          measure=="W_n" ~ "MRREs",
  #                          measure=="Procrustes" ~ "Procrustes")

  if(ncol(ann_table1) > 4) {
    ann_table1 <- ann_table1[,1:4]
    ylab <- "Trustworthiness"
    message("Quality measure not specified in `measure`. Trustworthiness is chosen by default. ")
  }
  
  # xintercept for true NN time of the whole process (`time`) for a vertical line
  nntime <- ann_table %>% 
    dplyr::select(par, time) %>% 
    filter(par==0) %>% 
    dplyr::select(time) %>% 
    as.numeric()

  p_measure <- ann_table1 %>% 
    # dplyr::select(method, annmethod, time, {{measure}}) %>% 
    filter(time <= nntime + 10) %>%
    pivot_longer(cols = !c(method, annmethod, time),
                 names_to = "measures",
                 values_to = "value") %>% 
    mutate(measures = fct_inorder(measures)) %>% 
    group_by(measures) %>%
    mutate(value1 = ifelse(rm.outliers & annmethod == "hnsw", filter_lims(value), value)) %>% # remove outliers for HNSW
    ungroup() %>%
    ggplot(aes(x = time, y = value1, group = measures)) +
    geom_point(aes(color = annmethod, group = measures, shape = annmethod)) + 
    # geom_smooth(aes(color = annmethod, group = annmethod, linetype = annmethod), size = 0.7, method = "loess", se = TRUE, span = 0.2, method.args = list(degree=1)) + 
    geom_vline(xintercept = nntime, linetype = 2) + # add KNN vertical line
    guides(color=guide_legend(keywidth = 3, keyheight = 1)) + 
    theme(legend.position = "right") +
    scale_color_hue(labels = c("Annoy", "HNSW", "k-d trees")) + 
    scale_shape_discrete(labels = c("Annoy", "HNSW", "k-d trees")) + 
    labs(x = "Computation time (second)", y = ylab, color = "ANN algorithms", linetype = "ANN algorithms", shape = "ANN algorithms")
  
  if(keep.smooth){
    p_measure <- p_measure + 
      geom_smooth(aes(color = annmethod, group = annmethod, linetype = annmethod), size = 0.7, method = "loess", se = TRUE, span = 0.2, method.args = list(degree=1))
  }
  
  return(p_measure)
}



# Modify measureplot() to show multiple manifold learning methods in one measure
mlfacets <- function(method = c("annIsomap", "annLLE", "annLaplacianEigenmaps", "annHLLE"),
                     measure = c("M_T", "M_C", "LCMC", "Qnx", "W_n", "W_nu", "Procrustes", "Qlocal", "Qglobal", "Rnx", "mean_Rnx", "auc_lnK_Rnx"),
                     annmethod = c("kdtree", "annoy", "hnsw"),
                     rm.outliers = FALSE, 
                     keep.smooth = FALSE,
                     percentage = FALSE) {
  # Plot one quality measure against computation time for multiple manifold learning methods in facets
  # Input: manifold learning method, `measure` name, which ANN method to be compared
  # Output: a ggplot comparing `measure` for three ANN methods
  # Example: mlfacets(method = c("annIsomap", "annLLE", "annLaplacianEigenmaps", "annHLLE"), measure = "M_T", annmethod = c("kdtree", "annoy", "hnsw"), percentage = F)
  
  require(tidyverse)
  if(is.null(method)) method <- c("annIsomap", "annLLE", "annLaplacianEigenmaps", "annHLLE")
  if(is.null(annmethod)) annmethod <- c("kdtree", "annoy", "hnsw")
  measure <- match.arg(measure)
  ylabel <- dplyr::case_when(measure=="M_T" ~ "Trustworthiness",
                             measure=="M_C" ~ "Continuity",
                             measure=="LCMC" ~ "LCMC",
                             measure=="Qnx" ~ "Co-ranking",
                             measure=="W_n" ~ "MRREs",
                             measure=="Procrustes" ~ "Procrustes")
  ylabel <- ifelse(percentage, paste("Percentage of change in", ylabel, "(%)"), ylabel)
  
  scens <- expand_grid(method, annmethod) 
  ann_table <- purrr::map2(scens$method, scens$annmethod, combineanntable) %>% 
    bind_rows() %>% 
    mutate(method = fct_inorder(method))
    # mutate(method = fct_relevel(method, "annIsomap", "annLLE", "annLaplacianEigenmaps", "annHLLE"))
  
  # xintercept for true NN time of the whole process (`time`) for a vertical line
  nntime <- ann_table %>% 
    filter(par==0) %>% 
    # group_by(method) %>%
    mutate(time=as.numeric(time)) %>% 
    dplyr::select(method, time, measure)

  
  ann_table1 <- ann_table %>% 
    dplyr::select(method, annmethod, par, time, measure) %>% 
    filter(case_when(method==nntime$method[1] ~ time <= nntime$time[1],
                     method==nntime$method[2] ~ time <= nntime$time[2],
                     method==nntime$method[3] ~ time <= nntime$time[3],
                     method==nntime$method[4] ~ time <= nntime$time[4],
    )) %>%
    rename(value = measure) %>%
    group_by(method) %>%
    mutate(value = ifelse(rm.outliers & annmethod == "hnsw", filter_lims(value), value)) %>% # remove outliers for HNSW
    group_by(method) %>% 
    mutate(value0 = case_when(method==nntime$method[1] ~ as.numeric(nntime[1,measure]),
                              method==nntime$method[2] ~ as.numeric(nntime[2,measure]),
                              method==nntime$method[3] ~ as.numeric(nntime[3,measure]),
                              method==nntime$method[4] ~ as.numeric(nntime[4,measure]),
                              ),
           percent = (value-value0)/value0*100) %>% 
    rename(value1 = ifelse(percentage, "percent", "value"))
    
  
  p_ml <- ann_table1 %>% 
    # group_by(method) %>%
    # rename(value = measure) %>%
    # mutate(value = ifelse(rm.outliers & annmethod == "hnsw", filter_lims(value), value)) %>% # remove outliers for HNSW
    # mutate(value1 = ifelse(percentage, percent, value)) %>%
    ggplot(aes(x = time, y = value1, group = method)) +
    geom_point(aes(color = annmethod, group = method, shape = annmethod), size = 3) + # alpha = par, removed from aes()
    facet_wrap(~method, scales="free", labeller = as_labeller(c(`annIsomap` = "Isomap",
                                                                `annLLE` = "LLE",
                                                                `annLaplacianEigenmaps` = "LaplacianEigenmaps",
                                                                `annHLLE` = "HLLE"))) + 
    geom_vline(data = nntime, aes(xintercept = time), linetype = 2) + # add KNN vertical line
    guides(color=guide_legend(keywidth = 2, keyheight = 1)) + 
    theme(legend.position = "bottom") +
    scale_color_hue(labels = c("Annoy", "HNSW", "k-d trees")) + 
    scale_shape_discrete(labels = c("Annoy", "HNSW", "k-d trees")) + 
    labs(x = "Computation time (second)", y = ylabel, color = "ANN algorithms", linetype = "ANN algorithms", shape = "ANN algorithms") # alpha = "ANN parameter"
  
  # parcolor <- colorspace::scale_color_continuous_sequential(
  #   palette = "viridis",
  #   breaks = c(0, 50, 100, 150, 200),
  #   # labels=c("00:00", "06:00", "12:00", "18:00", "24:00"),
  #   name="ANN parameters",
  #   guide=guide_colorbar(barheight = 10))
  # p_ml <- p_ml + parcolor
  
  if(keep.smooth){
    p_ml <- p_ml + 
      geom_smooth(aes(color = annmethod, group = annmethod, linetype = annmethod), size = 0.7, method = "loess", se = TRUE, span = 0.2, method.args = list(degree=1))
  }
  
  return(p_ml)
}



measure_percent <- function(x, xnn){
  (xnn-x) / xnn * 100
}


# Measure plots to compare three ANN methods, for each ML
measurefacets <- function(method = c("annIsomap", "annLaplacianEigenmaps", "annHLLE", "annLLE"),
                          annmethod = c("kdtree", "annoy", "hnsw"),
                          measure = c(M_T, M_C, LCMC, Qnx, W_n, W_nu, Procrustes, Qlocal, Qglobal, Rnx, mean_Rnx, auc_lnK_Rnx),
                          rm.outliers = FALSE,
                          keep.points = TRUE,
                          keep.smooth = FALSE,
                          percentage = FALSE){ 
  # Plot seven manifold learning quality measure against computation time for three ANN methods, k-d trees, Annoy and HNSW
  # Input: manifold learning method and ANN method to be compared in one plot, whether to remove the outliers for HNSW
  # Output: a facetting ggplots comparing 7 measure
  # Example: measurefacets(method = "annIsomap", annmethod = c("kdtree", "annoy", "hnsw"), measure = c(M_T, M_C, W_n, LCMC, Qnx, Procrustes), rm.outliers = FALSE, keep.points = TRUE, keep.smooth = FALSE, percentage = F)
  # require(rlang)
  method <- match.arg(method)
  if(is.null(annmethod)) annmethod <- c("kdtree", "annoy", "hnsw")
  
  ann_table <- purrr::map2(method, annmethod, combineanntable) %>% 
    dplyr::bind_rows() 
  
  # xintercept for true NN time of the whole process (`time`) for a vertical line
  nntime <- ann_table %>% 
    filter(par==0) %>% 
    dplyr::select(time) %>%
    as.numeric()
  
  # nnmeasure <- ann_table %>% 
  #   filter(par==0) %>% 
  #   dplyr::select(-c(annmethod, par, qps, knn_time, dijkstra_time, eigen_time, recall)) 
  
  # dat <- ann_table %>% 
  #   mutate_at(10:ncol(ann_table), list(~ (. - first(.))/first(.)))
  
  # ann_table1 <- ann_table %>% 
  #   dplyr::select(-c(annmethod, qps, knn_time, dijkstra_time, eigen_time, recall)) %>%
  #   filter(time <= nntime$time + 10) %>%
  #   rowwise() %>% 
  #   
  #   dplyr::select(method, annmethod, time, {{measure}}) %>% # {{measure}}
  
  # a faceting plot with 6 subplots, each representing one measure
  ann_table1 <- ann_table %>% 
    filter(time <= nntime + 10) %>%
    # mutate_at(10:ncol(ann_table), list(~ (first(.) - .)/first(.))) %>%
    # dplyr::select(-c(annmethod, qps, knn_time, dijkstra_time, eigen_time, recall)) 
    dplyr::select(method, annmethod, time, {{measure}}) #%>% # {{measure}}
  # remove outliers for HNSW
  # rowwise() %>%
  # mutate(percent = measure_percent(value, nntime[,measures])) %>% 
  # # rename(value1 = ifelse(percentage==F, starts_with("percent"), "value")) %>% 
  # ungroup() 
  
  if(!percentage){
    ann_table1 <- ann_table %>% 
      filter(time <= nntime + 10) %>% 
      dplyr::select(method, annmethod, time, {{measure}}) 
  } else {
    ann_table1 <- ann_table %>% 
      filter(time <= nntime + 10) %>%
      mutate_at(10:ncol(ann_table), list(~ (. - first(.))/first(.)*100)) %>%
      dplyr::select(method, annmethod, time, {{measure}}) 
  }
  
  
  p <- ann_table1 %>% 
    pivot_longer(cols = !c(method, annmethod, time),
                 names_to = "measures",
                 values_to = "value") %>% 
    mutate(measures = fct_inorder(measures)) %>% 
    group_by(measures) %>%
    mutate(value = ifelse(rm.outliers & annmethod == "hnsw", filter_lims(value, low=1, up=4), value)) %>% 
    ggplot(aes(x = time, y = value, group = measures)) +
    # geom_point(aes(color = annmethod, group = measures, shape = annmethod)) + 
    # geom_smooth(aes(color = annmethod, group = annmethod, linetype = annmethod), size = 0.7, method = "loess", se = TRUE, span = 0.2, method.args = list(degree=1)) + #method.args = list(family = "symmetric")
    geom_vline(xintercept = nntime, linetype = 2) + 
    facet_wrap(~measures, scales = "free", labeller = as_labeller(c(`M_T` = "Trustworthiness",
                                                                    `M_C` = "Continuity",
                                                                    `LCMC` = "LCMC",
                                                                    `Qnx` = "Co-ranking",
                                                                    `W_n` = "MRREs",
                                                                    `Procrustes` = "Procrustes"))) +
    guides(color=guide_legend(keywidth = 3, keyheight = 1)) + 
    theme(legend.position = "bottom") +
    scale_color_hue(labels = c("Annoy", "HNSW", "k-d trees")) + 
    scale_shape_discrete(labels = c("Annoy", "HNSW", "k-d trees")) + 
    labs(x = "Computation time (second)", y = ifelse(!percentage, "Quality measures", "Percentage of change in quality measures (%)"), color = "ANN algorithms", linetype = "ANN algorithms", shape = "ANN algorithms")
  
  if(keep.points){
    p <- p + geom_point(aes(color = annmethod, group = measures, shape = annmethod), size = 2) 
  }
  
  if(keep.smooth){
    p <- p + geom_smooth(aes(color = annmethod, group = annmethod, linetype = annmethod), size = 0.7, method = "loess", se = TRUE, span = 0.2, method.args = list(degree=1)) #method.args = list(family = "symmetric")
  }
  
  return(p)
}




# # Measure plots to compare three ANN methods, for each ML
# measurePlots <- function(method = c("annIsomap", "annLaplacianEigenmaps", "annHLLE", "annLLE"), 
#                          measure = c(M_T, M_C, LCMC, Qnx, W_n, W_nu, Procrustes, Qlocal, Qglobal, Rnx, mean_Rnx, auc_lnK_Rnx)){ # c("M_T", "M_C", "LCMC", "Qnx", "W_n", "W_nu", "Procrustes", "Qlocal", "Qglobal", "Rnx", "mean_Rnx", "auc_lnK_Rnx")
#   # Plot seven manifold learning quality measure against computation time for three ANN methods, k-d trees, Annoy and HNSW
#   # Input: manifold learning method to be compared in one plot
#   # Output: three ggplots comparing 7 measure, 1 measure specified in the argument `measure`, recall rate
#   # Example: measurePlots(method = "annIsomap", measure = LCMC)
#   # require(rlang)
#   method <- match.arg(method)
#   annmethod <- c("kdtree", "annoy", "hnsw")
#   
#   ann_table <- c()
#   for (i in 1:3) {
#     ann_table <- rbind(ann_table, combineanntable(method, annmethod[i]))
#   }
#   
#   # a faceting plot with 7 subplots, each representing one measure
#   p <- ann_table %>% 
#     # select(-c(par, qps, knn_time, dijkstra_time, eigen_time, recall, K)) %>% 
#     dplyr::select(method, annmethod, time, M_T:Procrustes) %>% 
#     pivot_longer(cols = !c(method, annmethod, time),
#                  names_to = "measures",
#                  values_to = "value") %>% 
#     mutate(measures = fct_inorder(measures)) %>% 
#     ggplot(aes(x = time, y = value, group = measures)) +
#       geom_point(aes(color = annmethod, group = measures, shape = annmethod)) + 
#       geom_smooth(aes(color = annmethod, group = annmethod, linetype = annmethod), size = 0.7, method = "loess", se = TRUE) + #method.args = list(family = "symmetric")
#       facet_wrap(~measures, scales = "free") +
#       guides(color=guide_legend(keywidth = 3, keyheight = 1)) + 
#       theme(legend.position = "bottom") +
#       labs(x = "Computation time (second)", y = "Quality measures", color = "ANN algorithms", linetype = "ANN algorithms", shape = "ANN algorithms")
#   
#   # only one measure
#   p_measure <- ann_table %>% 
#     dplyr::select(method, annmethod, time, {{measure}}) %>% 
#     pivot_longer(cols = !c(method, annmethod, time),
#                  names_to = "measures",
#                  values_to = "value") %>% 
#     mutate(measures = fct_inorder(measures)) %>% 
#     ggplot(aes(x = time, y = value, group = measures)) +
#     geom_point(aes(color = annmethod, group = measures, shape = annmethod)) + 
#     geom_smooth(aes(color = annmethod, group = annmethod, linetype = annmethod), size = 0.7, method = "loess", se = TRUE) + 
#     guides(color=guide_legend(keywidth = 3, keyheight = 1)) + 
#     theme(legend.position = "right") +
#     labs(x = "Computation time (second)", y = rlang::enquo(measure), color = "ANN algorithms", linetype = "ANN algorithms", shape = "ANN algorithms")
#   
#   # compare recall rate
#   p_recall <- ann_table %>% 
#     dplyr::select(method, annmethod, knn_time, recall) %>% 
#     pivot_longer(cols = !c(method, annmethod, knn_time),
#                  names_to = "measures",
#                  values_to = "value") %>% 
#     mutate(measures = fct_inorder(measures)) %>% 
#     ggplot(aes(x = knn_time, y = value, group = measures)) +
#     geom_point(aes(color = annmethod, group = measures, shape = annmethod)) + 
#     geom_line(aes(color = annmethod, group = annmethod, linetype = annmethod), size = 0.7) + 
#     guides(color=guide_legend(keywidth = 3, keyheight = 1)) + 
#     theme(legend.position = "right") +
#     labs(x = "Nearest neighbor searching time (second)", y = "Recall rate", color = "ANN algorithms", linetype = "ANN algorithms", shape = "ANN algorithms")
#   
#   return(list(p=p, p_measure=p_measure, p_recall=p_recall))
# }




# # Compare ANN methods in terms of recall rate for each ML method 
# comparePlots <- function(method = c("annIsomap", "annLaplacianEigenmaps", "annHLLE", "annLLE")){
#   # Plot manifold learning recall rate against computation time for three ANN methods, k-d trees, Annoy and HNSW
#   # Input: manifold learning method to be compared in one plot
#   # Output: one ggplot p_comp_recall to compare three ANN methods
#   # Example: comparePlots(method = "annIsomap")
#   method <- match.arg(method)
#   annmethod <- c("kdtree", "annoy", "hnsw")
#   
#   ann_table <- list()
#   for (i in 1:3) {
#     ann_table[[i]] <- combineanntable(method, annmethod[i])    
#   }
#   
#   # p_comp_MT <- ggplot(ann_table[[i-2]], aes(x = time, y = M_T)) +
#   #   geom_point(data = ann_table[[i-2]], size = 0.7, linetype = "longdash", aes(color = "kdtree")) +
#   #   geom_point(data = ann_table[[i-1]], size = 0.7, linetype = "solid", aes(color = "annoy")) +
#   #   geom_point(data = ann_table[[i]], size = 0.7, linetype = "twodash", aes(color = "hnsw")) +
#   #   labs(x = "Computation time (second)", y = "Trustworthiness") + # title = "High and to the left is better"
#   #   scale_color_manual(name = "ANN algorithms",
#   #                      values = c(annoy = "#F8766D", hnsw = "#7CAE00", kdtree = "#00BFC4")) +
#   #   geom_smooth(data = ann_table[[i-2]], size = 0.7, linetype = "longdash", aes(x=time, y=M_T, color = "kdtree"), method = "loess", se = TRUE) +
#   #   geom_smooth(data = ann_table[[i-1]], size = 0.7, linetype = "solid", aes(x=time, y=M_T, color = "annoy"), method = "loess", se = TRUE) +
#   #   geom_smooth(data = ann_table[[i]], size = 0.7, linetype = "twodash", aes(x=time, y=M_T, color = "hnsw"), method = "loess", se = TRUE) #+
#   #   # guides(shape = guide_legend(override.aes = list(size = 4)))
#   # 
#   # 
#   # p_comp_Wn <- ggplot(ann_table[[i-2]], aes(x = time, y = W_n)) +
#   #   geom_point(data = ann_table[[i-2]], size = 0.7, linetype = "longdash", aes(color = "kdtree")) +
#   #   geom_point(data = ann_table[[i-1]], size = 0.7, linetype = "solid", aes(color = "annoy")) +
#   #   geom_point(data = ann_table[[i]], size = 0.7, linetype = "twodash", aes(color = "hnsw")) +
#   #   scale_color_manual(name = "ANN algorithms",
#   #                      values = c(annoy = "#F8766D", hnsw = "#7CAE00", kdtree = "#00BFC4")) +
#   #   geom_smooth(data = ann_table[[i-2]], size = 0.7, linetype = "longdash", aes(x=time, y=W_n, color = "kdtree"), method = "loess", se = TRUE) +
#   #   geom_smooth(data = ann_table[[i-1]], size = 0.7, linetype = "solid", aes(x=time, y=W_n, color = "annoy"), method = "loess", se = TRUE) +
#   #   geom_smooth(data = ann_table[[i]], size = 0.7, linetype = "twodash", aes(x=time, y=W_n, color = "hnsw"), method = "loess", se = TRUE) +
#   #   guides(guide_legend(keywidth = 3, keyheight = 1)) +
#   #   labs(x = "Computation time (second)", y = "Mean Relative Rank Error")
#   
#   
#   p_comp_Recall <- ggplot(ann_table[[i-2]], aes(x = knn_time, y = recall)) +
#     geom_point(data = ann_table[[i-2]], aes(color = "kdtree", shape = "kdtree")) +
#     geom_point(data = ann_table[[i-1]], aes(color = "annoy", shape = "annoy")) +
#     geom_point(data = ann_table[[i]], aes(color = "hnsw", shape = "hnsw")) +
#     geom_line(data = ann_table[[i-2]], size = 0.7, aes(color = "kdtree", linetype = "kdtree")) +
#     geom_line(data = ann_table[[i-1]], size = 0.7, aes(color = "annoy", linetype = "annoy")) +
#     geom_line(data = ann_table[[i]], size = 0.7, aes(color = "hnsw", linetype = "hnsw")) +
#     theme(legend.title = element_text(), legend.position = "right") + 
#     scale_linetype_manual(values=c(annoy = "solid", hnsw = "twodash", kdtree = "longdash")) +
#     scale_color_manual(values = c(annoy = hue_pal()(3)[1], hnsw = hue_pal()(3)[2], kdtree = hue_pal()(3)[3])) + 
#     guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) + 
#     labs(color = "ANN algorithms", linetype = "ANN algorithms", shape = "ANN algorithms", 
#          x = "Nearest neighbor searching time (second)", y = "Recall rate") 
#   
#   return(p_comp_Recall)
# }





# The following function can load all anntable .rds files and update the variables for each simulation scenario (1-1004). 
# Input: i in 1:1004
# Output: ann_table_i, p_recall_i, p_MT_i, p_time_i
# loadanntable <- function(scen = 1){
#   ml <- c("annIsomap", "annLaplacianEigenmaps", "annHLLE", "annLLE")#[-3]
#   epsilon <- seq(0, 10, 0.1) # kdtree
#   ntrees <- seq(2, 100, 2) #2:100, # Annoy
#   nlinks <- seq(2, 200, 2) #2:200 # HNSW
#   simtable <- expand_grid(ml = ml, 
#                           rbind(
#                             expand_grid(annmethod = "kdtree", param = "epsilon", value = epsilon),
#                             expand_grid(annmethod = "annoy", param = "ntrees", value = ntrees),
#                             expand_grid(annmethod = "hnsw", param = "nlinks", value = nlinks) )) 
#   # simtable
#   # (101+50+100) * 4 = 1004 - 200 = 804
#   if(!(scen %in% 1:nrow(simtable))) stop("The scenario number must be an interger between 1 and ", nrow(simtable), ".")
#   
#   (simj <<- simtable[scen,]) # Extract row of table
#   method <<- simj$ml 
#   annmethod <<- simj$annmethod %>% as.character() 
#   param_name <<- simj$param %>% as.character()
#   par <<- simj$value
#   
#   switch(dataname,
#          "MNISTtest" = {filename <<- paste0(folder, "anntable_", method, "_", annmethod, "_", par, "_MNISTtest_", N,".rds") },
#          "electricity" = {filename <<- paste0(folder, "anntable_", method, "_", annmethod, "_", par, "_electricity_", nid, "id", ntow, "tow.rds") }
#   )
#   ann_table <<- readRDS(here::here(filename))
# }




# Plotting
# simtable <- expand_grid(method = c("annIsomap", "annLaplacianEigenmaps", "annHLLE", "annLLE"),
#                         annmethod = c("kdtree", "annoy", "hnsw")) 
# simtable
# 
# wholetable <- simtable %>% 
#   group_by(method, annmethod) %>%
#   nest() %>%
#   # select(-method, -annmethod) %>% 
#   mutate(ann_table = map(data, ~ combineanntable(method, annmethod))) %>% 
#   unnest(ann_table, names_repair = tidyr_legacy) %>% 
#   ungroup() %>%
#   select(-data, -method1, -annmethod1) 
# wholetable
# # 1004*22
# # saveRDS(wholetable, file = paste0(folder, dataname, "10000_mlann_table.rds"))
# 
# # mapply(combineanntable, simtable$method, simtable$annmethod)
# 
# wholetable %>% 
#   filter(method == "annHLLE", annmethod == "hnsw") -> ann_table
# ann_table %>%
#   anntablePlots()
# 
# comparePlots("annIsomap")
# comparePlots("annHLLE")
# map(simtable$method, comparePlots)
# 
# 
# wholetable <- simtable %>% 
#   group_by(method, annmethod) %>%
#   nest() %>%
#   mutate(ann_table = map(data, ~ combineanntable(method, annmethod))) 
# # %>% 
# #   unnest(ann_table, names_repair = tidyr_legacy) %>% 
# #   ungroup() %>%
# #   select(-data, -method1, -annmethod1)
# wholetable
# 
# ann_table <- paste0("method", "_", "annmethod")
# for (i in 1:nrow(simtable)) {
#   tmp <- wholetable[i,] %>% 
#     unnest(ann_table, names_repair = tidyr_legacy) %>% 
#     ungroup() %>%
#     select(-data, -method1, -annmethod1)
#   assign(ann_table[i], tmp)       
# }
# ann_table[[i]]
