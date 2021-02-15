#!/usr/bin/env Rscript

### Packages you will need
list.of.packages <- c(
  "ash",
  "coRanking",
  "crop",
  "data.table",
  "devtools",
  "dtplyr",
  "dimRed",
  "doParallel",
  "foreach",
  "furrr",
  "future",
  "ggplot2",
  "gridExtra",
  "GGally",
  "geigen",
  "Jmisc", 
  "MASS",
  "HDoutliers",
  "hdrcde",
  "ks",
  "kableExtra",
  "knitr",
  "Matrix", 
  "microbenchmark",
  "parallel",
  "philentropy",
  "purrr",
  "RColorBrewer",
  "readr",
  "reshape2",
  "tibble",
  "tictoc",
  "tidyr",
  "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                  installed.packages()[,"Package"])]
if(length(new.packages) > 0) 
  install.packages(new.packages, repos='https://cloud.r-project.org', dependencies = TRUE)

#update.packages(checkBuilt=TRUE, ask=FALSE, repos='https://cloud.r-project.org')
