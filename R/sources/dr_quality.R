##---------------------------------------------------
# All quality measures in a function
##---------------------------------------------------
dr_quality <- function(X, Y, K = 20, nn.idx, treetype = "kd", searchtype = "standard"){
  # Output: a list of Q, the co-ranking martrix, and the quality data.frame containing 8 quality measures
  # add an argument nn.idx for the exact nearest neighbor index
  
# dr_quality <- function(e, treetype = "kd", searchtype = "standard"){
  # e is the embedding dimRedResult
  # X <- as.matrix(e@org.data)
  # Y <- e@data@data
  # pars <- e@pars
  # if (!is.list(pars))         stop("pars must be a list consists of knn and epsilon")
  if (dim(X)[1] != dim(Y)[1]) stop("X and Y must have the same rows!")
  
  
  ## coRanking matrix Q
  Q <- coRanking::coranking(Y, X)
  N <- nrow(Q) + 1
  # K <- pars$knn
  
  # Trustworthiness & Continuity
  M_T <- coRanking:::cm.M_T(Q, K = K)
  M_C <- coRanking:::cm.M_C(Q, K = K)
  
  # Mean Relative Rank Errors
  W_n <- coRanking:::cm.W_n(Q, K = K)
  W_nu <- coRanking:::cm.W_nu(Q, K = K)
  
  # Local Continuity Meta-Criterion
  lcmc <- coRanking::LCMC(Q, K = as.integer(K))
  # Optimize K but very slow: coRanking:::cm.K_max(Q)$K_max
  
  # Co-ranking Matrix measure
  Qnx <- sum(Q[1:K, 1:K]) / K / N
  # Qnx <- dimRed::Q_NX(e)
  # Qlocal <- dimRed::quality(e, "Q_local")
  # Qglobal <- dimRed::quality(e, "Q_global")
  Rnx <- ((N - 1) * Qnx - K) / (N - 1 - K)
  # Rnx <- dimRed::R_NX(e)
  # mean_Rnx <- dimRed::quality(e, "mean_R_NX")
  # auc_lnK_Rnx <- dimRed::quality(e, "AUC_lnK_R_NX")
  # suppressMessages(procrustes <- proc.measure(X, Y, K, nn.idx, treetype = treetype, searchtype = searchtype)$R)
  procrustes <- proc.measure(X, Y, K, nn.idx, treetype = treetype, searchtype = searchtype)$R_N # standerdized procrustes measure R_N, unstanderdized $R
  # K_optim <- as.numeric(names(Qglobal))
  
  return(list(Q = Q, 
              quality=data.frame(
                # K = K_optim, 
                                 M_T=M_T, M_C=M_C, LCMC=lcmc, Qnx=Qnx, 
                                 W_n=W_n, W_nu=W_nu, Procrustes=procrustes,
                                 # Qlocal=Qlocal, Qglobal=Qglobal,
                                 Rnx=Rnx, 
                                 # mean_Rnx=mean_Rnx, auc_lnK_Rnx=auc_lnK_Rnx, 
                                 row.names = ""
                                 )))
}


##------------------------------------------------------
## Procrustes measure R(X, Y)
##------------------------------------------------------
# R(X, Y) = 1/n \sum_{i=1}{n} {G(X_i, Y_i)},
# where G(X_i, Y_i) is the Procrustes statistics of X_i and Y_i.
# X: n*q, n samples of q dimension
# Y: n*d, d-dimensional embedding of X, d << q
# X_i: neighborhood of x_i (i = 1, ..., n)
# Y_i: embedding of X_i
# K: number of neasrest neighbors
# nn.idx: exact nearest neighbor index
proc.measure <- function(X, Y, K, nn.idx, treetype = "kd", searchtype = "standard"){
# proc.measure <- function(e, treetype = "kd", searchtype = "standard"){
  # e is the embedding dimRedResult
  # X <- as.matrix(e@org.data)
  # Y <- e@data@data
  # K <- pars$knn
  # neighborhood
  ## each row consists of c(row index i, neighbors index) 
  # nnidx <- RANN::nn2(data = X, query = X, k = pars$knn + 1, eps = 0, 
  #                    treetype = "kd", searchtype = "standard")$nn.idx[, -1]
  # calculate R(X,Y)
  G <- list()
  R_XY <- c()
  R_N_XY <- c()
  for(i in 1:nrow(X)){
    Xi <- X[nn.idx[i, ], , drop = FALSE]
    Yi <- Y[nn.idx[i, ], , drop = FALSE]
    # calculate G(X_i, Y_i)
    G[[i]] <- invisible(vegan::procrustes(Xi, Yi))
    R_XY[i] <- G[[i]]$ss
    # normalized R_N_XY
    Xi <- scale(Xi, scale = FALSE)
    R_N_XY[i] <- R_XY[i]/sum(Xi^2)
  }
  R <- sum(R_XY)
  R_N <- sum(R_N_XY) / nrow(X)
  # message("The Procrustes measure R(X,Y) is ", R)
  result <- list(G_XY = G, R_i = R_XY, R = R, R_N = R_N)
  return(result)
}

# # proc.measure function test
# library(dimRed)
# sr <- loadDataSet("Swiss Roll")
# sr_isomap <- embed(sr, .method = "annIsomap")
# X <- sr_isomap@org.data
# Y <- sr_isomap@data@data
# pars <- list(knn = 10)
# nn.idx <- RANN::nn2(data = X, query = X, k = pars$knn + 1, eps = 0,
#                    treetype = "kd", searchtype = "standard")$nn.idx[, -1]
# PM <- proc.measure(X, Y, K = sr_isomap@pars$knn, nn.idx)
# plot(PM$R_i, type = "h", xlab = "Index of neighborhood k", ylab = "Procrustes statistics G(X,Y)")
# # find the optim K
# which.min(PM$R_i)
# # [1] 16
# PM1 <- proc.measure(X, Y, K = which.min(PM$R_i), nn.idx)
# PM1
# 
# # Example
# dr_quality(X, Y, K, nn.idx)
