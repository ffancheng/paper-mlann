annLLE <- setClass("annLLE",
                      contains = "dimRedMethod",
                      prototype = list(
                        stdpars = list(knn = 50,
                                       eps = 0, 
                                       ndim = 2,
                                       get_geod = FALSE,
                                       annmethod = "kdtree", 
                                       nt = 50, 
                                       nlinks = 16, 
                                       ef.construction = 200,
                                       distance = c("euclidean", "manhattan")),
                        fun = function (data, pars,
                                        keep.org.data = TRUE) {
                          # chckpkg("RSpectra")
                          # chckpkg("igraph")
                          # chckpkg("RANN")
                          # chckpkg("microbenchmark")
                          # chckpkg("BiocNeighbors")
                          message(Sys.time(), ": annLLE START")
                          meta <- data@meta
                          orgdata <- if (keep.org.data) data@data else NULL
                          if (is.null(pars$distance)) pars$distance <- "euclidean"
                          if (length(pars$distance) > 1) pars$distance <- pars$distance[1]
                          indata <- data@data
                          
                          if (is.null(pars$eps))      pars$eps <- 0
                          if (is.null(pars$get_geod)) pars$get_geod <- FALSE
                          ## k >= ntow to get connected graph
                          if(pars$knn >= dim(indata)[1]) stop("Number of nearest neighbors greater than number of rows! Please use a smaller knn.")
                          
                          ## geodesic distances
                          message(Sys.time(), ": constructing knn graph")
                          message("Memory used before ANN: ", pryr::mem_used())
                          knn_time <- microbenchmark::microbenchmark(
                            knng <- makeKNNgraph(x = indata, k = pars$knn, eps = pars$eps, 
                                                 annmethod = pars$annmethod, 
                                                 nt = pars$nt, nlinks = pars$nlinks, 
                                                 ef.construction = pars$ef.construction, distance = pars$distance),
                            times = 1,
                            unit = "s"
                          )#$time * 1e-9
                          knn_time <- summary(knn_time)$median
                          if(!igraph::is.connected(knng$g)) stop("The KNN graph is not connected! Please try a larger k.")
                          nnidx <- knng$nn2res$nn.idx
                          message("Memory used after ANN: ", pryr::mem_used())
                          
                          
                          message(Sys.time(), ": reconstructing with linear weights")
                          weighting_time <- microbenchmark::microbenchmark(
                            weights <- get_weights(nns = nnidx, X = indata, m = pars$ndim),
                            times = 1,
                            unit = "s"
                          )#$time * 1e-9
                          weighting_time <- summary(weighting_time)$median
                          
                          message(Sys.time(), ": mapping to embedded coordinates")
                          embedding_time <- microbenchmark::microbenchmark(
                              outdata <- get_coords(weights, 
                                              N=dim(indata)[1], 
                                              n=dim(indata)[2], 
                                              m=pars$ndim),
                            times = 1,
                            unit = "s"
                          )#$time * 1e-9
                          embedding_time <- summary(embedding_time)$median

                          message(Sys.time(), ": DONE")
                        
                          return(new(
                            "dimRedResult",
                            data         = new("dimRedData",
                                               data = outdata,
                                               meta = meta),
                            org.data     = orgdata,
                            running.time = c(knn_time, weighting_time, embedding_time),
                            nn.idx       = nnidx,
                            has.org.data = keep.org.data,
                            has.apply    = TRUE,
                            method       = "annLLE",
                            pars         = pars,
                            other.data   = if (pars$get_geod) list(geod = as.dist(geodist))
                            else               list()
                          ))
                          
                        })
)


###-------------------------------------------------
### Step2: reconstructing with linear weights
###-------------------------------------------------
# ?lle::find_weights()

get_weights <- function(nns=nnidx, X=indata, m=2){
  # get dimensions
  N <- dim(nns)[1] 
  n <- dim(X)[2]
  k <- dim(nns)[2] - 1
  
  # matrix of weights
  # wgts <- matrix(0,N,N)
  wgts <- Matrix::Matrix(0,N,N, sparse=TRUE) 
  
  for (i in (1:N)){
    # no neighbours (find_nn_k(k=0) or eps-neighbourhood)
    if( k==0 ) next
    
    # calculate the differences between xi and its neighbours
    nni <- X[nns[i, ], , drop = TRUE]
    # Z <- nni[-1,] - t(nni[1,]) # wrong matrix calculation!
    Z <- sweep(nni[-1,], 2, t(nni[1,])) 
    Z <- as.matrix(Z)
    
    #gram-matrix
    G <- tcrossprod(Z)
    
    # weights W are solution of GW=1
    # wgts[i,nns[i,-1]] <- solve(G,rep(1,k))
    # using pseudoinverse ginv(A): works better for bad conditioned systems
    wgts[i,nns[i,-1]] <- t(MASS::ginv(G)%*%rep(1,k))
    wgts[i,] <- wgts[i,]/sum(wgts[i,])
  }
  
  return(wgts)
}
# nnidx <- RANN::nn2(data = indata, query = indata, k = knn + 1, eps = 0, 
#                    treetype = "kd", searchtype = "standard")$nn.idx#[, -1]
# (weights <- get_weights(nns = nnidx, X = indata, m = 2))


###-------------------------------------------------
### Step3: mapping to embedded coordinates
###-------------------------------------------------
get_coords <- function(weights, N=dim(indata)[1], n=dim(indata)[2], m=2){
  W <- weights
  dg <- Matrix::Diagonal(N)
  M <- Matrix::crossprod(dg - W)
  # dg <- diag(1, N)
  # M <- t(dg - W) %*% (dg - W)
  
  # calculate the eigenvalues and -vectors of M, M is symmetric
  # e <- eigen(M)
  e <- RSpectra::eigs(M, m + 1, which = "LA", sigma = 0,
                      opts = list(retvec = TRUE))
  message(paste(c("Eigenvalues:", format(e$values)), collapse = " "))
  
  # choose the eigenvectors belonging to the m smallest not-null eigenvalues
  # and re-scale data
  Y <- e$vectors[,1:m] * sqrt(N)
  sqrt(colSums(Y^2))
  colnames(Y) <- paste0("LLE", 1:m)
  
  return(Y)
}
# (Y <- get_coords(weights,
#                  N=dim(indata)[1], 
#                  n=dim(indata)[2], 
#                  m=ndim))

###-------------------------------------------------
### Combined
###-------------------------------------------------
# nn_embed <- function(indata, query = NULL, knn = 10, eps = 0, ndim = 2){
#   N <- dim(indata)[1]
#   ## k >= ntow to get connected graph
#   if(knn >= N) stop("Number of nearest neighbors greater than number of rows! Please use a smaller knn.")
#   # if(knn == N - 1) message("All rows are taken as neighbors.")
#   
#   ## constructing knn graph
#   knng <- makeKNNgraph(x = indata, k = knn, eps = eps)
#   if(!igraph::is.connected(knng)) stop("The KNN graph is not connected! Please try a larger k.")
#   message(Sys.time(), ": Finding nearest neighbors", sep = "")
#   
#   if(is.null(query)) query = indata 
#   nnidx <- RANN::nn2(data = indata, query = query, k = knn + 1, 
#                      treetype = "kd", searchtype = "standard", eps = eps)$nn.idx#[, -1]    
#   
#   
#   # reconstruct using linear weights
#   message(Sys.time(), ": Reconstructing using linear weights", sep = "")
#   weights <- get_weights(nns = nnidx, X = query, m = ndim)
#   
#   # embedded coordinates
#   message(Sys.time(), ": Embedding", sep = "")
#   Y <- get_coords(weights, 
#                   N=dim(query)[1], 
#                   n=dim(query)[2], 
#                   m=ndim)
#   
#   return(Y)
# }

# load('nn_embed_function.rda')
# (Y <- nn_embed(indata, knn = 10, ndim = 2))
# plot(Y)
