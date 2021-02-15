## input data(matrix or data frame) return knn graph implements
## "smart" choices on RANN::nn2 parameters we ignore radius search
## "eps=0" will compute pairwise distances to find nearest neighbors
## ANN TODO: find out a good limit to switch from kd to bd trees COMMENT:
## bd trees are buggy, they dont work if there are duplicated data
## points and checking would neutralize the performance gain, so bd
## trees are not really usable.

makeKNNgraph <- function (x, k, eps = 0, annmethod = c("kdtree", "annoy", "hnsw"), nt = 50, search.k = 500, nlinks = 16, ef.construction = 200, diag = FALSE, distance = c("euclidean", "manhattan")){
  ## requireNamespace("RANN")
  ## requireNamespace("igraph")
  require(BiocNeighbors)
  require(RANN)
  require(RANN.L1)
  require(igraph)
  annmethod <- match.arg(annmethod)
  distance <- match.arg(distance)
  
  
  ## consts
  INF_VAL <- 1.340781e+15
  NA_IDX  <- 0
  BDKD_LIM <- 1000000                 #todo: figure out a good value here
  
  ## select parameters
  M <- nrow(x)
  
  # if (eps == 0) {
  #   # find exact nearest neighbors
  #   ## compute pairwise distances matrix
  #   d <- t(rowSums(x^2))
  #   d <- d[rep(1, each = M), ]
  #   d <- d + t(d) - 2 * x %*% t(x)
  #   diag(d) <- 0
  #   d <- sqrt(d)
  #   
  #   lst <-  apply(d, 2, sort, index.return = TRUE)
  #   nn.lst <- do.call(cbind, Map(cbind, 
  #                                # colInd = seq_along(lst),
  #                                lapply(lst, function(x) do.call(cbind, x))))[1:(k+1), ]
  #   odd <- seq(1, ncol(nn.lst), 2)   # index
  #   nn.dists <- t(nn.lst[, odd])
  #   nn.idx <- t(nn.lst[, -odd])
  #   nn2res <- list(nn.idx=nn.idx, nn.dists=nn.dists)
  # } else {
    # treetype <- "kd"                # if (M < BDKD_LIM) "kd" else "bd"
    # see:
    # https://github.com/jefferis/RANN/issues/19
    # searchtype <- if (eps == 0) "standard" else "priority"
    # searchtype <- "priority"
    
    ## RANN::nn2 returns the points in data with respect to query
    ## e.g. the rows in the output are the points in query and the
    ## columns the points in data.
    # nn2res <- RANN::nn2(data = x, query = x, k = k + 1, treetype = treetype,
    #                    searchtype = searchtype, eps = eps)
  # }
  
  switch(annmethod,
         "kdtree" = {
           treetype <- "kd"                
           searchtype <- "priority"
           nn2res <- dplyr::case_when(distance=="euclidean" ~ RANN::nn2(data = x, query = x, k = k + 1, treetype = treetype, searchtype = searchtype, eps = eps),
                                      distance=="manhattan" ~ RANN.L1::nn2(data = x, query = x, k = k + 1, treetype = treetype, searchtype = searchtype, eps = eps),
           )
           names(nn2res) <- c("nn.idx", "nn.dists")
           
         },
         "annoy"   = {
           nn2res <- dplyr::case_when(distance=="euclidean" ~ BiocNeighbors::queryKNN(X = x, query = x, k = k + 1, BNPARAM = AnnoyParam(ntrees = nt, search.mult = search.k, distance = "Euclidean")),
                                      distance=="manhattan" ~ BiocNeighbors::queryKNN(X = x, query = x, k = k + 1, BNPARAM = AnnoyParam(ntrees = nt, search.mult = search.k, distance = "Manhattan")),
           )
           names(nn2res) <- c("nn.idx", "nn.dists")
         }, 
         "hnsw"    = {
           nn2res <- dplyr::case_when(distance=="euclidean" ~ BiocNeighbors::queryKNN(X = x, query = x, k = k + 1, BNPARAM = HnswParam(nlinks = nlinks, ef.construction = ef.construction, distance = "Euclidean")),
                                      distance=="manhattan" ~ BiocNeighbors::queryKNN(X = x, query = x, k = k + 1, BNPARAM = HnswParam(nlinks = nlinks, ef.construction = ef.construction, distance = "Manhattan")),
           )
           names(nn2res) <- c("nn.idx", "nn.dists")
         }
         )
  
  ## create graph: the first ny nodes will be y, the last nx nodes
  ## will be x, if x != y
  ## it is not really pretty to create a
  ## directed graph first and then make it undirected.
  g <- igraph::make_empty_graph(M, directed = TRUE)
  g[from = if (diag) rep(seq_len(M), times = k + 1) else      rep(seq_len(M), times = k),
    to   = if (diag) as.vector(nn2res$nn.idx)  else      as.vector(nn2res$nn.idx[, -1]),
    attr = "weight"] <-
    if (diag)  as.vector(nn2res$nn.dists) else as.vector(nn2res$nn.dists[, -1])
  
  return(list(g = igraph::as.undirected(g, mode = "collapse", edge.attr.comb = "first"),
              nn2res = nn2res))
}



## Computes the pairwise Euclidean distances between all data samples
##
## input:
##       data: NxD matrix (N samples, D features)
## output:
##	NxN matrix with the pairwise Euclidean distances
# RDRTools::pairwiseDistances()
pairwiseDistances = function(data){
  num_samples = nrow(data)

  d = t(rowSums(data^2))
  d = d[rep(1, each = num_samples),]
  d = d + t(d) - 2*data%*%t(data)
  diag(d) = 0
  d = sqrt(d)

  return(d)
}
