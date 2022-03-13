#' Find approximate nearest neighbors using different methods
#'
#' @param x A numeric data matrix where rows are points and columns are dimensions. 
#' @param knn 
#' @param annmethod 
#' @param eps 
#' @param radius 
#' @param nt 
#' @param nlinks 
#' @param ef.construction 
#' @param ef.search 
#' @param distance 
#' @param treetype 
#' @param searchtype 
#' @param ...
#'
#' @return A list is returned containing \code{nn.idx}, an integer matrix of neighbor identities; and \code{nn.dists}, a numeric matrix of distances to those neighbors.
#' @export
#'
#' @examples
find_ann <- function(x, 
                     knn = 50,
                     annmethod = "kdtree", 
                     eps = 0, 
                     radius = 1,
                     nt = 50, 
                     nlinks = 16, 
                     ef.construction = 200,
                     ef.search = 10,
                     distance = c("euclidean", "manhattan"),
                     treetype = c("kd", "bd"),
                     searchtype = c("standard", "priority", "radius"),
                     get_geod = FALSE,
                     ...) {
  
  if(knn > nrow(x) - 1) stop("Number of nearest neighbors should be less than `nrow(x)-1`!")
  annmethod <- match.arg(annmethod, c("brute-force", "kdtree", "annoy", "hnsw"))
  distance <- match.arg(distance)
  message(Sys.time(), ": Finding ANN using ", annmethod, sep = "")
  
  base::switch(annmethod,
               "brute-force" = {
                 d <- as.matrix(dist(x, method = distance))
                 nn2res <- list(nn.idx = t(apply(d, 1, order))[, 1:(knn+1)],
                                nn.dists = t(apply(d, 1, sort))[, 1:(knn+1)]
                                )
                 dimnames(nn2res$nn.idx) <- NULL
                 dimnames(nn2res$nn.dists) <- NULL
               },
               
               "kdtree" = {
                 if(is.null(treetype)) treetype <- "kd"                
                 if(is.null(searchtype)) searchtype <- "priority"
                 nn2res <- base::switch(distance,
                                       "euclidean" = {RANN::nn2(data = x, query = x, k = knn + 1, treetype = treetype, searchtype = searchtype, eps = eps, radius = radius)},
                                       "manhattan" = {RANN.L1::nn2(data = x, query = x, k = knn + 1, treetype = treetype, searchtype = searchtype, eps = eps, radius = radius)})
               },
               
               "annoy"   = {
                 nn2res <- base::switch(distance,
                                       "euclidean" = {BiocNeighbors::queryKNN(X = x, query = x, k = knn + 1, BNPARAM = BiocNeighbors::AnnoyParam(ntrees = nt, distance = "Euclidean"), ...)},
                                       "manhattan" = {BiocNeighbors::queryKNN(X = x, query = x, k = knn + 1, BNPARAM = BiocNeighbors::AnnoyParam(ntrees = nt, distance = "Manhattan"), ...)})
               }, 
               
               "hnsw"    = {
                 nn2res <- base::switch(distance,
                                       "euclidean" = {BiocNeighbors::queryKNN(X = x, query = x, k = knn + 1, BNPARAM = BiocNeighbors::HnswParam(nlinks = nlinks, ef.construction = ef.construction, ef.search = ef.search, distance = "Euclidean"), ...)},
                                       "manhattan" = {BiocNeighbors::queryKNN(X = x, query = x, k = knn + 1, BNPARAM = BiocNeighbors::HnswParam(nlinks = nlinks, ef.construction = ef.construction, ef.search = ef.search, distance = "Manhattan"), ...)})
               }
  )
  names(nn2res) <- c("nn.idx", "nn.dists")              
  
  return(nn2res)
}
