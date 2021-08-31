#' Change the list of nearest neighbor index and distances to square distance matrix
#'
#' @param nn A list of nearest neighbor index and distances. 
#' @param sparse Whether a sparse distance matrix should be returned, TRUE by default. 
#'
#' @return A square matrix of distances
#' @export
#'
#' @examples
nn2dist <- function(nn, N = NULL) { # , k = NULL, sparse = TRUE, keep_graph = FALSE, ...
  
  index <- nn$nn.idx
  distances <- nn$nn.dists
  
  if(is.null(N)) {
      N <- nrow(index)
      # k <- ncol(index) - 1
  }
  
  Kn <- matrix(1e+5, N, N)
  for(i in 1:N) {
    Kn[i, index[i,]] <- distances[i, ]
  }
  
  return(Kn)

  # closest <- 
  #   sapply(nn, cbind) %>%
  #   as_tibble() %>% 
  #   mutate(row.idx = rep(1:N, times = k+1)) %>% 
  #   filter(nn.idx!=0) %>% 
  #   mutate(weights = nn.dists) %>%  # the weights
  #   arrange(row.idx)
  # 
  # # Now construct the graph
  # g <- igraph::make_empty_graph(N, directed = TRUE)
  # g[from = closest$row.idx,
  #   to   = closest$nn.idx,
  #   attr = "weight"] <-
  #   closest$weights # k_radius(p,p')
  # if(!igraph::is.connected(g)) stop("Neighborhood graph not connected. Please select a larger k/radius. ")
  # 
  # # Distance matrix of dimension N*N
  # Kn <- igraph::as_adjacency_matrix(g, attr = "weight", sparse = sparse, ...) # dgCMatrix, or g[]
  
}
