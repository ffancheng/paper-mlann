 annIsomap <- setClass("annIsomap",
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
                          
                          message(Sys.time(), ": annIsomap START")
                          meta <- data@meta
                          orgdata <- if (keep.org.data) data@data else NULL
                          indata <- data@data
                          
                          if (is.null(pars$eps))      pars$eps <- 0
                          if (is.null(pars$get_geod)) pars$get_geod <- FALSE
                          if (is.null(pars$distance)) pars$distance <- "euclidean"
                          if (length(pars$distance) > 1) pars$distance <- pars$distance[1]
                          
                          ## geodesic distances
                          message(Sys.time(), ": constructing knn graph")
                          message("Memory used before ANN: ", pryr::mem_used())
                          knn_time <- microbenchmark::microbenchmark(
                            knng <- makeKNNgraph(x = indata, k = pars$knn, eps = pars$eps, 
                                                 annmethod = pars$annmethod, 
                                                 nt = pars$nt, nlinks = pars$nlinks, 
                                                 ef.construction = pars$ef.construction,
                                                 distance = pars$distance),
                            times = 1,
                            unit = "s"
                          )#$time * 1e-9
                          knn_time <- summary(knn_time)$median
                          message("Memory used after ANN: ", pryr::mem_used())
                          
                          
                          message(Sys.time(), ": calculating geodesic distances")
                          dijkstra_time <- microbenchmark::microbenchmark(
                            geodist <- igraph::distances(knng$g, algorithm = "dijkstra"),
                            times = 1,
                            unit = "s"
                          )#$time * 1e-9
                          dijkstra_time <- summary(dijkstra_time)$median
                          
                          message(Sys.time(), ": Classical Scaling")
                          eigen_time <- microbenchmark::microbenchmark(
                            {
                              ## TODO: add regularization
                              k <- geodist ^ 2
                              k <- .Call(stats:::C_DoubleCentre, k)
                              k <- - k / 2
                              ## TODO: explicit symmetrizing
                              ## TODO: return eigenvectors?
                              e <- RSpectra::eigs_sym(k, pars$ndim, which = "LA",
                                                      opts = list(retvec = TRUE))
                              e_values <- e$values
                              e_vectors <- e$vectors
                              neig <- sum(e_values > 0)
                              if (neig < pars$ndim) {
                                warning("Isomap: eigenvalues < 0, returning less dimensions!")
                                e_values <- e_values[seq_len(neig)]
                                e_vectors <- e_vectors[, seq_len(neig), drop = FALSE]
                              }
                              
                              e_vectors <- e_vectors * rep(sqrt(e_values), each = nrow(e_vectors))
                              
                              colnames(e_vectors) <- paste0("ISO", seq_len(neig))
                              },
                            times = 1,
                            unit = "s"
                          )#$time * 1e-9
                          eigen_time <- summary(eigen_time)$median
                          
                          # appl <- function (x) {
                          #   message(Sys.time(), ": L-Isomap embed START")
                          #   appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
                          #   indata    <- if (inherits(x, "dimRedData")) x@data else x
                          #   
                          #   if (ncol(indata) != ncol(data@data))
                          #     stop("x must have the same number of dimensions as the original data")
                          #   
                          #   nindata <- nrow(indata)
                          #   norg <- nrow(orgdata)
                          #   
                          #   message(Sys.time(), ": constructing knn graph")
                          #   lknng <- dimRed:::makeKNNgraph(rbind(indata, orgdata),
                          #                                  k = pars$knn, eps = pars$eps)
                          #   message(Sys.time(), ": calculating geodesic distances")
                          #   lgeodist <- igraph::distances(lknng,
                          #                                 seq_len(nindata),
                          #                                 nindata + seq_len(norg))
                          #   
                          #   message(Sys.time(), ": embedding")
                          #   dammu <- sweep(lgeodist ^ 2, 2, colMeans(geodist ^ 2), "-")
                          #   Lsharp <- sweep(e_vectors, 2, e_values, "/")
                          #   out <- -0.5 * (dammu %*% Lsharp)
                          #   
                          #   message(Sys.time(), ": DONE")
                          #   return(new("dimRedData", data = out, meta = appl.meta))
                          # }
                          
                          return(new(
                            "dimRedResult",
                            data         = new("dimRedData",
                                               data = e_vectors,
                                               meta = meta),
                            org.data     = orgdata,
                            running.time = c(knn_time, dijkstra_time, eigen_time),
                            nn.idx       = knng$nn2res$nn.idx,
                            has.org.data = keep.org.data,
                            # apply        = appl,
                            has.apply    = TRUE,
                            method       = "annIsomap",
                            pars         = pars,
                            other.data   = if (pars$get_geod) list(geod = as.dist(geodist))
                            else               list()
                          ))
                          
                          
                        })
)
