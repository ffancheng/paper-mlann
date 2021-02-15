annLaplacianEigenmaps <- setClass(
  "annLaplacianEigenmaps",
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
                   sparse = "knn",
                   t = Inf, norm = TRUE,
                   distance = c("euclidean", "manhattan")),
    fun = function (data, pars,
                    keep.org.data = TRUE) {
      
      meta <- data@meta
      orgdata <- if (keep.org.data) data@data else NULL
      indata <- data@data
      
      if (is.null(pars$d))     pars$d    <- dist
      if (is.null(pars$knn))   pars$knn  <- 50
      if (is.null(pars$ndim))  pars$ndim <- 2
      if (is.null(pars$t))     pars$t    <- Inf
      if (is.null(pars$norm))  pars$norm <- TRUE
      if (is.null(pars$eps))   pars$eps <- 0
      if (is.null(pars$distance)) pars$distance <- "euclidean"
      if (length(pars$distance) > 1) pars$distance <- pars$distance[1]
      
      message(Sys.time(), ": Creating weight matrix")
      matrix_time <- microbenchmark::microbenchmark(
        W <- if (pars$sparse == "knn") {
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
          
          
          if (is.infinite(pars$t)){
            igraph::set_edge_attr(knng$g, name = "weight", value = 1)
          } else {
            igraph::set_edge_attr(
              knng$g, name = "weight",
              value = exp( -(
                igraph::edge_attr(
                  knng$g, name = "weight"
                ) ^ 2
              ) / pars$t )
            )
          }
          igraph::as_adj(knng$g, sparse = TRUE,
                         attr = "weight", type = "both")
        } else if (pars$sparse == "eps") {
          tmp <- makeEpsSparseMatrix(indata, pars$eps)
          tmp@x <- if (is.infinite(pars$t)) rep(1, length(tmp@i))
          else exp(- (tmp@x ^ 2) / pars$t)
          ## diag(tmp) <- 1
          as(tmp, "dgCMatrix")
        } else {                        # dense case
          tmp <- dist(indata)
          tmp[] <- if (is.infinite(pars$t)) 1
          else exp( -(tmp ^ 2) / pars$t)
          tmp <- as.matrix(tmp)
          diag(tmp) <- 1
          tmp
        },
        times = 1,
        unit = "s"
      )#$time * 1e-9 - knn_time
      matrix_time <- summary(matrix_time)$median - knn_time
      
      eigen_time <- microbenchmark::microbenchmark(
        {
          ## we don't need to test for symmetry, because we know the
          ## matrix is symmetric
          D <- Matrix::Diagonal(x = Matrix::rowSums(W));
          L <- D - W;
          ## for the generalized eigenvalue problem, we do not have a solver
          ## use A u = \lambda B u
          ## Lgen <- Matrix::Diagonal(x = 1 / Matrix::diag(D) ) %*% L
          ## but then we get negative eigenvalues and complex eigenvalues
          Lgen <- L;
          message(Sys.time(), ": Eigenvalue decomposition");
          outdata <- if (pars$norm) {
            DS <- Matrix::Diagonal(x = 1 / sqrt(Matrix::diag(D)))
            RSpectra::eigs_sym(DS %*% Lgen %*% DS,
                               k = pars$ndim + 1,
                               sigma = -1e-5)
          } else {
            RSpectra::eigs_sym(Lgen,
                               k = pars$ndim + 1,
                               sigma = -1e-5)
          };
          message("Eigenvalues: ", paste(format(outdata$values),
                                         collapse = " "));
          ## The eigenvalues are in decreasing order and we remove the
          ## smallest, which should be approx 0:
          outdata <- outdata$vectors[, order(outdata$values)[-1],
                                     drop = FALSE];
          colnames(outdata) <- paste0("LEIM", 1:ncol(outdata))
        },
        times = 1,
        unit = "s"
      )#$time * 1e-9
      eigen_time <- summary(eigen_time)$median
      
      message(Sys.time(), ": DONE")
      return(new(
        "dimRedResult",
        data         = new("dimRedData",
                           data = outdata,
                           meta = meta),
        org.data     = orgdata,
        running.time = c(knn_time, matrix_time, eigen_time),
        nn.idx       = knng$nn2res$nn.idx,
        has.org.data = keep.org.data,
        method       = "annleim",
        pars         = pars
      ))
    })
)
