annHLLE <- setClass(
  "annHLLE",
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
    fun = function(data, pars,
                   keep.org.data = TRUE) {
      
      require(BiocNeighbors)
      if (pars$ndim < 2) stop("ndim must be 2 or larger.")
      if (pars$knn < (pars$ndim + pars$ndim*(pars$ndim+1)/2)) stop("knn is too small for Hessian matrix when ndim=", D, ". Increase knn to be no less than ", pars$ndim + pars$ndim*(pars$ndim+1)/2, ".")
      
      if (is.null(pars$knn))   pars$knn  <- 50
      if (is.null(pars$ndim))  pars$ndim <- 2
      if (is.null(pars$eps))   pars$eps <- 0
      if (is.null(pars$distance)) pars$distance <- "euclidean"
      if (length(pars$distance) > 1) pars$distance <- pars$distance[1]
      
      indata <- data@data
      n <- nrow(indata)
      hs <- pars$ndim * (pars$ndim + 1) / 2
      W <- Matrix::sparseMatrix(i = numeric(0),
                                j = numeric(0),
                                x = numeric(0),
                                dims = c(n, hs * n))
      ii <- jj <- ww <- list()
      ## Identify neighbors:
      message(Sys.time(), ": Finding nearest neighbors", sep = "")
      # if (pars$eps == 0){
      #   ## compute pairwise distances matrix
      #   # d <- t(rowSums(x^2))
      #   # d <- d[rep(1, each = M), ]
      #   # d <- d + t(d) - 2 * x %*% t(x)
      #   # diag(d) <- 0
      #   # d <- sqrt(d)
      #   d <- pairwiseDistances(indata)
      #   idx <- apply(d, 2, order)
      #   nnidx <- t(idx[1:(pars$knn+1),]) # equal to nn2res$idx
      # } else {
        # nnidx <- RANN::nn2(data = indata, query = indata, k = pars$knn + 1,
        #                    treetype = "kd", "standard", eps = pars$eps)$nn.idx#[, -1]
      # }
      
      # knn_time <- microbenchmark::microbenchmark(
      #   base::switch(annmethod,
      #                "kdtree" = {
      #                  message(Sys.time(), ": Finding ANN using ", annmethod, sep = "")
      #                  treetype <- "kd"                
      #                  searchtype <- "priority"
      #                  nnidx <- base::switch(pars$distance,
      #                                        "euclidean" = {RANN::nn2(data = indata, query = indata, k = pars$knn + 1, treetype = treetype, searchtype = searchtype, eps = pars$eps)$nn.idx},
      #                                        "manhattan" = {RANN.L1::nn2(data = indata, query = indata, k = pars$knn + 1, treetype = treetype, searchtype = searchtype, eps = pars$eps)$nn.idx})
      #                },
      #                "annoy"   = {
      #                  message(Sys.time(), ": Finding ANN using ", annmethod, sep = "")
      #                  nnidx <- base::switch(pars$distance,
      #                                        "euclidean" = {BiocNeighbors::queryKNN(X = indata, query = indata, k = pars$knn + 1, BNPARAM = AnnoyParam(ntrees = pars$nt, distance = "Euclidean"))$index},
      #                                        "manhattan" = {BiocNeighbors::queryKNN(X = indata, query = indata, k = pars$knn + 1, BNPARAM = AnnoyParam(ntrees = pars$nt, distance = "Manhattan"))$index})
      #                }, 
      #                "hnsw"    = {
      #                  message(Sys.time(), ": Finding ANN using ", annmethod, sep = "")
      #                  nnidx <- base::switch(pars$distance,
      #                                        "euclidean" = {BiocNeighbors::queryKNN(X = indata, query = indata, k = pars$knn + 1, BNPARAM = HnswParam(nlinks = pars$nlinks, ef.construction = pars$ef.construction, distance = "Euclidean"))$index},
      #                                        "manhattan" = {BiocNeighbors::queryKNN(X = indata, query = indata, k = pars$knn + 1, BNPARAM = HnswParam(nlinks = pars$nlinks, ef.construction = pars$ef.construction, distance = "Manhattan"))$index})
      #                }
      #   ),
      #   times = 1,
      #   unit = "s"
      # )#$time * 1e-9  
      # knn_time <- summary(knn_time)$median
      
      
      message(Sys.time(), ": Finding ANN using ", annmethod, sep = "")
      knn_time <- microbenchmark::microbenchmark(
        nn2res <- find_ann(x = indata, 
                           knn = pars$knn,
                           get_geod = FALSE,
                           annmethod = pars$annmethod, 
                           eps = pars$eps, 
                           radius = pars$radius,
                           nt = pars$nt, 
                           nlinks = pars$nlinks, 
                           ef.construction = pars$ef.construction,
                           ef.search = pars$ef.search,
                           distance = pars$distance,
                           treetype = pars$treetype,
                           searchtype = pars$searchtype),
        times = 1,
        unit = "s"
      )#$time * 1e-9
      knn_time <- summary(knn_time)$median
      nnidx <- nn2res$nn.idx
      
      
      message(Sys.time(), ": Calculating Hessian", sep = "")
      hessian_time <- microbenchmark::microbenchmark(
        {
          for (i in seq_len(n)) {
            cat(i, "/", n, "\r", sep = "")
            ## get neighborhood
            Nui <- indata[nnidx[i, ], , drop = FALSE]
            
            ## Form tangent coordinates:
            Nui <- sweep(Nui, 2, colMeans(Nui), "-")
            tc <- svd(Nui, nu = pars$ndim, nv = 0)$u
            
            ## Develop Hessian Estimator
            Xi <- cbind(
              1, tc, tc ^ 2,
              apply(combn(seq_len(pars$ndim), 2), 2,
                    function(x) tc[, x[1]] * tc[, x[2]])
            )
            tHi <- qr.Q(qr(Xi))[, -(1:(pars$ndim + 1)),
                                drop = FALSE]
            
            ## Add quadratic form to hessian
            ii[[i]] <- rep(nnidx[i, ], hs)
            jj[[i]] <- rep((i - 1) * hs + (1:hs), each = ncol(nnidx))
            ww[[i]] <- as.vector(tHi)
          };
          H <- as(Matrix::tcrossprod(Matrix::spMatrix(
            i = unlist(ii, FALSE, FALSE),
            j = unlist(jj, FALSE, FALSE),
            x = unlist(ww, FALSE, FALSE),
            nrow = n, ncol = n * hs)
          ), "dgCMatrix")
        },
        times = 1,
        unit = "s"
      )#$time * 1e-9
      hessian_time <- summary(hessian_time)$median
      
      ## Find null space:
      message(Sys.time(), ": Embedding", sep = "")
      
      eigen_time <- microbenchmark::microbenchmark(
        {
          ## eigs and eigs_sym converges much more reliably and faster
          ## with sigma = -eps than with which = "L*"
          outdata <- RSpectra::eigs_sym(H, k = pars$ndim + 1, sigma = -1e-5);
          
          message(paste(c("Eigenvalues:", format(outdata$values)),
                        collapse = " "));
          outdata <- outdata$vectors[, order(outdata$values)[-1], drop = FALSE];
          
          colnames(outdata) <- paste0("HLLE", seq_len(ncol(outdata)))
        },
        times = 1,
        unit = "s"
      )#$time * 1e-9
      eigen_time <- summary(eigen_time)$median
      
      message(Sys.time(), ": DONE", sep = "")
      return(new(
        "dimRedResult",
        data         = new("dimRedData",
                           data = outdata,
                           meta = data@meta),
        org.data     = if (keep.org.data) data@data else NULL,
        running.time = c(knn_time, hessian_time, eigen_time),
        nn.idx       = nnidx,
        has.org.data = keep.org.data,
        method       = "annHLLE",
        pars         = pars
      ))
    })
)
