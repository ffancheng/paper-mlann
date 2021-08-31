#' t-Distributed Stochastic Neighborhood Embedding
#'
#' An S4 Class for t-SNE.
#'
#' t-SNE is a method that uses Kullback-Leibler divergence between the
#' distance matrices in high and low-dimensional space to embed the
#' data. The method is very well suited to visualize complex
#' structures in low dimensions.
#'
#' @template dimRedMethodSlots
#'
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#' t-SNE can take the following parameters:
#' \describe{
#'   \item{d}{A distance function, defaults to euclidean distances}
#'   \item{perplexity}{The perplexity parameter, roughly equivalent to neighborhood size.}
#'   \item{theta}{Approximation for the nearest neighbour search, large values are more inaccurate.}
#'   \item{ndim}{The number of embedding dimensions.}
#' }
#'
#' @section Implementation:
#'
#' Wraps around \code{\link[Rtsne]{Rtsne}}, which is very well
#' documented. Setting \code{theta = 0} does a normal t-SNE, larger
#' values for \code{theta < 1} use the Barnes-Hut algorithm which
#' scales much nicer with data size. Larger values for perplexity take
#' larger neighborhoods into account.
#'
#' @references
#' van der Maaten, L., 2014. Accelerating t-SNE using Tree-Based
#' Algorithms. Journal of Machine Learning Research 15, 3221-3245.
#'
#' van der Maaten, L., Hinton, G., 2008. Visualizing Data using
#' t-SNE. J. Mach. Learn. Res. 9, 2579-2605.
#'
#' @examples
#' \dontrun{
#' library(dimRed)
#' dat <- loadDataSet("3D S Curve", n = 300)
#' emb <- embed(dat, "anntSNE", perplexity = 80)
#' plot(emb, type = "2vars")
#' }

anntSNE <- setClass(
  "anntSNE",
  contains = "dimRedMethod",
  prototype = list(
    stdpars = list(nn.idx = NULL,
                   nn.dists = NULL,
                   get_geod = FALSE,
                   knn = 30,
                   perplexity = 30,
                   theta = 0.5,
                   ndim = 2,
                   annmethod = "kdtree", 
                   eps = 0,
                   radius = 1,
                   nt = 50, 
                   nlinks = 16, 
                   ef.construction = 200,
                   ef.search = 10,
                   distance = c("euclidean", "manhattan"),
                   treetype = c("kd", "bd"), 
                   searchtype = c("standard", "priority", "radius")
    ),
    fun = function (data, pars,
                    keep.org.data = TRUE) {
      # chckpkg("Rtsne")
      # chcpkg(tidyverse)
      
      meta <- data@meta
      indata <- data@data
      orgdata <- if (keep.org.data) indata else NULL
      
      if (is.null(pars$eps))      pars$eps <- 0
      # if (is.null(pars$get_geod)) pars$get_geod <- FALSE
      if (is.null(pars$distance)) pars$distance <- "euclidean"
      if (length(pars$distance) > 1) pars$distance <- pars$distance[1]
      if(!is.null(pars$knn) & !is.null(pars$perplexity)) {
        pars$knn <- floor(3 * pars$perplexity)
        # message("The number of NN is set as three-fold the given perplexity, ", pars$knn)
      }
      
      if (!is.null(pars$nn.idx) & !is.null(pars$nn.dists)) {
        nn2res <- list(nn.idx = pars$nn.idx, nn.dists = pars$nn.dists)
      } else {
        
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
        
      }
      
      
      
      embed_time <- microbenchmark::microbenchmark( {
        
        # Convert RANN::nn2() output as N*N distance matrix, non-NN distances are set as 1e+05
        Kn <- nn2dist(nn2res)
        
        outdata <- Rtsne::Rtsne(X = Kn,
                                is_distance = TRUE,
                                dims = pars$ndim,
                                perplexity = pars$perplexity,
                                theta = pars$theta, 
        )$Y
        
        colnames(outdata) <- paste0("tSNE", 1:ncol(outdata))
      },
      times = 1,
      unit = "s"
      )#$time * 1e-9
      embed_time <- summary(embed_time)$median
      
      
      return(new(
        "dimRedResult",
        data         = new("dimRedData",
                           data = outdata,
                           meta = meta),
        org.data     = orgdata,
        has.org.data = keep.org.data,
        method       = "anntSNE",
        pars         = pars,
        running.time = c(knn_time, 0, embed_time),
        nn.idx       = nn2res$nn.idx,
        # nn.dists     = nn2res$nn.dists,
        has.apply    = TRUE,
        other.data   = ifelse (pars$get_geod, list(geod = as.dist(Kn)), list())
        ))
      
    })
)
