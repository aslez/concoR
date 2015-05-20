#'Hierarchical clustering using CONCOR.
#'
#'Paritions relational data using a CONvergence of iterated CORrelations 
#'(CONCOR) algorithm.

#'@param m0 A list of \eqn{n \times n} matrices, each of which refers to a
#'relation on a set of vertices.  Only one-mode data are supported at present.
#'#'@param cutoff A value between 0 and 1 used to determine convergence.
#'@param max.iter An integer representing the maximum number of iterations.
#'@param p An integer representing the desired number of partitions.

#'@return A \code{data.frame} depicting the block assignment for each vertex.
#'
#'@references Breiger, R.L., Boorman, S.A., and Arabie, P.  1975.  An Algorithm
#'for Clustering Relational Data with Applications to Social Network Analysis
#'and Comparison with Multidimensional Scaling.  \emph{Journal of Mathematical
#'Psychology}, 12: 328--383.
#'
#'@examples
#'data(bank_wiring)
#'b <- block(bank_wiring, p = 2)
#'lapply(bank_wiring, block_dens, blocks = b)

block <- function(m0, cutoff = 0.999, max.iter = 25, p = 1) {
  mat_stack <- do.call(rbind, m0)
  p_list <- list(mat_stack)
  for(i in 1:p) {
    p_list <- unlist(lapply(p_list, 
                            function(x) concor(x, cutoff, max.iter)), 
                     recursive = FALSE)
  }
  do.call(rbind, block_names(p_list))
}

#'@rdname block
concor <- function(m0, cutoff = 0.999, max.iter = 50) {
  if (ncol(m0) < 2) stop("Too few columns to partition.")
  mi <- cor(m0)
  iter <- 1
  while(any(abs(mi) <= cutoff) & iter <= max.iter) {
    mi <- cor(mi)
    iter <- iter + 1
  }
  group <- mi[, 1] > 0
  list(m0[, group, drop = FALSE], m0[, !group, drop = FALSE])
}

#'@rdname block
block_names <- function(p_list) {
  lapply(seq_along(p_list), 
         function(x) data.frame(block = x, 
                                vertex = names(p_list[[x]]),
                                stringsAsFactors = FALSE))
}
