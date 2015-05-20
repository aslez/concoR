#'Calculate tie densities.
#'
#'Uses tie density to assess the fit of a blockmodel on multiple relations.

#'@param m0 A list of \eqn{n \times n} matrices, each of which refers to a
#'relation on a set of vertices.  Only one-mode data are supported at present.
#'@param blocks A \code{data.frame} depicting the assignment of vertices 
#'to blocks.
#'@param digits The number of significant digits used when printing results.

#'@return A \code{list} of matrices representing block-by-block tie densities
#'for each relation containined in \code{m0}.
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

block_dens <- function(m0, blocks, digits = 3) {
  nb <- max(blocks$block)
  m_p <- as.matrix(m0[blocks$vertex, blocks$vertex])
  diag(m_p) <- NA
  m_d <- array(dim = c(nb, nb))
  for(i in 1:nb) {
    for(j in 1:nb) {     
      vert_i <- b$vertex[b$block == i]
      vert_j <- b$vertex[b$block == j]
      m_d[i, j] <- mean(m_p[vert_i, vert_j], na.rm = TRUE)
    }
  }
  dimnames(m_d) <- list(paste('Block', 1:nb), paste('Block', 1:nb))
  round(m_d, digits)
}
