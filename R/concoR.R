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

block_names <- function(p_list) {
  lapply(seq_along(p_list), 
         function(x) data.frame(block = x, 
                                vertex = names(p_list[[x]]),
                                stringsAsFactors = FALSE))
}

block <- function(m0, cutoff = 0.999, max.iter = 25, p = 1) {
  p_list <- list(m0)
  for(i in 1:p) {
    p_list <- unlist(lapply(p_list, 
                            function(x) concor(x, cutoff, max.iter)), 
                     recursive = FALSE)
  }
  do.call(rbind, block_names(p_list))
}

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
