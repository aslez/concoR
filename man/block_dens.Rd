\name{block_dens}
\alias{block_dens}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
block_dens(m0, blocks, digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m0}{
%%     ~~Describe \code{m0} here~~
}
  \item{blocks}{
%%     ~~Describe \code{blocks} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (m0, blocks, digits = 3) 
{
    nb <- max(blocks$block)
    m_p <- as.matrix(m0[blocks$vertex, blocks$vertex])
    diag(m_p) <- NA
    m_d <- array(dim = c(nb, nb))
    for (i in 1:nb) {
        for (j in 1:nb) {
            vert_i <- b$vertex[b$block == i]
            vert_j <- b$vertex[b$block == j]
            m_d[i, j] <- mean(m_p[vert_i, vert_j], na.rm = TRUE)
        }
    }
    dimnames(m_d) <- list(paste("Block", 1:nb), paste("Block", 
        1:nb))
    round(m_d, digits)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
