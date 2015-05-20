\name{concor}
\alias{concor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
concor(m0, cutoff = 0.999, max.iter = 50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m0}{
%%     ~~Describe \code{m0} here~~
}
  \item{cutoff}{
%%     ~~Describe \code{cutoff} here~~
}
  \item{max.iter}{
%%     ~~Describe \code{max.iter} here~~
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
function (m0, cutoff = 0.999, max.iter = 50) 
{
    if (ncol(m0) < 2) 
        stop("Too few columns to partition.")
    mi <- cor(m0)
    iter <- 1
    while (any(abs(mi) <= cutoff) & iter <= max.iter) {
        mi <- cor(mi)
        iter <- iter + 1
    }
    group <- mi[, 1] > 0
    list(m0[, group, drop = FALSE], m0[, !group, drop = FALSE])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
