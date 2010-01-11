\name{range.dna_seg}
\alias{range.dna_seg}
\alias{range.comparison}
\title{
  Range calculation
}
\description{
  Calculate the range of dna_seg and comparisons.
}
\usage{
\method{range}{dna_seg}(x, ...)
\method{range}{comparison}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object to calculate the range from.
  }
  \item{\dots}{
    Unused.
  }
}
\details{
  Calculate the overall range of a \code{dna_seg} or
  \code{comparison} object. 
}
\value{
  A numeric of length 2.
}
\author{
  Lionel Guy
}
\seealso{
  \code{\link{dna_seg}}, \code{\link{comparison}}
}
\examples{
## load data
data(three_genes)

## on dna_seg
dna_segs[[1]]
range(dna_segs[[1]])

## on comparison
comparisons[[2]]
range(comparisons[[2]])
}
\keyword{ data }
