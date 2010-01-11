\name{trim}
\alias{trim}
\alias{trim.default}
\alias{trim.dna_seg}
\alias{trim.comparison}
\title{
  Trimming data frames with >= 2 numeric columns 
}
\description{
  Trims data frames with 2 or more numeric columns using a
  xlim. \code{xlim}(s) are as used to filter rows whose numeric values are
  included in this interval.
}
\usage{
trim(x, ...)
\method{trim}{default}(x, xlim = NULL, ...)
\method{trim}{dna_seg}(x, xlim = NULL, ...)
\method{trim}{comparison}(x, xlim1 = NULL, xlim2 = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    An object to trim,. generally a data frame or a matrix.
  }
  \item{xlim}{
    A numeric of length 2. In a general case, the rows whose values are
    included in this interval are returned.
  }
  \item{\dots}{
    Unused.
  }
  \item{xlim1}{
    A numeric of length 2. In the case of comparison, where the
    comparison can be filtered on two sides, the interval to filter the
    first side. 
  }
  \item{xlim2}{
    A numeric of length 2. The interval to filter the second side.
  }
}
\value{
  Returns the same object as input, with the rows corresponding to the
  interval given.
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
