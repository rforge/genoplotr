\name{apply_color_scheme}
\alias{apply_color_scheme}
\title{
  Apply a color scheme
}
\description{
  Apply a color scheme to a numeric vector, eventually taking the
  direction into account.
}
\usage{
apply_color_scheme(x, direction = NULL, color_scheme = "grey")
}
\arguments{
  \item{x}{
    A numeric, that will be used to apply a gradient of colors to a
    comparison. 
  }
  \item{direction}{
    If a red-blue scheme is choosen, the vector (composed of -1 and 1
    values and of same length as \code{x}) giving the direction of the
    comparison. 
  }
  \item{color_scheme}{
    Character. One of \code{red_blue}, \code{blue_red}, \code{grey},
    \code{gray}. 
  }
}
\details{
  A color scale is calculated, with the darker color corresponding to
  the highest values of \code{x}. For the moment, two schemes (red-blue
  and grey scale) are used.

  For the red-blue scale (as in ACT), the direct comparisons are
  colored in red hues, and the reversed ones in blue hues.

  This is especially useful to replace comparison values (such as BLAST
  percent identity values) by color hues.
}
\value{
  A character vector of same length as \code{x}, representing colors.
}
\references{
  Artemis Comparison Tool, \url{http://www.sanger.ac.uk/Software/ACT/}
}
\author{
  Lionel Guy
}
\seealso{
  \code{\link{comparison}}
}
\examples{
## load data
data(three_genes)

## color_schemes
## greys
comparisons[[1]]$col <- c(70, 80, 90)
comparisons[[1]]$col <- apply_color_scheme(comparisons[[1]]$col,
                                           color_scheme="grey")
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons)
## red-blue
comparisons[[1]]$col <- c(70, 80, 90)
comparisons[[1]]$col <- apply_color_scheme(comparisons[[1]]$col,
                                           direction=comparisons[[1]]$direction,
                                           color_scheme="red_blue")
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons)
}
\keyword{ data }
