\name{plot_gene_map}
\alias{plot_gene_map}
\title{
  Plot gene and genome maps
}
\description{
  This plotting function represents linearly DNA segments and their
  comparisons. It will plot one line per DNA segment, eventually
  separated by the comparisons. In addition, a tree can be plotted on
  the left of the plot, and annotations on the top row. Since this is a
  grid plot, it can be placed into other graphics, or modified subsequently.
}
\usage{
plot_gene_map(dna_segs,
              comparisons = NULL,
              tree = NULL,
              tree_width = NULL,
              legend = NULL, 
              annotation = NULL, 
              annotation_height = 1, 
              annotation_cex = 0.8, 
              xlims = NULL,
              offsets = NULL, 
              main = NULL, 
              main_pos = "centre", 
              dna_seg_labels = NULL, 
              gene_type = NULL, 
              scale = TRUE, 
              plot_new=TRUE,
              debug = 0
              )
}
\arguments{
  \item{dna_segs}{
    A list of \code{dna_seg} objects. Mandatory.
  }
  \item{comparisons}{
    A list of \code{comparison} objects. Optional. 
  }
  \item{tree}{
    A tree, under the form of a \code{\link[ade4]{phylog}} object. If
    specified, takes place at the left of the tags. The tree doesn't
    need to be ordered as the \code{dna_seg_labels}, but a permutation
    of the tree with that order should exist.
  }
  \item{tree_width}{
    Numeric. The width of the tree area in the plot, in inches. By
    default, takes 20 percent of the total plot.
  }
  \item{legend}{
    Unimplemented.
  }
  \item{annotation}{
    An \code{annotation} object. See details. Optional.
  }
  \item{annotation_height}{
    Numeric. The height, in lines, of the annotation line. One by
    default, if \code{annotation is defined}.
  }
  \item{annotation_cex}{
    Numeric. The \code{cex} (i.e. the character expansion) of the
    annotation line. 
  }
  \item{xlims}{
    A data frame with as many rows as there are \code{dna_seg}s, and
    with numeric columns \code{x0} and \code{x1}.
  }
  \item{offsets}{
    A numeric as long as there are \code{dna_seg}s. Manually change the
    alignment of segments: gives the first nucleotide displayed on the
    left of the plot. If \code{NULL}, an alignment minimizing the
    length of the edges of the comparison blocks is calculated.
  }
  \item{main}{
    A character. Main title of the plot.
  }
  \item{main_pos}{
    Position of the main title. One of \code{centre}, \code{left} or
    \code{right}.
  }
  \item{dna_seg_labels}{
    A character, same length as \code{dna_segs}. The names of the
    segments. If \code{NULL}, the names of \code{dna_segs} will be
    used, if available. Else, no name are plotted. If a \code{tree} is
    given, names must exist either in \code{dna_seg_labels} or in the
    names of \code{dna_segs}.
  }
  \item{gene_type}{
    A character. Describes the type of representation of genes or
    \code{dna_seg} elements. See details.
  }
  \item{scale}{
    A logical. Should the scale be displayed on the plot. \code{TRUE} by
    default. 
  }
  \item{plot_new}{
    Logical. Produce a new plot? If \code{TRUE}, uses
    \code{grid.newpage} before plotting.
  }
  \item{debug}{
    A numeric. If > 0, only that number of element will be plotted for
    each \code{dna_seg} and \code{comparison}.
  }
}
\details{
  One line is plotted per \code{dna_seg}. Eventually, the space between
  the lines will be filled with the \code{comparisons}. A phylogenetic
  tree (a \code{phylog} object from package \code{ade4}) can be drawn at
  the left of the plot.

  A scale, a main title and an annotation row at the top of the plot can
  also be added.

  The format of the elements of \code{dna_segs} is previously determined
  in the object or can be globally set by \code{gene_type}. See the
  function \code{\link{gene_types}} to return the available types.
}
\value{
  %% Change this when/if would return gTree
  Nothing. A lattice graphic is plotted on the current device.
}
\author{
  Lionel Guy \email{lionel.guy@ebc.uu.se}
}
\note{
  This plotting function has been tested as far as possible, but given
  its complexity and that the package is young, bugs or strange behaviors
  are possible. Please report them to the author.
}
\seealso{
  \code{\link{dna_seg}} and \code{\link{comparison}} for the base objects;
  \code{\link{read_dna_seg_from_tab}}, \code{\link{read_dna_seg_from_ptt}},
  \code{\link{read_comparison_from_tab}} and
  \code{\link{read_comparison_from_blast}} to read from files;
  \code{\link{gene_types}} for \code{gene_type} argument;
}
\examples{
data("three_genes")

## segments only
plot_gene_map(dna_segs=dna_segs) 

## with comparisons
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons) 

## tree
names <- c("A_aaa", "B_bbb", "C_ccc")
names(dna_segs) <- names
tree <- newick2phylog("(((A_aaa:4.2,B_bbb:3.9):3.1,C_ccc:7.3):1);")
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons,
              tree=tree)

## increasing tree width
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons,
              tree=tree, tree_width=3)

## annotation
mid_pos <- apply(dna_segs[[1]][,c("start", "end")], 1, mean)
annot1 <- annotation(x1=mid_pos, text=dna_segs[[1]]$name)
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons, annotation=annot1)
## exploring options
annot2 <- annotation(x1=c(mid_pos[1], dna_segs[[1]]$end[2]),
                     x2=c(NA, dna_segs[[1]]$end[3]),
                     text=c(dna_segs[[1]]$name[1], "region1"),
                     rot=c(30, 0), col=c("grey", "black"))
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons,
              annotation=annot2, annotation_height=1.3)

## xlims
xlims <- data.frame(x0=rep(NA, length(dna_segs)),
                    x1=rep(NA, length(dna_segs)))
for (i in 1:length(dna_segs)) xlims[i,] <- range(dna_segs[[i]])
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons, xlims=xlims)
xlims[3,2] <- 2800
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons, xlims=xlims)
xlims[3,] <- c(1, 4000)
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons, xlims=xlims)

## offsets
offsets <- c(0, 0, 0)  
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons, offsets=offsets)
offsets <- c(200, 400, 0)  
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons, offsets=offsets)

## main
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons,
              main="Comparison of A, B and C")
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons,
              main="Comparison of A, B and C", main_pos="left")

## dna_seg_labels
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons,
              dna_seg_labels=c("Huey", "Dewey", "Louie"))

## gene_type
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons,
              gene_type="side_blocks")
}
\keyword{ hplot }
