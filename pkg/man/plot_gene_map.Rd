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
              arrow_head_len = 200,
              dna_seg_line = TRUE,
              scale = TRUE, 
              dna_seg_scale = !scale,
              global_color_scheme = c("auto", "auto", "blue_red"),
              override_color_schemes = FALSE,
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
  \item{arrow_head_len}{
    A numeric. Gives the length of arrow heads for gene type
    "arrows". The arrow head extends at maximum at half of the gene. Set
    to \code{Inf} to have all arrow heads covering the half of the
    gene. 200 by default.
  }
  \item{dna_seg_line}{
    A vector, either logical or giving colors, of length 1 or of same
    length as \code{dna_segs}. Should the line in the middle of the
    segments be drawn, and if yes, in what color. \code{TRUE} by
    default, which gives black lines. \code{FALSE} (logical, or as a
    string) results in no plotting.
  }
  \item{scale}{
    A logical. Should the scale be displayed on the plot. \code{TRUE} by
    default. 
  }
  \item{dna_seg_scale}{
    A logical, of length one or of the same length as
    \code{dna_segs}. Should a scale be displayed below each or all dna
    segments, respectively. \code{!scale} by default. 
  }
  \item{global_color_scheme}{
    A character of length 3. If no \code{col} column is present on any
    comparison or is \code{override_color_schemes} is set, apply a
    global color scheme over all comparions. See below for more details.
    \code{c("auto", "auto", "blue_red")} by default. 
  }
  \item{override_color_schemes}{
    A logical. If \code{TRUE}, apply a global color scheme even if there
    are comparisons that have \code{col} columns. \code{FALSE} by
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

  The three elements of \code{global_color_scheme} are (i) which column
  serves as scale to apply the color  scheme, or "auto" (default);
  (ii) if the scale is increasing or decreasing (see
  \code{apply_color_scheme} for more details), or "auto" (default);
  (iii) the color scheme to apply.
}
\value{
  %% Change this when/if would return gTree
  Nothing. A lattice graphic is plotted on the current device.
}
\author{
  Lionel Guy \email{lionel.guy@ebc.uu.se}, Jens Roat Kultima
}
\note{
  This plotting function has been tested as far as possible, but given
  its complexity and that the package is young, bugs or strange behaviors
  are possible. Please report them to the author.

  As of 10/3/2010, support for viewing exons/introns is only available
  using genbank and embl formats, not when importing ptt files.
}
\seealso{
  \code{\link{dna_seg}} and \code{\link{comparison}} for the base objects;
  \code{\link{read_dna_seg_from_tab}}, \code{\link{read_dna_seg_from_ptt}},
  \code{\link{read_comparison_from_tab}} and
  \code{\link{read_comparison_from_blast}} to read from files;
  \code{\link{gene_types}} for \code{gene_type} argument;
  \code{\link{apply_color_scheme}} for color schemes;
}
\examples{
old.par <- par(no.readonly=TRUE)
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

## dna_seg_line
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons,
              dna_seg_line=c("FALSE", "red", grey(0.6)))

## gene_type
plot_gene_map(dna_segs=dna_segs, comparisons=comparisons,
              gene_type="side_blocks")

## dna_seg_scale & global_color_scheme
data("barto")
tree <- newick2phylog("(BB:2.5,(BG:1.8,(BH:1,BQ:0.8):1.9):3);")
xlims <- data.frame(x0=c(1400000, 10000, 15000, 8000),
                    x1=c(1445000, 63000, 58000, 62000))
plot_gene_map(barto$dna_segs, barto$comparisons, tree=tree, xlims=xlims)
plot_gene_map(barto$dna_segs, barto$comparisons, tree=tree, xlims=xlims,
              dna_seg_scale=c(TRUE, FALSE, FALSE, TRUE), scale=FALSE,
              global_color_scheme=c("e_value","auto","grey"))

##
## Exploring and modifying a gene map plot
##
plot_gene_map(barto$dna_segs, barto$comparisons, tree=tree, xlims=xlims)
## View viewports
current.vpTree()
## Go down to one of the viewports, add an xaxis, go back up to root viewport
downViewport("scale")
grid.xaxis()
upViewport(0)
## Get all the names of the objects
grobNames <- getNames()
grobNames
## Change the color ot the scale line
grid.edit("scale.lines", gp=gpar(col="grey"))
## Remove first dna_seg_lines
grid.remove("dna_seg_line.1")

##
## Plot genoPlotR logo
##
col <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
         "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")
cex <- 2.3
## First segment 
start1 <- c(150, 390, 570)
end1   <- c(  1, 490, 690)
genoR <- c(270, 530)
## Second segment
start2 <- c(100, 520, 550)
end2   <- c(240, 420, 650)
Plot <- c(330)
## dna_segs
ds1 <- as.dna_seg(data.frame(name=c("", "", ""),
                             start=start1, end=end1, strand=rep(1, 3),
                             col=col[c(2, 6, 1)], stringsAsFactor=FALSE))
ds_genoR <- as.dna_seg(data.frame(name=c("geno", "R"),
                             start=genoR, end=genoR, strand=rep(1, 2),
                             col=c(col[8], "black"),
                             stringsAsFactor=FALSE), cex=cex, gene_type="text")
ds2 <- as.dna_seg(data.frame(name=c("", "", ""),
                             start=start2, end=end2, strand=rep(1, 3),
                             col=col[c(5, 3, 7)],
                             stringsAsFactor=FALSE))
ds_Plot <- as.dna_seg(data.frame(name="Plot",
                             start=Plot, end=Plot, strand=1,
                             col=col[c(1)],
                             stringsAsFactor=FALSE), cex=cex, gene_type="text")
## comparison
c1 <- as.comparison(data.frame(start1=start1, end1=end1,
                               start2=start2, end2=end2,
                               col=grey(c(0.6, 0.8, 0.5))))
## Generate genoPlotR logo
\dontrun{
cairo_pdf("logo.pdf", h=0.7, w=3)
}
par(fin=c(0.7, 3))
plot_gene_map(dna_segs=list(c(ds1, ds_genoR), c(ds2, ds_Plot)),
              comparisons=list(c1), scale=FALSE,
              dna_seg_line=grey(0.7), offsets=c(-20,160))
\dontrun{
dev.off()
}
par(old.par)
}
\keyword{ hplot }
