##
## Chromosome Y subsegment comparison
##
## Load library & data
library(genoPlotR)
data(chrY_subseg)

## Annotations
genes_homo <- unique(chrY_subseg$dna_segs[[1]]$gene)
x_homo <- sapply(genes_homo, function(x)
                 range(chrY_subseg$dna_segs[[1]]
                       [chrY_subseg$dna_segs[[1]]$gene == x,])
                 )
annot_homo <- annotation(x1=x_homo[1,], x2=x_homo[2,],
                         text=dimnames(x_homo)[[2]])
genes_pan <- unique(chrY_subseg$dna_segs[[2]]$gene)
x_pan <- sapply(genes_pan, function(x)
                range(chrY_subseg$dna_segs[[2]]
                      [chrY_subseg$dna_segs[[2]]$gene == x,])
                 )
annot_pan <- annotation(x1=x_pan[1,], x2=x_pan[2,],
                        text=dimnames(x_pan)[[2]])
## Title
main <- "Comparison of two subsegments in H. sapiens and P. troglodytes"
## Plots
png("../img/chrY_subseg.png", h=200, w=500)
plot_gene_map(chrY_subseg$dna_segs, chrY_subseg$comparison,
              annotations=list(annot_homo, annot_pan),
              dna_seg_scale=TRUE,
              main=main,
              scale=FALSE)
dev.off()
cairo_pdf("../pdfs/chrY_subseg.pdf", h=2.8, w=7)
plot_gene_map(chrY_subseg$dna_segs, chrY_subseg$comparison,
              annotations=list(annot_homo, annot_pan),
              dna_seg_scale=TRUE,
              main=main,
              scale=FALSE)
dev.off()
