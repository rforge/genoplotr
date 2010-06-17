##
## Barto multisegment plot
##
## Load library & data
library(genoPlotR)
data(barto)

## Saving data
## Uncomment the two commented lines if you wish to save the figures
## on your desktop
imgPath <- "../img"
pdfPath <- "../pdfs"
#imgPath <- "~/Desktop"
#pdfPath <- "~/Desktop"

## Adding a tree
tree <- newick2phylog("(BB:2.5,(BG:1.8,(BH:1,BQ:0.8):1.9):3);")
## Showing several subsegments per genome
xlims2 <- list(c(1445000, 1415000, 1380000, 1412000),
               c(  10000,   45000,   50000,   83000, 90000, 120000),
               c(  15000,   36000,   90000,  120000, 74000,  98000),
               c(   5000,    82000))
## Adding annotations for all genomes
annots <- lapply(barto$dna_segs, function(x){
  mid <- middle(x)
  annot <- annotation(x1=mid, text=x$name, rot=30)
  # removing gene names starting with "B" and keeping 1 in 4
  idx <- grep("^[^B]", annot$text, perl=TRUE)
  annot[idx[idx %% 4 == 0],] 
})

## Plots
png(file.path(imgPath, "barto_multiseg.png"), h=300, w=500)
plot_gene_map(barto$dna_segs, barto$comparisons, tree=tree,
              annotations=annots,
              xlims=xlims2,
              limit_to_longest_dna_seg=FALSE,
              dna_seg_scale=TRUE,
              main="Comparison of the same segment in 4 Bartonella genomes")
dev.off()
cairo_pdf(file.path(pdfPath, "barto_multiseg.pdf"), h=4, w=7)
plot_gene_map(barto$dna_segs, barto$comparisons, tree=tree,
              annotations=annots,
              xlims=xlims2,
              limit_to_longest_dna_seg=FALSE,
              dna_seg_scale=TRUE,
              main="Comparison of the same segment in 4 Bartonella genomes")
dev.off()
