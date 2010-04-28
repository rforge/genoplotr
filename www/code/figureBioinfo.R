##
## Figure to the article submitted to Bioinformatics
##

#### Gather data ####
library(genoPlotR)
data(barto)
#names <- c("B_bacilliformis", "B_grahamii", "B_henselae", "B_quintana")
names <- c("BB", "BG", "BH", "BQ")
#names(barto$dna_segs) <- names
## Adding a tree
tree <- newick2phylog("(BB:2.5,(BG:1.8,(BH:1,BQ:0.8):1.9):3);")
#tree <- newick2phylog("(B_bacilliformis:2.5,(B_grahamii:1.8,(B_henselae:1,B_quintana:0.8):1.9):3);")


## Panel A: Bartonella mauve output
bbone_file <- system.file('extdata/barto.backbone', package = 'genoPlotR')
## Read backbone
bbone <- read_mauve_backbone(bbone_file, ref=2, filter_low=2000) 
names(bbone$dna_segs) <- names

## Panel B: Barto multisegment plot
## Showing several subsegments per genome
xlims <- list(c(1445000, 1415000, 1380000, 1412000),
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

## Panel C: Chromosome Y subsegment comparison
## Load data
data(chrY_subseg)
names(chrY_subseg$dna_segs) <- c("Homo", "Pan")
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

#### Plot ####
## Superstructure

for (device in c("png", "pdf", "jpg")){
  if (device == "png") {
    png("../img/figureBioinfo.png", h=500, w=350)
  } else if (device == "pdf"){
    cairo_pdf("../pdfs/figureBioinfo.pdf", h=7, w=5)
  } else {
    jpeg("../img/figureBioinfo.jpg", h=500, w=350, quality=90)
  }
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(3,1,
                          heights=unit(c(1,1.3,0.8), rep("null", 3)))))
  ## Panel A
  pushViewport(viewport(layout.pos.row=1))
  plot_gene_map(dna_segs=bbone$dna_segs, comparisons=bbone$comparisons,
                tree=tree, dna_seg_scale=c(FALSE, FALSE, FALSE, TRUE),
                scale=FALSE, main="A", main_pos="left", plot_new=FALSE)
  upViewport()
  ## Panel B
  pushViewport(viewport(layout.pos.row=2))
  plot_gene_map(barto$dna_segs, barto$comparisons, tree=tree,
              annotations=annots, xlims=xlims,
              limit_to_longest_dna_seg=FALSE, scale=FALSE,
              dna_seg_scale=TRUE, main="B", main_pos="left",
              annotation_height=0.6, annotation_cex=0.5, 
              plot_new=FALSE)
  upViewport()
  ## Panel C
  pushViewport(viewport(layout.pos.row=3))
  plot_gene_map(chrY_subseg$dna_segs, chrY_subseg$comparison,
                annotations=list(annot_homo, annot_pan),
                dna_seg_scale=TRUE, scale=FALSE, main="C", main_pos="left",
                plot_new=FALSE)
  upViewport(2)
  dev.off()
}
