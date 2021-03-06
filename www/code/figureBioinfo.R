##
## Figure to the article submitted to Bioinformatics
##

#### Gather data ####
library(genoPlotR)
data(barto)

## Saving data
## Uncomment the two commented lines if you wish to save the figures
## on your desktop
imgPath <- "../img"
pdfPath <- "../pdfs"
#imgPath <- "~/Desktop"
#pdfPath <- "~/Desktop"

names <- c("BB", "BG", "BH", "BQ")
## Adding a tree
tree <- newick2phylog("(BB:2.5,(BG:1.8,(BH:1,BQ:0.8):1.9):3);")

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
  annot[idx[idx %% 5 == 0],] 
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
## Example code
exampleCode <- c('dna_seg1 <- read_dna_seg_from_file("myGbk.gbk")',
                 #'dna_seg2 <- read_dna_seg_from_file("myEmbl.embl")',
                 #'dna_seg3 <- read_dna_seg_from_tab("myTab1.tab")',
                 'comp1 <- read_comparison_from_blast("myBlast.blast")',
                 #'comp2 <- read_comparison_from_tab("myTab2.tab")',
                 'tree <- newick2phylog("(A:2,(B:1,C:0.5):0.8);")',
                 'plot_gene_map(dna_segs=list(dna_seg1, dna_seg2, dna_seg3),',
                 '              comparisons=list(comp1, comp2), tree=tree)')
n_lines_code <- length(exampleCode)
code_cex <- 0.9
              
#### Plot ####
## Superstructure
#for (device in c("png", "pdf", "jpg", "eps")){
for (device in c("png", "pdf", "jpg")){
  if (device == "png") {
    png(file.path(imgPath, "figureBioinfo.png"), h=650, w=350)
  } else if (device == "pdf"){
    cairo_pdf(file.path(pdfPath, "figureBioinfo.pdf"), h=8, w=5)
  } else if (device == "jpg"){
    jpeg(file.path(imgPath, "figureBioinfo.jpg"), h=1250, w=700, quality=100, res=150)
  } else if (device == "eps"){
    #setEPS(horizontal=FALSE, onefile=FALSE, paper="special")
    cairo_ps(file.path(imgPath, "figureBioinfo.eps"), onefile=TRUE, height=8,
             width=5)
  }
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(4, 1,
                          heights=unit(c(1.3 + code_cex*n_lines_code, 1, 1.6, 0.8),
                            c("lines", "null", "null", "null")))))
  ## Example code
  pushViewport(viewport(layout.pos.row=1,
                        yscale=c(1.3 + code_cex*n_lines_code, 0),
                        xscale=c(0,1)))
  grid.text(label='A', x=0.02, y=0,
            just=c("left", "top"), gp=gpar(cex=1.2), default.units="native")
  for (i in 1:n_lines_code){
    grid.text(label=exampleCode[i], x=0.04, y=i,
              just=c("left", "top"), gp=gpar(cex=code_cex),
              default.units="native")
  }
  upViewport()
  ## Panel B
  pushViewport(viewport(layout.pos.row=2))
  plot_gene_map(dna_segs=bbone$dna_segs, comparisons=bbone$comparisons,
                n_scale_ticks=5, scale_cex=0.8, dna_seg_label_cex=1.2,
                tree=tree, dna_seg_scale=c(FALSE, FALSE, FALSE, TRUE),
                scale=FALSE, main="B", main_pos="left", plot_new=FALSE)
  upViewport()
  ## Panel C
  pushViewport(viewport(layout.pos.row=3))
  plot_gene_map(barto$dna_segs, barto$comparisons, tree=tree,
                annotations=annots, xlims=xlims,
                n_scale_ticks=5, scale_cex=0.8, dna_seg_label_cex=1.2,
                limit_to_longest_dna_seg=FALSE, scale=FALSE,
                dna_seg_scale=TRUE, main="C", main_pos="left",
                annotation_height=1.2, annotation_cex=0.8,
                arrow_head_len=1000,
                plot_new=FALSE)
  upViewport()
  ## Panel D
  pushViewport(viewport(layout.pos.row=4))
  plot_gene_map(chrY_subseg$dna_segs, chrY_subseg$comparison,
                annotations=list(annot_homo, annot_pan),
                dna_seg_label_cex=1.2,
                main="D", main_pos="left",
                plot_new=FALSE)
  upViewport()
  ##
  upViewport()
  dev.off()
}
