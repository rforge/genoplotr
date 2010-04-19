##
## Barto subsegment 1
##
## Load library & data
library(genoPlotR)
data(barto)

## Reference segment 2
xlim_ref <- c(10000, 45000)
barto$dna_segs[[2]] <- trim(barto$dna_segs[[2]], xlim=xlim_ref)
## Seg 1
barto$comparisons[[1]] <- trim(barto$comparisons[[1]], xlim2=xlim_ref)
xlim1 <- range(barto$comparisons[[1]], overall=FALSE)$xlim1
barto$dna_segs[[1]] <- trim(barto$dna_segs[[1]], xlim=xlim1)
## Seg 3
barto$comparisons[[2]] <- trim(barto$comparisons[[2]], xlim1=xlim_ref)
xlim3 <- range(barto$comparisons[[2]], overall=FALSE)$xlim2
barto$dna_segs[[3]] <- trim(barto$dna_segs[[3]], xlim=xlim3)
## Seg 4
barto$comparisons[[3]] <- trim(barto$comparisons[[3]], xlim1=xlim3)
xlim4 <- range(barto$comparisons[[3]], overall=FALSE)$xlim2
barto$dna_segs[[4]] <- trim(barto$dna_segs[[4]], xlim=xlim4)
## Annotations
mids <- apply(barto$dna_segs[[1]][c("start", "end")], 1, mean)
text <- barto$dna_segs[[1]]$name
text[grep("BARBAKC", text)] <- ""
annot <- annotation(x1=mids, text=text, rot=30)
## Tree
tree <- newick2phylog("(BB:2.5,(BG:1.8,(BH:1,BQ:0.8):1.9):3);")

## Plots
png("../img/barto_seg1.png", h=300, w=500)
plot_gene_map(barto$dna_segs, barto$comparisons, tree=tree,
              annotations=annot, dna_seg_scale=c(rep(FALSE, 3), TRUE),
              scale=FALSE,
              main="Comparison of the same segment in 4 Bartonella genomes")
dev.off()
cairo_pdf("../pdfs/barto_seg1.pdf", h=4, w=7)
plot_gene_map(barto$dna_segs, barto$comparisons, tree=tree,
              annotations=annot, dna_seg_scale=c(rep(FALSE, 3), TRUE),
              scale=FALSE,
              main="Comparison of the same segment in 4 Bartonella genomes")
dev.off()
