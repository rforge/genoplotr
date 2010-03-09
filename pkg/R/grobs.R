################################################################################
# Grobs
################################################################################
# create gene grobs
gene_grob <- function(gene){
  if (!is.dna_seg(gene)) stop("A dna_seg object is required")
  if (nrow(gene) > 1) stop ("gene must be single-row")
  # arrows
  mid <- (gene$start + gene$end)/2
  if (gene$gene_type == "arrows"){
    arrow <- arrow_coord(x1=gene$start, x2=gene$end,
                         y=0.5, strand=gene$strand, head_len=100)
    grob <- polygonGrob(arrow$x, arrow$y, name=gene$name,
                        gp=gpar(fill=gene$col, lty=gene$lty, lwd=gene$lwd),
                        default.units="native")
  }
  # blocks
  else if (gene$gene_type == "blocks" || gene$gene_type == "side_blocks"){
    if (gene$gene_type == "side_blocks"){
      block <- block_coord(gene$start, gene$end, strand=gene$strand, y=0.5)
    }
    else {
      block <- block_coord(gene$start, gene$end, strand=2, y=0)
    }
    grob <- polygonGrob(block$x, block$y, name=gene$name,
                        gp=gpar(fill=gene$col, col=gene$col, lty=gene$lty,
                          lwd=gene$lwd),
                        default.units="native")  
  }
  # bars
  else if (gene$gene_type == "bars" || gene$gene_type == "side_bars") {
    if (gene$gene_type == "side_bars"){
      y0 <- 0.5; y1 <- 0.5+gene$strand/2
    }
    else {
      y0 <- 0; y1 <- 1
    }
    grob <- segmentsGrob(x0=mid, y0=y0, x1=mid, y1=y1,
                         gp=gpar(col=gene$col, lwd=gene$lwd, lty=gene$lty),
                         default.units="native")
  }

  # introns
  else if (gene$gene_type == "intron") {
    gM <- gene$start + (gene$end - gene$start) / 2
    intron <- list(x0=c(gene$start, gM), x1=c(gM, gene$end), y0=c(1,1.5), y1=c(1.5, 1))
    grob <- segmentsGrob(x0=intron$x0, y0=intron$y0, x1=intron$x1, y1=intron$y1, name=gene$name,
                        gp=gpar(lty=gene$lty, lwd=gene$lwd),
                        default.units="native")
  }
  
  # points
  else if (gene$gene_type == "points" || gene$gene_type == "side_points") {
    if (gene$gene_type == "side_points"){
      y <- 0.5+gene$strand/4
    }
    else {
      y <- 0.5
    }
    grob <- pointsGrob(x=mid, y=y, pch=gene$pch, size=unit(gene$cex/2, "char"),
                       gp=gpar(col=gene$col),
                       default.units="native")
  }
  # text
  else if (gene$gene_type == "text" || gene$gene_type == "side_text") {
    if (gene$gene_type == "side_text"){
      just <- c("centre", c("top", "bottom")[gene$strand/2 + 1.5])
    }
    else {
      just <- c("centre", "centre")
    }
    grob <- textGrob(label=gene$name, x=mid, y=0.5, just=just,
                     gp=gpar(col=gene$col, cex=gene$cex),
                     default.units="native")
  }
  else {
    stop(paste("Invalid gene_type:",  gene$gene_type))
  }
  grob
}
# create dna_seg grobs
dna_seg_grob <- function(dna_seg){
  if(!is.dna_seg(dna_seg)) stop("A dna_seg object is required")
  grob_list <- gList()
  if (nrow(dna_seg) < 1) return(grob_list)
  for (i in 1:nrow(dna_seg)){
    gene <- as.dna_seg(dna_seg[i,])
    grob_list[[i]] <- gene_grob(gene)
  }
  grob_list
}
# create similarity grobs
similarity_grob <- function(similarity, xlim1, xlim2, offset1, offset2){
  if (!is.comparison(similarity)) stop("A comparison object is required")  
  if (nrow(similarity) > 1) stop ("gene must be single-row")
  if (!all(is.numeric(c(offset1, offset2)))) stop("Offsets must be numeric")
  x1 <- c(similarity$start1, similarity$end1)+offset1-xlim1[1]
  x2 <- c(similarity$end2, similarity$start2)+offset2-xlim2[1]
  polygonGrob(x=c(x1, x2), y=c(1, 1, 0, 0),
              gp=gpar(fill=similarity$col, col=similarity$col, lwd=0.1),
              default.units="native")
}
# create comparisons grobs
comparison_grob <- function(comparison, ...){
  if (!is.comparison(comparison)) stop("A comparison object is required")
  grob_list <- gList()
  if (nrow(comparison) < 1) return(grob_list)
  for (i in 1:nrow(comparison)){
    grob_list[[i]] <- similarity_grob(comparison[i,], ...)
  }
  grob_list
}
# create annot grob
label_grob <- function(label, cex=0.8){
  y <- 0
  w <- 0.1
  if (!is.annotation(label)) stop("An annotation object is required")
  if (nrow(label) > 1) stop("A single-line annotation is required")
  grob_list <- gList()
  if (!is.na(label$x2)){
    bracket_coord <- bracket_coord(label$x1, label$x2, y=y, w=w)
    grob_list[[2]] <- linesGrob(x=bracket_coord$x, y=bracket_coord$y,
                                default.units="native",
                                gp=gpar(col=label$col))
    x <- mean(c(label$x1, label$x2))
    w <- w*2
  } else {
    x <- label$x1
  }
  if (label$rot == 0){
    just <- c(0.5, 0)
  } else {
    just <- c(0, 0.5)
  }
  grob_list[[1]] <- textGrob(label$text, x=x, y=y+w, just=just,
                             rot=label$rot,
                             default.units="native",
                             gp=gpar(col=label$col, cex=cex))
  grob_list
}
# create annotation grob
annotation_grob <- function(annotation, ...){
  if (!is.annotation(annotation)) stop("An annotation object is required")
  grob_list <- gList()
  if (nrow(annotation) < 1) return(grob_list)
  for (i in 1:nrow(annotation)){
    label <- as.annotation(annotation[i,])
    grob_list[[i]] <- label_grob(label, ...)
  }
  grob_list
}
# create tree grob
dna_seg_label_grob <- function(labels){
  n_label <- length(labels)
  y <- seq(1, 0, len=n_label)
  labelGrobs <- gList()
  for (i in 1:n_label) {
    labelGrobs[[i]] <-
      textGrob(x=0, y=y[i], label=labels[i], just="left",
               default.units="native")
  }
  width <- unit(1, "grobwidth", labelGrobs[[which.max(nchar(labels))]])
  labelTree <- gTree(children=labelGrobs,
                     vp=viewport(xscale=c(0, 1), yscale=c(0, 1),
                       width=width))
  list(grob=labelTree, width=width)
}
# create scale grob
scale_grob <- function(max_length){
    rng <- diff(pretty(c(0, max_length), n=8))[1]
    scale_grob <- gList(segmentsGrob(x0=max_length, y0=0,
                                     x1=max_length-rng, y1=0,
                                     default.units="native"),
                        textGrob(label=human_nt(rng)$text,
                                 x=max_length-rng/2, y=0.5,
                                 default.units="native"))
    scale_grob
}
