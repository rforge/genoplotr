################################################################################
# Plot helpers
################################################################################
# calculate arrow coordinates from gene coordinates
arrow_coord <- function(x1, x2, y1=0.5, strand=NULL, width=1, head_len=100){
  # take care of strand, to get x1 as bottom and x2 as tip of arrow
  if (!is.null(strand) && strand == -1){
    x_temp <- x2
    x2 <- x1
    x1 <- x_temp
  }
  w2 <- width/4
  # if the head of the arrow is larger than half of the gene, reduce to half
  if (head_len > abs(x1-x2)/2){
    head_len <- abs(x1-x2)/2
  }
  # calculate xi, x "internal"
  if (x2 > x1){
    xi <- x2-head_len
  } else {
    xi <- x2+head_len
  }
  list(x=c(x1,    xi,    xi,      x2, xi,      xi,    x1),
       y=c(y1-w2, y1-w2, y1-w2*2, y1, y1+w2*2, y1+w2, y1+w2)
       )
}
# coords for a block
block_coord <- function(start, end, strand, y=0.5){
  x <- c(rep(start, 2), rep(end, 2))
  y <- c(y, y + strand/2, y + strand/2, y)
  list(x=x, y=y)
}
# coords for a zone annotation
bracket_coord <- function(start, end, y=0, w=0.1){
  x <- c(rep(start, 2), rep(end, 2))
  y <- c(y, rep(y+w, 2), y)
  list(x=x, y=y)
}
# human readable coordinates
human_nt <- function(nt, signif=FALSE){
  tag <- "nt"
  mult <- 1
  if (nt >= 1e9){
    nt <- nt/1e9
    tag <- "Gb"
    mult <- 1e9
  } else if (nt >= 1e6){
    nt <- nt/1e6
    tag <- "Mb"
    mult <- 1e6
  } else if (nt >= 1e3){
    nt <- nt/1e3
    tag <- "kb"
    mult <- 1e3
  }
  if (signif) nt <- signif(nt, signif)
  list(n=nt, tag=tag, mult=mult, text=paste(nt, tag))
}
