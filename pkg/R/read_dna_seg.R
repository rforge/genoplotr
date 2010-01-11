################################################################################
# File reading functions: read dna_seg
################################################################################
# reading genes from a file. Use source=tab or ptt to specify type
read_dna_seg_from_ptt <- function(file, meta_lines=2, header=TRUE, ...){
  # reads meta info
  seg_name <- readLines(file, n=1)
  seg_name <- strsplit(seg_name, "/,|-/", fixed=TRUE)[[1]][1]
  # reads ptt table
  ptt <- read.table(file, skip=meta_lines, as.is=TRUE, header=header,
                    sep="\t", quote="")
  if (header){
    names(ptt) <- tolower(names(ptt))
  }
  else {
    names(ptt) <- c("location", "strand", "length", "pid", "gene",
                    "synonym", "code", "cog", "product")
  }
  # parse location
  location <- strsplit(ptt$location, "..", fixed=TRUE)
  start <- as.numeric(sapply(location, function(x) x[[1]]))
  end <- as.numeric(sapply(location, function(x) x[[2]]))
  # parse strand
  strand <- ptt$strand
  strand[strand=="-"] <- -1
  strand[strand=="+"] <- 1
  strand <- as.numeric(strand)
  # parse gene name from name or synonym if not present
  name <- ifelse(ptt$gene == "-", ptt$synonym, ptt$gene)
  table <- data.frame(name=name, start=start, end=end, strand=strand,
                      length=ptt$length, pid=ptt$pid, gene=ptt$gene,
                      synonym=ptt$synonym, code=ptt$code, cog=ptt$cog,
                      product=ptt$product,
                      stringsAsFactors=FALSE)
  .read_dna_seg(table, seg_name, ...)
}
read_dna_seg_from_tab <- function(file, header=TRUE, ...) {
  table <- read.table(file, as.is=TRUE, header=header, sep="\t", quote="")
  if (ncol(table) < 4) stop("Insufficent number of columns in table")
  col_names <-  c("name", "start", "end", "strand")
  names(table)[1:length(col_names)] <- col_names
  # parse name from file name by default
  seg_name <- basename(file)
  .read_dna_seg(table, seg_name, ...)
}
.read_dna_seg <- function(table, seg_name, reverse=FALSE, xlim=NULL, ...){
  # check args
  if (ncol(table) < 4) stop("Insufficent number of columns in table")
  if (nrow(table) < 1) stop("No lines in table")
  col_names <-  c("name", "start", "end", "strand")
  if (!all(col_names %in% names(table)))
    stop("Table should contain at least columns name, start, end and strand")
  # make dna_seg object, set seg_name attribute
  dna_seg <- as.dna_seg(table, ...)
  dna_seg <- trim.dna_seg(dna_seg, xlim)
  if (reverse) dna_seg <- reverse.dna_seg(dna_seg)
  attr(dna_seg, "seg_name") <- seg_name
  dna_seg
}
