################################################################################
# File reading functions: read comparison
################################################################################
# read comparison file. Use source=tab or blast to specify type
read_comparison_from_blast <- function(file, color_from="per_id",
                                       color_scheme="red_blue", ...){
  # read table anyway
  table <- read.table(file, as.is=TRUE, header=FALSE, sep="\t", quote="")
  # from blast output
  col_names <- c("name1", "name2", "per_id", "aln_len", "mism", "gaps",
                 "start1", "end1", "start2", "end2", "e-value", "bit_score")
  # check col column
  if (!color_from %in% col_names)
    stop("Argument color_from not found in column names")
  names(table) <- col_names
  table$col <- table[[color_from]]
  if (!is.numeric(table$col)) stop("Column color_from not numeric")
  # reorder from weakest to strongest
  table <- table[order(table$col),]
  table <- table[,c(col_names[7:10], "col", col_names[c(1:6,11:12)])]
  .read_comparison(table, color_scheme=color_scheme, ...)
}
read_comparison_from_tab <- function(file, header=TRUE, ...){
  col_names <-  c("start1", "end1", "start2", "end2", "col")
  # from tabular data
  table <- read.table(file, as.is=TRUE, header=header, sep="\t", quote="")
  if (ncol(table) < 4) {
    stop("Insufficent number of columns in table")
  } else if (!header){
    if (ncol(table) == 4) {
      names(table) <- col_names[1:4]
    } else {
      names(table)[1:length(col_names)] <- col_names[1:length(col_names)]
    }
  }
  .read_comparison(table, ...)
}
.read_comparison <- function(table, source="tab", color_scheme=NULL,
                             filt_low=NULL, filt_len=NULL,
                             reverse=0, ...){
  # check arguments
  if (ncol(table) < 4) stop("Insufficent number of columns in table")
  if (!all(c("start1", "end1", "start2", "end2") %in% names(table)))
    stop("Table must contain columns start1, end1, start2, end2")
  
  # filter strength
  if (!is.null(filt_low)){
    if (!is.numeric(filt_low)) stop("filt_low must be numeric")
    if (!is.numeric(table$col)) stop("Cannot filter on non-numeric col data")
    table <- table[table$col >= filt_low,]
  }
  # filter length
  if (!is.null(filt_len)){
    if (!is.numeric(filt_len)) stop("filt_len must be numeric")
    av_len <- apply(cbind(abs(table$end1-table$start1),
                          abs(table$end2-table$start2)), 1, mean)
    table <- table[av_len >= filt_len,]
  }
  # reverse
  if (is.numeric(reverse) && reverse > 0){
    table <- reverse.comparison(table, side=reverse)
  }
  # make comparison object
  comparison <- as.comparison(table, ...)
  # colors
  if (!is.null(color_scheme))
    comparison$col <- apply_color_scheme(x=table$col,
                                         direction=comparison$direction,
                                         color_scheme=color_scheme)
  comparison
}
