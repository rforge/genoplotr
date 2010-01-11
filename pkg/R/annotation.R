################################
# Annotation class and methods
################################
# annotation is a set of text and one or two positions for each
# if one, other must be NA
annotation <- function(x1, x2=NA, text, rot=0, col="black"){
  if (missing(x1) | missing(text)) stop("Args x1 and text must be provided")
  if (!is.numeric(x1)) stop("x1 must be numeric")
  if (!is.na(x2) && !is.numeric(x2)) stop("x2 must be numeric")
  if (!is.character(text)) stop("text must be character")
  as.annotation(data.frame(x1=x1, x2=x2, text=text, stringsAsFactors=FALSE),
                rot=rot, col=col)
}
as.annotation <- function(df, x2=NA, rot=0, col="black"){
  if (is.annotation(df)) return(df)
  if (!all(c("x1", "text") %in% names(df)))
    stop("Data frame should have at least a x1 and text column")
  ## if (!is.numeric(df[[1]])) stop("First column of df must be numeric")
  ## x1 <- df[[1]]
  ## if (ncol(df) == 2){
  ##   if (!is.character(df[[2]]))
  ##     stop("With two columns, second one must be character")
  ##   x2 <- NA
  ##   text=df[[2]]
  ## }
  ## else {
  ##   if (is.numeric(df[[2]])){
  ##     x2 <- df[[2]]
  ##     if (!is.character(df[[3]])) stop("Third column must be character")
  ##     text <- df[[3]]
  ##   }
  ##   else {
  ##     x2 <- NA
  ##     if (!is.character(df[[3]])) stop("Second column must be character")
  ##     text <- df[[3]]
  ##   }
  ## }
  # attributes x2, col and arg to all rows if not defined
  if (is.null(df$x2)) df$x2 <- x2
  if (is.null(df$color)) df$color <- col
  if (is.null(df$rot)) df$rot <- rot
  class(df) <- c("annotation", "data.frame")
  df
}
is.annotation <- function(annotation){
  inherits(annotation, "annotation")  
}
