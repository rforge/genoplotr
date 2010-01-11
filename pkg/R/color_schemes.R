################################################################################
# Color schemes
################################################################################
# apply color scheme to a numeric vector
apply_color_scheme <- function(x, direction=NULL, color_scheme="grey"){
  # check arguments
  # if x is null and direction is not, get x to 1s (mainly for blue/red)
  if (is.null(x) && !is.null(direction)) {
    x <- rep(1, length(direction))
  }
  if (!is.numeric(x)) stop("Color column is not numeric")
  rng <- range(x)
  col <- rep(grey(0.5), length(x))
  # red blue
  if (any(color_scheme %in% c("red_blue", "blue_red"))){
    if (is.null(direction)) direction <- rep(1, length(x))
    blues <- brewer.pal(9, "Blues")
    reds  <- brewer.pal(9, "Reds")
    # case: only one value:
    if (diff(rng) == 0){
      level <- rep(9, length(x))
    } else { # case: several values
      level <- round(((x-rng[1])/diff(rng))*8+1)
    }
    col[direction==1] <- reds[level[direction==1]] 
    col[direction==-1] <- blues[level[direction==-1]]
  }
  # grey
  else if (any(color_scheme %in% c("grey", "gray", "grays", "greys")))
    # case: only one value:
    if (diff(rng) == 0){
      col <- rep(grey(0.5), length(x))
    } else {
      col <- grey(0.75-((x-rng[1])/diff(rng))/2)
    }
  else {
    stop("Color scheme name invalid, choose between red_blue or grey")
  }
  col
}
