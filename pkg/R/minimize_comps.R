################################################################################
# Minimize comparison sizes
################################################################################
# from a set of comparisons and lengths, determine the best possible
# arrangement of maps
minimize_comps <- function(comparisons, lengths, x0){
  # function to minimize. Calculates the mean of the absolute differences
  # between starts and ends for direct hits only
  mean_w_offset <- function(offset, s_ref, e_ref, s, e){
    direction <- sign(e_ref-s_ref)*sign(e-s)
    dists <- c(abs(s_ref-(s+offset)),
             abs(e_ref-(e+offset)))[direction > 0]
    mean(dists)
  }
  n_org <- length(lengths)
  offsets <- rep(0, n_org)
  if (length(comparisons) < 1) return(offsets)
  idx_ref <- which.max(lengths)
  max_len <- max(lengths)
  # go up from ref
  if (idx_ref > 1){
    # comp i is between org i and i+1
    for (i in (idx_ref-1):1){
      comp <- comparisons[[i]]
      # optimise
      opt <- optimise(f=mean_w_offset, interval=c(0, max_len-lengths[i]+1),
                      comp$start2+offsets[i+1]-x0[i+1],
                      comp$end2+offsets[i+1]-x0[i+1],
                      comp$start1+offsets[i]-x0[i],
                      comp$end1+offsets[i]-x0[i])
      offsets[i] <- opt$min
    }
  }
  # go down
  if (idx_ref < n_org){
    for (i in idx_ref:(n_org-1)){
      comp <- comparisons[[i]]
      # optimise
      opt <- optimize(f=mean_w_offset, interval=c(0, max_len-lengths[i+1]+1),
                      comp$start1+offsets[i]-x0[i],
                      comp$end1+offsets[i]-x0[i],
                      comp$start2+offsets[i+1]-x0[i+1],
                      comp$end2+offsets[i+1]-x0[i+1])
      offsets[i+1] <- opt$min
    }
  }
  offsets
}
