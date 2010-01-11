###############################################################################
# PLOT GENE MAPS                                                              #
###############################################################################
# A R framework to plot comparison of gene stretches, à la Artemis, but
# with production-like graphics, and a static interface
################################################################################
# Plotting function
################################################################################
# plot frame, makes space for tree and legend if necessary
plot_gene_map <- function(dna_segs,
                          comparisons=NULL,
                          tree=NULL,
                          tree_width=NULL, # in inches
                          legend=NULL, # unimplemented
                          annotation=NULL,
                          annotation_height=1, # height of annot line
                          annotation_cex=0.8, # size of annotations
                          xlims=NULL,
                          offsets=NULL, # regulates manually alignment of segs
                          main=NULL, # main title
                          main_pos="centre", # centre, left, right
                          dna_seg_labels=NULL,
                          gene_type=NULL, # if not null, resets gene_type
                          scale=TRUE,
                          plot_new=TRUE,
                          debug=0){
  ### check arguments ###
  # dna_segs
  if (missing(dna_segs)) stop("Argument dna_segs must be provided")
  if (!is.list(dna_segs) || !all(sapply(dna_segs, is.dna_seg)))
    stop("Argument dna_segs must be a list of dna_seg objects")
  n_dna_segs <- length(dna_segs)
  # comparisons
  n_comparisons <- length(comparisons)
  if (n_comparisons > 1){
    if (!is.list(comparisons) || !all(sapply(comparisons, is.comparison)))
      stop("Argument comparisons must be a list of comparison objects")
  }
  # check that there are enough comparisons compared to dna segments
  if (n_comparisons > 0 && !(n_dna_segs - n_comparisons == 1))
    stop("Number of comparisons not correct")
  # if dna_seg is a named list, attribute names to dna_seg_labels
  if (is.null(dna_seg_labels) && !is.null(dna_segs)){
    dna_seg_labels <- names(dna_segs)
  }
  # check length of labels
  if (!is.null(dna_seg_labels) && !(length(dna_seg_labels) == n_dna_segs))
    stop("Argument dna_seg_labels doesn't have the same length as dna_segs")
  
  # check tree
  if (!is.null(tree)){
    if (!inherits(tree, "phylog"))
      stop("Argument tree should be of class phylog (ade4)")    
    # check correspondence between names (given by names(comparison) or
    # dna_seg_labels) and tree leaves
    if (is.null(dna_seg_labels))
      stop("If tree is given, label names should be provided via named list dna_segs or dna_seg_labels")
    # check that number of leaves corresponds to number of segs
    if (length(tree$leaves) != n_dna_segs)
      stop("Number of leaves in the tree not equal to number of dna segs")
    if (!all(dna_seg_labels %in% names(tree$leaves)))
      stop("Tree leaves not corresponding to dna_seg labels")
  }
  
  # check xlims
  if (!is.null(xlims)){
    if (nrow(xlims) != n_dna_segs){
      stop("Number of rows of xlims not equal to number of dna_segs")
    }
    if (!all(names(xlims) %in% c("x0","x1"))){
      stop("xlims should have x and y columns")
    }
    if (!is.numeric(xlims$x0) && !is.numeric(xlims$x1)){
      stop("xlims columns x and y must be numeric")
    }
  }
  # check offsets
  if (!is.null(offsets) && length(offsets) != n_dna_segs){
    stop("Length of offsets not equal to number of dna_segs")
  }
  
  # check main_pos
  if (main_pos == "centre"){
    main_x <- 0.5
    main_just <- "centre"
  } else if (main_pos == "left"){
    main_x <- 0
    main_just <- "left"
  } else if (main_pos == "right"){
    main_x <- 1
    main_just <- "right"
  } else {
    stop("main_pos should be one of centre, left, right")
  }
  
  
  # check gene_type
  if (!is.null(gene_type) && !(gene_type %in% gene_types()))
    stop(paste("gene_type muste be one of:",
               paste(gene_types(), collapse=", ")))
         
  ### prepare plotting frame ###
  frame_w <- c(0,1,0)
  # calculate beginning of plotting and set sensible offsets
  seg_x0 <- sapply(dna_segs, function(x) range(x)[1])
  seg_x1 <- sapply(dna_segs, function(x) range(x)[2])
  seg_rng <- sapply(dna_segs, function(x) diff(range(x)))
  # xlims & lengths
  if (is.null(xlims)){
    xlims <- data.frame(x0=seg_x0, x1=seg_x1)
    for (i in 1:n_dna_segs){
      xlims$x0[i] <- seg_x0[i] - 0.02*seg_rng[i]
      xlims$x1[i] <- seg_x1[i] + 0.02*seg_rng[i]
    }
  }
  seg_lengths <- xlims$x1 - xlims$x0
  max_length <- max(seg_lengths)
  
  ### filter objects ###
  if (!is.null(xlims)){
    for (i in 1:n_dna_segs){
      dna_segs[[i]] <- trim.dna_seg(dna_segs[[i]], as.numeric(xlims[i,]))
    }
    if (n_comparisons > 0){
      for (i in 1:n_comparisons){
        comparisons[[i]] <- trim.comparison(comparisons[[i]],
                                            xlim1=as.numeric(xlims[i,]),
                                            xlim2=as.numeric(xlims[i+1,]))
      }
    }
  }

  ### plotting options ###
  # offsets
  if (is.null(offsets)) {
    offsets <- minimize_comps(comparisons, seg_lengths, xlims[,1])
  } else {
    if (!length(offsets) == n_dna_segs)
      stop("Length of offsets not equal to dna_segs") 
  }
  
  # deal with resetting symbols
  if (!is.null(gene_type)){
    if (gene_type == "auto"){
      n_genes <- sapply(dna_segs, nrow)
      gene_type <- auto_gene_type(n_genes)
    }
    for (i in 1:n_dna_segs){
      dna_segs[[i]]$gene_type <- gene_type
    }
  }
  
  ### collect grobs ###
  # collect dna_seg grobs
  dna_seg_grobs <- list()
  for (i in 1:n_dna_segs){
    # debug
    if (debug > 0 && debug < nrow(dna_segs[[i]]))
      dna_segs[[i]] <- dna_segs[[i]][1:debug,]
    # end debug
    dna_seg_grobs[[i]] <- dna_seg_grob(dna_segs[[i]])
  }
  # collect comparison grobs
  comparison_grobs <- list()
  if (n_comparisons > 0){
    for (i in 1:n_comparisons){
      # debug
      if (debug > 0 && debug < nrow(comparisons[[i]]))
        comparisons[[i]] <- comparisons[[i]][1:debug,]
      # end debug
      comparison_grobs[[i]] <- comparison_grob(comparisons[[i]],
                                               offsets[i], offsets[i+1],
                                               xlim1=as.numeric(xlims[i,]),
                                               xlim2=as.numeric(xlims[i+1,]))
    }
  }

  ### annotation ###
  if (!is.null(annotation)){
    annotation_grob <- annotation_grob(annotation, annotation_cex)
    annot_h <- annotation_height
  } else {
    annot_h <- 0
  }

  ### scale ###
  if (scale){
    scale_grob <- scale_grob(max_length)
    scale_h <- 1
  } else {
    scale_h <- 0
  }

  ### main title ###
  if (!is.null(main)){
    main_grob <- textGrob(x=main_x, label=main,
                          gp=gpar(cex=1.2), just=main_just)
    main_h <- 1.8
  } else {
    main_h <- 0
  }

  ### tree ###
  if (!is.null(tree)){
    # tree
    #phylog <- newick2phylog(tree)
    # check that a nice permutation is OK, return ys
    y <- permute_tree(tree, dna_seg_labels)
    # feed tree grob with permutation transformed as y coords
    tree_grob <- phylog_grob(tree, 1-((y-1)/(n_dna_segs-1)))
    #tree_w <- unit(0.20, "npc")
    tree_w <- unit(0.1, "npc") + tree_grob$width
  } else if(!is.null(dna_seg_labels)){
    # just labels
    tree_grob <- dna_seg_label_grob(dna_seg_labels)
    tree_w <- tree_grob$width
  } else {
    # nothing
    tree_grob <- NULL
    tree_w <- unit(0, "npc")
  }
  if (!is.null(tree_width)) tree_w <- unit(tree_width, "inches")
  
  ### plotting ###
  # overall frame
  if (plot_new) grid.newpage()
  pushViewport(viewport(width=unit(1, "npc")-unit(1, "lines"),
                        height=unit(1, "npc")-unit(1, "lines"),
                        name="oma"),
               viewport(layout=grid.layout(2, 1,
                          heights=unit(c(main_h, 1),
                            c("lines", "null"))),
                        name="oma_layout"))
  # main title
  if (!is.null(main)) {
    pushViewport(viewport(layout.pos.row=1, name="main"))
    grid.draw(main_grob)
    upViewport()
  }

  # frame
  pushViewport(viewport(layout.pos.row=2,
                        layout=grid.layout(3, 3,
                          heights=unit(c(annot_h, 1, scale_h),
                            c("lines", "null", "lines")),
                          widths=unit.c(tree_w,
                            unit(c(1, 0), c("null", "null")))),
                        name="frame"))
  # annotation
  if (!is.null(annotation)){
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=2),
                 viewport(width=unit(1, "npc")-unit(1, "lines"),
                          xscale=c(xlims[1,1]-offsets[1],
                            xlims[1,1]+max_length-offsets[1]),
                          name="annotation"))
    grid.draw(annotation_grob)
    upViewport(2)    
  }
  # scale
  if (scale) {
    pushViewport(viewport(layout.pos.row=3, layout.pos.col=2,
                          xscale=c(0, max_length)))
    grid.draw(scale_grob)
    upViewport()
  }

  # tree or labels. Height is 1-3 lines because margin is 2 in plotarea,
  # and 1 to center to the middle of each dna_seg (1/2 line top and bottom)
  if (!is.null(tree_grob)){
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=1),
                 viewport(width=unit(1, "npc")-unit(1, "lines"),
                          height=unit(1, "npc")-unit(1, "lines"),
                          name="tree"))
    grid.draw(tree_grob$grob)
    upViewport(2)    
  } 
  
  # plotting area
  pushViewport(viewport(layout.pos.row=2, layout.pos.col=2),
               viewport(width=unit(1, "npc")-unit(1, "lines"),
                        height=unit(1, "npc")-unit(0, "lines"),
                        name="plotarea"))

  # map grid
  pushViewport(viewport(layout=grid.layout(2*n_dna_segs - 1, 1,
                          heights=unit(rep(1, 2*n_dna_segs - 1),
                            c(rep(c("lines", "null"), n_dna_segs -1), "lines"))
                          ),
                        name="map")) 
  ### dna_segs ###
  for (i in 1:n_dna_segs){
    pushViewport(viewport(layout.pos.row=2*i-1,
                          yscale=c(0,1),
                          xscale=c(xlims[i,1]-offsets[i],
                            xlims[i,1]+max_length-offsets[i]),
                          name = paste("dna_seg", i, sep="_")))
    # draw segment line
    grid.segments(x0=unit(xlims$x0[i], "native"),
                  y0=unit(0.5, "native"),
                  x1=unit(xlims$x1[i], "native"),
                  y1=unit(0.5, "native"),
                  gp=gpar(col="black"))
    #grid.xaxis()
    # draw dna_seg grobs
    grid.draw(dna_seg_grobs[[i]])
    upViewport() # pop dna_segs[i] vp
  }
  ### comparisons ###
  if (n_comparisons > 0){
    for (i in 1:n_comparisons){
      pushViewport(viewport(layout.pos.row=2*i,
                            yscale=c(0,1),
                            xscale=c(0, max_length),
                            name = paste("comparison", i, sep="_")))
      # draw comparison grobs
      grid.draw(comparison_grobs[[i]])
      upViewport() # pop comparisons[[i]] vp
    }
  }
  
  upViewport(2) # pop map viewports
  upViewport(2) # pop plotarea viewport
  upViewport(2) # pop frame+oma viewport
}
