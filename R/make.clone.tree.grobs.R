
make.clone.tree.grobs <- function(
    ccf.df,
    tree,
    scale1,
    scale2,
    wid, 
    line.lwd,
    length.from.node.edge,
    seg1.col,
    seg2.col,
    cluster.list,
    add.polygons,
    extra.len,
    sig_shape,
    sig_curve,
    spread,
    fixed_angle,
    add.genes,
    genes_df,
    gene_line_dist,
    gene.cex,
    genes_on_nodes,
    yaxis_position,
    yaxis1.label,
    yaxis2.label,
    axis.label.cex,
    axis.cex,
    yaxis1.interval,
    yaxis2.interval, 
    ylimit,
    xaxis.label,
    xaxis.space.left,
    xaxis.space.right,
    min.width,
    nodes,
    rad,
    label.nodes,
    node.col,
    labe.cex,
    add.normal,
    normal.cex,
    title,
    title.cex,
    title.y,
    title.y.units,
    ... 
    ) {

	#initializing dataframe for subclones
	if ('excluded' %in% colnames(ccf.df)) {
		v <- ccf.df[!ccf.df$excluded,];
	} else {
		v <- ccf.df;
		v$excluded <- FALSE;
	    }

	v <- v[order(v$id), ];
	no_ccf <- FALSE;

	if (!('ccf' %in% colnames(ccf.df)) | all(is.na(ccf.df$ccf)) | add.polygons == FALSE) {
	    v$vaf <- NULL;
	    v$vaf[v$parent == -1] <- 1;
	    no_ccf <- TRUE;
	} else {
	    v <- v[order(v$id),]
	    v$vaf[!v$excluded] <- v$ccf[!v$excluded]/max(v$ccf[!v$excluded])
	    }

	if (all(is.null(ccf.df$colour))) {
	    v$colour <- node.col
	    }
	
	v$x <- v$y <- v$len <- v$x.mid <- numeric(length(nrow(v)));

	v <- get_num_tiers(v);
	v <- v[order(v$tier,v$parent), ];

	#initializing line segment dataframe and adjusting lengths to accomodate the node circles
	tree$angle <- numeric(length=nrow(tree));
	tree$angle[tree$parent == -1] <- 0;
	if ('length2' %in% colnames(tree)) {
        tree$length2.c <- tree$length2 / scale1 * scale2;
        
        tree$length <- apply(
            tree,
            MARGIN = 1,
            FUN = function(x) { 
                max(x[c(3, 6)])
                }
            );
	} else {
	    tree$length <- tree$length1;
	    }

	raw_tree <- tree;

	if (nodes != "none" && length.from.node.edge == TRUE) {
	    tree <- adjust_branch_lengths(v,tree,rad, scale1);
    	}

	extra.len <- extra.len * (1 / scale1);
	
	clone.out <- make_clone_polygons(
	    v,
	    tree,
	    wid,
	    scale1,
	    scale2,
	    extra.len,
	    node.col,
	    spread = spread,
	    sig_shape = sig_shape,
	    fixed_angle = fixed_angle,
	    beta_in = sig_curve,
	    add.polygons = add.polygons,
	    no_ccf = no_ccf
	    );

	clone.out$no_ccf <- no_ccf;
	plot_size <- calculate_main_plot_size(
	    clone.out,
	    scale1,
	    wid,
	    min.width,
	    xaxis.space.left,
	    xaxis.space.right,
	    rad
	    );

	if (!no_ccf) {
		get.CP.polygons(clone.out);
	}

	add_tree_segs(clone.out, rad, line.lwd, scale1, seg1.col, seg2.col);

	if (!is.null(cluster.list)) {
		add_pie_nodes(clone.out, rad, cluster.list)
	} else {
		add_node_ellipse(clone.out,rad, label.nodes, labe.cex, scale1)
	    }

	if (add.normal == TRUE) {
		add.normal(clone.out,rad,labe.cex, normal.cex)
	    }

	if (yaxis_position != "none" ) {
		add_axes(
		    clone.out,
		    yaxis_position,
		    scale1 = scale1,
		    scale2 = scale2,
		    axis.label.cex = axis.label.cex,
		    axis.cex = axis.cex,
		    no_ccf = no_ccf,
		    xaxis.label = xaxis.label,
		    yaxis1.label = yaxis1.label,
		    yaxis2.label = yaxis2.label,
		    yaxis1.interval = yaxis1.interval,
		    yaxis2.interval = yaxis2.interval,
		    ylimit = ylimit
		    );
	    }

	if (add.genes == TRUE & !is.null(genes_df)) {			
	    gene_grobs <- add_text2(
	        clone.out$tree,
	        genes_df,
	        label.nodes = genes_on_nodes,
	        line.dist = gene_line_dist,
	        title.y = clone.out$height,
	        panel_height = clone.out$height,
	        panel_width = clone.out$width,
	        xlims = clone.out$xlims,
	        ymax = clone.out$ymax,
	        cex = gene.cex,
	        v = clone.out$v,
	        axis.type = yaxis_position,
	        rad = rad,
	        scale = scale1,
	        clone.out = clone.out,
	        alternating = FALSE
	        );

	    clone.out$grobs <- c(clone.out$grobs, list(gene_grobs));
	    }

	if (!is.null(title)) {
		add_title(clone.out, title, title.cex, title.y, title.y.units);
    	}

	return(clone.out);
    }
