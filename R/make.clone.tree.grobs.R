
make.clone.tree.grobs <- function(
    ccf_df,
    tree,
    scale1,
    scale2,
    wid, 
    line.lwd,
    length_from_node_edge,
    seg1.col,
    seg2.col,
    cluster_list,
    add_polygons,
    extra_len,
    sig_shape,
    sig_curve,
    spread,
    fixed_angle,
    add_genes,
    genes_df,
    gene_line_dist,
    gene.cex,
    genes_on_nodes,
    yaxis_position,
    yaxis1_label,
    yaxis2_label,
    axis_label_cex,
    axis_cex,
    yaxis1_interval,
    yaxis2_interval, 
    ylimit,
    xaxis_label,
    xaxis_space_left,
    xaxis_space_right,
    min_width,
    nodes,
    rad,
    label_nodes,
    node_col,
    label_cex,
    add_normal,
    normal_cex,
    title,
    title.cex,
    title.y,
    title.y.units,
    ... 
    ) {

	#initializing dataframe for subclones
	if ('excluded' %in% colnames(ccf_df)) {
		v <- ccf_df[!ccf_df$excluded,];
	} else {
		v <- ccf_df;
		v$excluded <- FALSE;
	    }

	v <- v[order(v$lab), ];
	no_ccf <- FALSE;

	if (!('ccf' %in% colnames(ccf_df)) | all(is.na(ccf_df$ccf)) | add_polygons == FALSE) {
	    v$vaf <- NULL;
	    v$vaf[v$parent == -1] <- 1;
	    no_ccf <- TRUE;
	} else {
	    v <- v[order(v$lab),]
	    v$vaf[!v$excluded] <- v$ccf[!v$excluded]/max(v$ccf[!v$excluded])
	    }

	if (all(is.null(ccf_df$colour))) {
	    v$colour <- node_col
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

	if (nodes != "none" && length_from_node_edge == TRUE) {
	    tree <- adjust_branch_lengths(v,tree,rad, scale1);
    	}

	extra_len <- extra_len * (1 / scale1);
	
	clone.out <- make_clone_polygons(
	    v,
	    tree,
	    wid,
	    scale1,
	    scale2,
	    extra_len,
	    node_col,
	    spread = spread,
	    sig_shape = sig_shape,
	    fixed_angle = fixed_angle,
	    beta_in = sig_curve,
	    add_polygons = add_polygons,
	    no_ccf = no_ccf
	    );

	clone.out$no_ccf <- no_ccf;
	plot_size <- calculate_main_plot_size(
	    clone.out,
	    scale1,
	    wid,
	    min_width,
	    xaxis_space_left,
	    xaxis_space_right,
	    rad
	    );

	if (!no_ccf) {
		get.CP.polygons(clone.out);
	}

	add_tree_segs(clone.out, rad, line.lwd, scale1, seg1.col, seg2.col);

	if (!is.null(cluster_list)) {
		add_pie_nodes(clone.out, rad, cluster_list)
	} else {
		add_node_ellipse(clone.out,rad, label_nodes, label_cex, scale1)
	    }

	if (add_normal == TRUE) {
		add_normal(clone.out,rad,label_cex, normal_cex)
	    }

	if (yaxis_position != "none" ) {
		add_axes(
		    clone.out,
		    yaxis_position,
		    scale1 = scale1,
		    scale2 = scale2,
		    axis_label_cex = axis_label_cex,
		    axis_cex = axis_cex,
		    no_ccf = no_ccf,
		    xaxis_label = xaxis_label,
		    yaxis1_label = yaxis1_label,
		    yaxis2_label = yaxis2_label,
		    yaxis1_interval = yaxis1_interval,
		    yaxis2_interval = yaxis2_interval,
		    ylimit = ylimit
		    );
	    }

	if (add_genes == TRUE & !is.null(genes_df)) {			
	    gene_grobs <- add_text2(
	        clone.out$tree,
	        genes_df,
	        label_nodes = genes_on_nodes,
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