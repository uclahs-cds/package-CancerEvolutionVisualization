
make_clone_polygons <- function(v, tree, wid, scale1, scale2, extra_len, node_col, sig_shape=3, fixed_angle=NULL, beta_in=3, add_polygons=TRUE, no_ccf=FALSE){
  	#if the tree is linear the width is fixed
	branching <- ifelse(any(duplicated(tree$parent)== TRUE),TRUE,FALSE)
	lin.width <- wid/3

  	if(branching==FALSE){
  	  wid <-(lin.width)*1/scale1
  	  len <- extra_len + sum(tree$length1[-1])
  	}else{
  	  wid <- wid*1/scale1
  	  len <- extra_len 
  	}
  	clone_out <- compute_clones(v=v, wid=wid, tree=tree, extra_len=extra_len, x=0, y=tree$length[tree$parent==-1], fixed_angle=fixed_angle, sig_shape=sig_shape, beta_in=beta_in, branching=branching, no_ccf=no_ccf, spread=spread)
  	return(clone_out)  	
}


make_clone_tree_grobs <- function(ccf_df, tree,  scale1, scale2, wid, 
								  line.lwd, length_from_node_edge, seg1.col, seg2.col, 
								  add_polygons, extra_len, sig_shape, sig_curve, spread, fixed_angle,
								  add_genes, genes_df, gene_line_dist, gene_cex, genes_on_nodes,
								  yaxis_position, yaxis1_label, yaxis2_label, w_padding, h_padding, axis_label_cex, axis_cex, yaxis1_interval, yaxis2_interval,  yaxis1_max, yaxis2_max, xaxis_label, xaxis_space_left, xaxis_space_right, min_width,
								  nodes, rad, label_nodes,  node_col, label_cex, 
								  add_normal, title, title.cex, title.y,... ){

	#initializing dataframe for subclones
	no_ccf <- FALSE
	v <- ccf_df[!ccf_df$excluded,]
	v <- v[order(v$lab),]
	if(all(is.null(ccf_df$ccf)) | add_polygons == FALSE){
	  v$vaf <- NULL
	  v$vaf[v$parent == -1] <- 1
	  no_ccf <- TRUE
	} else{
	  v <- ccf_df[!ccf_df$excluded,]
	  v <- v[order(v$lab),]
	  v$vaf[!v$excluded] <- v$ccf[!v$excluded]/max(v$ccf[!v$excluded])
	}

	if(all(is.null(ccf_df$c.col))){
	  v$c.col <- node_col
	}
	v$x <- numeric(length(nrow(v)))
	v$y <- numeric(length(nrow(v)))
	v$len <- numeric(length(nrow(v)))
	v$x.mid <- numeric(length(nrow(v)))
	
	v <- get_num_tiers(v)
	v <- v[order(v$tier,v$parent),]

	#initializing line segment dataframe and adjusting lengths to accomodate the node circles
	tree$angle <- numeric(length=nrow(tree))
	tree$angle[tree$parent==-1] <- 0
	if('length2' %in% colnames(tree)){
	  tree$length2.c <- tree$length2/scale1*scale2
	  tree$length <- apply(tree, 1,function(x) max(x[c(3,6)]))
	} else{
	  tree$length <- tree$length1
	}

	raw_tree <- tree
	print("raw_tree")
	print(raw_tree)
	if(nodes != "none" && length_from_node_edge==TRUE){
	  tree <- adjust_branch_lengths(v,tree,rad, scale1)  
  print("adjusted tree")
  print(tree)
	}
	
	clone.out <- make_clone_polygons(v, tree, wid, scale1, scale2,extra_len, node_col, sig_shape=sig_shape, fixed_angle=fixed_angle, beta_in=sig_curve, add_polygons= add_polygons, no_ccf=no_ccf)
	plot_size <- calculate_main_plot_size(clone.out, scale1, wid, min_width, xaxis_space_left, xaxis_space_right)
	# browser()
	add_clone_grobs(clone.out) 
	add_tree_segs(clone.out, rad, line.lwd, scale1, seg1.col, seg2.col)

	add.node.ellipse(clone.out,rad, label_nodes, label_cex, scale1)
	add_normal(clone.out,rad,label_cex)
	print(length(clone.out$grobs))
	add_axes(clone.out, yaxis_position, scale1=scale1, scale2=scale2,  axis_label_cex=axis_label_cex, axis_cex=axis_cex, no_ccf=no_ccf, xaxis_label=xaxis_label,  yaxis1_label=yaxis1_label, yaxis2_label=yaxis2_label, yaxis1_interval=yaxis1_interval, yaxis2_interval=yaxis2_interval, ...)
	print(length(clone.out$grobs))

	return(clone.out)
}

package_clone_grobs <- function(clone.out){
	grob_list <- do.call(gList, clone.out$grobs)
	return(grob_list)
}

SRCGrob <- function(  ccf_df, tree_df, filename="SRC_tree.pdf", scale1=0.05443424, scale2=0.5/362, wid=1.2, 
					  line.lwd=3, length_from_node_edge=TRUE, seg1.col="black", seg2.col="green", 
					  add_polygons=TRUE, extra_len=10, sig_shape=3, sig_curve=3, spread=1, fixed_angle=NULL,
					  genes_df=NULL, genes="default", gene_line_dist=NULL, gene.cex=0.85, add_genes=FALSE, genes_on_nodes=FALSE,
					  yaxis_position="left", yaxis1_label="SNVs", yaxis2_label=NULL, w_padding=0.3, h_padding=0.3, axis_label_cex=list(x=1.55, y=1.55), axis_cex=list(x=1.45, y=1.45), yaxis1_interval=NA, yaxis2_interval=NA,  yaxis1_max=NULL, yaxis2_max=NULL, xaxis_label=NULL, xaxis_space_left=0, xaxis_space_right=0, min_width=NULL,
					  nodes="circle", rad=0.1, label_nodes=TRUE,  node_col="grey29",  label_cex=NA, 
				  	  add_normal=FALSE, title=NULL, title.cex=1.7, title.y=0.3, ...){


	clone.out <- make_clone_tree_grobs(ccf_df=ccf_df, tree=tree_df, genes_df=genes_df,rad=rad, scale1=scale1, scale2=scale2, wid=wid, 
												  line.lwd=line.lwd, length_from_node_edge=length_from_node_edge, seg1.col=seg1.col, seg2.col=seg2.col, 
												  add_polygons=add_polygons, extra_len=extra_len, sig_shape=sig_shape, sig_curve=sig_curve, spread=spread,fixed_angle=fixed_angle,
												  add_genes=add_genes, genes_on_nodes=genes_on_nodes, gene_line_dist=gene_line_dist, gene_cex=gene_cex,
												  yaxis_position=yaxis_position, yaxis1_label=yaxis1_label, yaxis2_label=yaxis2_label, w_padding=w_padding, h_padding=h_padding, axis_label_cex=axis_label_cex, axis_cex=axis_cex, yaxis1_interval=yaxis1_interval, yaxis2_interval=yaxis2_interval,  yaxis1_max=yaxis1_max, yaxis2_max=yaxis2_max, xaxis_label=xaxis_label, xaxis_space_left=xaxis_space_left, xaxis_space_right=xaxis_space_right, min_width=min_width,
												  nodes=nodes, label_nodes=label_nodes,  node_col=node_col, label_cex=label_cex, 
												  add_normal=add_normal, title=title, title.cex=title.cex, title.y=title.y, ...
												  )			
	# return(clone.out)
	out_tree <- gTree(children = package_clone_grobs(clone.out),
		  	vp = clone.out$vp,
			cl = "SRCGrob")

	return(list(out_tree, clone.out))
}