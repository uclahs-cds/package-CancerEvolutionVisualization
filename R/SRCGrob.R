SRCGrob <- function(  ccf_df, tree_df, filename="SRC_tree.pdf", scale1=0.05443424, scale2=0.5/362, wid=1.2, 
                      line.lwd=3, length_from_node_edge=TRUE, seg1.col="black", seg2.col="green", 
                      add_polygons=TRUE, extra_len=10, sig_shape=3, sig_curve=3, spread=1, fixed_angle=NULL,
                      genes_df=NULL, genes="default", gene_line_dist=0.1, gene.cex=0.85, add_genes=FALSE, genes_on_nodes=FALSE,
                      yaxis_position="left", yaxis1_label="SNVs", yaxis2_label=NULL,  axis_label_cex=list(x=1.55, y=1.55), axis_cex=list(x=1.45, y=1.45), yaxis1_interval=NA, yaxis2_interval=NA, ylimit=NULL, xaxis_label=NULL, xaxis_space_left=0, xaxis_space_right=0, min_width=NULL,
                      nodes="circle", rad=0.1, label_nodes=TRUE,  node_col="grey29",  label_cex=NA, cluster_list=NULL,
                      add_normal=FALSE, normal_cex=1, title=NULL, title.cex=1.7, title.y=NULL, title.y.units="npc", ...){
    
    
    clone.out <- make_clone_tree_grobs(ccf_df=ccf_df, tree=tree_df, genes_df=genes_df,rad=rad, scale1=scale1, scale2=scale2, wid=wid, 
                                       line.lwd=line.lwd, length_from_node_edge=length_from_node_edge, seg1.col=seg1.col, seg2.col=seg2.col, 
                                       add_polygons=add_polygons, extra_len=extra_len, sig_shape=sig_shape, sig_curve=sig_curve, spread=spread,fixed_angle=fixed_angle,
                                       add_genes=add_genes, genes_on_nodes=genes_on_nodes, gene_line_dist=gene_line_dist, gene.cex=gene.cex,
                                       yaxis_position=yaxis_position, yaxis1_label=yaxis1_label, yaxis2_label=yaxis2_label,  axis_label_cex=axis_label_cex, axis_cex=axis_cex, yaxis1_interval=yaxis1_interval, yaxis2_interval=yaxis2_interval,  ylimit=ylimit, xaxis_label=xaxis_label, xaxis_space_left=xaxis_space_left, xaxis_space_right=xaxis_space_right, min_width=min_width,
                                       nodes=nodes, label_nodes=label_nodes,  node_col=node_col, label_cex=label_cex, cluster_list=cluster_list,
                                       add_normal=add_normal, normal_cex=normal_cex, title=title, title.cex=title.cex, title.y=title.y, title.y.units=title.y.units, ...
    )			

    out_tree <- gTree(children = package_clone_grobs(clone.out),
                      vp = clone.out$vp,
                      cl = "SRCGrob")
    
    return(list(out_tree, clone.out))
}