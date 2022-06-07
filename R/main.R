make_clone_polygons <- function(v, tree, wid, scale1, scale2, extra.len, node_col, spread=1, sig_shape=3, fixed_angle=NULL, beta_in=3, add.polygons=TRUE, no_ccf=FALSE){
  	#if the tree is linear the width is fixed
	branching <- ifelse(any(duplicated(tree$parent)== TRUE),TRUE,FALSE)
	lin.width <- wid/3

  	if(branching==FALSE){
  	  wid <-(lin.width)*1/scale1
  	  len <- extra.len + sum(tree$length1[-1])
  	}else{
  	  wid <- wid*1/scale1
  	  len <- extra.len 
  	}
  	clone_out <- compute_clones(v=v, wid=wid, tree=tree, extra.len=extra.len, x=0, y=tree$length[tree$parent==-1], fixed_angle=fixed_angle, sig_shape=sig_shape, beta_in=beta_in, branching=branching, no_ccf=no_ccf, spread=spread)
  	return(clone_out)
}

package_clone_grobs <- function(clone.out){
	grob_list <- do.call(gList, clone.out$grobs)
	return(grob_list)
}
