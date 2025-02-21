make.clone.polygons <- function(
    v,
    tree,
    wid,
    scale1,
    scale2,
    extra.len,
    node.col,
    spread = 1,
    fixed.angle = NULL,
    beta.in = 3,
    add.polygons = TRUE,
    no.ccf = FALSE
    ) {

  	# If the tree is linear, the width is fixed
	branching <- any(duplicated(tree$parent));
	lin.width <- wid / 3;

  	if (!branching) {
        wid <- lin.width / scale1;
        len <- extra.len + sum(tree$length1[-1]);
  	} else {
        wid <- wid / scale1;
        len <- extra.len;
  	    }

	clone.out <- compute.clones(
	    v,
	    wid = wid,
	    tree = tree,
	    extra.len = extra.len,
	    x = 0,
	    y = tree$length[tree$parent == -1],
	    fixed.angle = fixed.angle,
	    beta.in = beta.in,
	    branching = branching,
	    no.ccf = no.ccf,
	    spread = spread
	    );

  	return(clone.out);
    }

package.clone.grobs <- function(clone.out) {
	grob.list <- do.call(gList, clone.out$grobs);
	return(grob.list);
    }
