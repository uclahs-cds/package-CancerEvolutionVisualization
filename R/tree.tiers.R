get.num.tiers <- function(tree) {
    tiers <- rep(0, nrow(tree));

	get.tiers <- function(node.id) {
	    # Need to exclude NA values because comparison is bypassed
	    # NA values return NA instead, so children could be c(NA, -1)
		children <- tree$id[tree$parent == node.id & !is.na(tree$parent)];

		for (child in children) {
			tiers[tree$id == child] <<- tiers[tree$id == node.id] + 1;
			get.tiers(child);
		    }
	    }

	root.node <- '-1';
	get.tiers(root.node);

	return(tiers);
    }
