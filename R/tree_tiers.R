get_num_tiers <- function(tree) {
    tier_env <- new.env();
    tree$tier <- 1;
    assign("tier_tree", tree, envir = tier_env);

	get_tiers <- function(node = 1) {
		tree = get("tier_tree", envir = tier_env);
		children = tree$id[tree$parent == node];

		for (child in children) {
			tree = get("tier_tree", envir = tier_env);
			tree$tier[tree$lab == child] = tree$tier[tree$id == node] + 1;
			assign("tier_tree",tree,envir = tier_env);
			get_tiers(child);
		    }

		return(tree);
	    }

	get_tiers();
	tier_tree <- get("tier_tree", envir = tier_env);

	n_tiers <- max(tier_tree$tier);
	
	return(tier_tree);
    }