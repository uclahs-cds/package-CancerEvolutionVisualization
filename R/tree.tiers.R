get.num.tiers <- function(tree) {
    tier.env <- new.env();
    tree$tier <- 1;
    assign('tier.tree', tree, envir = tier.env);

	get.tiers <- function(node = 1) {
		tree <- get('tier.tree', envir = tier.env);
		children <- tree$id[tree$parent == node];

		for (child in children) {
			tree <- get('tier.tree', envir = tier.env);
			tree$tier[tree$lab == child] <- tree$tier[tree$id == node] + 1;
			assign('tier.tree',tree,envir <- tier.env);
			get.tiers(child);
		    }

		return(tree);
	    }

	get.tiers();
	tier.tree <- get('tier.tree', envir = tier.env);

	n.tiers <- max(tier.tree$tier);

	return(tier.tree);
    }
