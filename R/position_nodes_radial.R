count_leaves_per_node <- function(v) {
	count_env <- new.env();

	v$leaves <- 0;
	leaf_nodes <- v$id[!(v$id %in% v$parent)];
	v$leaves[v$id %in% leaf_nodes] <- 1;

	assign("leaves_v", v, envir = count_env);

	count_leaves <- function(node = 1) {
		v <- get("leaves_v", envir = count_env);
		par <- v$parent[v$id == node];

		if (par != -1) {
			v <- get("leaves_v", envir = count_env);

			v$leaves[v$id == par] <- v$leaves[v$id == par] + 1;
			assign("leaves_v", v, envir = count_env);
			count_leaves(par);
		    }
	    }

	for (node in leaf_nodes) {
		count_leaves(node);
	    }

	v <- get("leaves_v", envir = count_env);
	return(v);
	}

assign_weight <- function(node,v, extra_len, spread) {
	node_weight <- v$leaves[v$id == node] / v$leaves[v$parent == -1];
	return(node_weight);
    }

position_nodes_radial <- function(v, tree, extra_len, spread = 1) {
	w <- spread * pi;
	xpos <- ypos <- 0;
	tau <- -(pi / 2.5);
	vi <- v[v$parent == -1, ];

	preorder_traversal <- function(
	    node = NULL,
	    tree = NULL,
	    w = NULL,
	    tau = NULL,
	    eta = NULL,
	    spread = 1
	    ) {

		vi <- v[v$id == node, ];
		d <- tree$length[tree$tip == vi$id & tree$parent == vi$parent];

		if (vi$parent != -1) {
			v$x[v$id == vi$id] <<- v$x[v$id == vi$parent] + d * sin(tau + w / 2);
			v$y[v$id == vi$id] <<- v$y[v$id == vi$parent] + d * cos(tau + w / 2);
			tree$angle[tree$tip==vi$id & tree$parent == vi$parent] <<- tau + w / 2;
		} else {
			v$x[v$id == vi$id] <<- 0;
			v$y[v$id == vi$id] <<- d;
			tree$angle[tree$tip == vi$id & tree$parent == vi$parent] <<- 0;
		    }

		eta <- tau;

		for (child in v$id[v$parent == vi$id]) {
			child_weight <- assign_weight(child, v);
			w <- child_weight * spread * pi;
			tau <- eta;
			eta <- eta + w;

			preorder_traversal(
			    node = child,
			    tree = tree,
			    w = w,
			    tau = tau,
			    eta = eta,
			    spread = spread
			    );
		    }
	     }

	preorder_traversal(
	    node = 1,
	    tree = tree,
	    w = w,
	    tau = tau,
	    spread = spread
	    );

	v$len <- sapply(
	    v$y,
	    FUN = function(x) { max(v$y) + extra_len - x }
	    );

	return(list(v = v, tree = tree));
    }