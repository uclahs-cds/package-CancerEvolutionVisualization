count.leaves.per.node <- function(v) {
	count.env <- new.env();

	v$leaves <- 0;
	leaf.nodes <- v$id[!(v$id %in% v$parent)];
	v$leaves[v$id %in% leaf.nodes] <- 1;

	assign('leaves.v', v, envir = count.env);

	count.leaves <- function(node = 1) {
		v <- get('leaves.v', envir = count.env);
		par <- v$parent[v$id == node];

		if (par != -1) {
			v <- get('leaves.v', envir = count.env);

			v$leaves[v$id == par] <- v$leaves[v$id == par] + 1;
			assign('leaves.v', v, envir = count.env);
			count.leaves(par);
		    }
	    }

	for (node in leaf.nodes) {
		count.leaves(node);
	    }

	v <- get('leaves.v', envir = count.env);
	return(v);
	}

assign.weight <- function(node,v, extra.len, spread) {
	node.weight <- v$leaves[v$id == node] / v$leaves[v$parent == -1];
	return(node.weight);
    }

position.nodes.node.radiusial <- function(v, tree, extra.len, spread = 1) {
	w <- spread * pi;
	xpos <- ypos <- 0;
	tau <- -(pi / 2.5);
	vi <- v[v$parent == -1, ];

	tree$angle <- calculate.angles.radial(v, tree, spread, abs(tau));

	preorder.traversal <- function(
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
		    angle <- tree$angle[tree$tip == vi$id & tree$parent == vi$parent];
			v$x[v$id == vi$id] <<- v$x[v$id == vi$parent] + d * sin(angle);
			v$y[v$id == vi$id] <<- v$y[v$id == vi$parent] + d * cos(angle);
		} else {
		    v$x[v$id == vi$id] <<- 0;
			v$y[v$id == vi$id] <<- d;
		    }

		eta <- tau;

		for (child in v$id[v$parent == vi$id]) {
			child.weight <- assign.weight(child, v);
			w <- child.weight * spread * pi;
			tau <- eta;
			eta <- eta + w;

			preorder.traversal(
			    node = child,
			    tree = tree,
			    w = w,
			    tau = tau,
			    eta = eta,
			    spread = spread
			    );
		    }
	     }

	preorder.traversal(
	    node = 1,
	    tree = tree,
	    w = w,
	    tau = tau,
	    spread = spread
	    );

	v <- reposition.clones(tree, v);

	v$len <- sapply(
	    v$y,
	    FUN = function(x) {
	        max(v$y) + extra.len - x;
	        }
	    );

	return(list(v = v, tree = tree));
    }
