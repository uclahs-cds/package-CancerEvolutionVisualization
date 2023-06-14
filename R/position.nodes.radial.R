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

position.nodes.node.radiusial <- function(v, tree, extra.len, spread = 1) {
	w <- spread * pi;
	xpos <- ypos <- 0;
	tau <- -(pi / 2.5);
	vi <- v[v$parent == -1, ];

	preorder.traversal <- function(
	    node,
	    tree,
	    w,
	    tau,
	    spread,
	    eta = NULL
	    ) {

		vi <- v[v$id == node, ];
		d <- tree$length[tree$tip == vi$id & tree$parent == vi$parent];

		parent.index <- v$id == vi$parent;
        child.index <- v$id == vi$id;

        # What is tau + w / 2?
        angle <- tau + w / 2;

		if (vi$parent != -1) {
			v$x[child.index] <<- v$x[parent.index] + d * sin(angle);
			v$y[child.index] <<- v$y[parent.index] + d * cos(angle);
			tree$angle[tree$tip == vi$id & tree$parent == vi$parent] <<- angle;
		} else {
			v$x[child.index] <<- 0;
			v$y[child.index] <<- d;
			tree$angle[tree$tip == vi$id & tree$parent == vi$parent] <<- 0;
		    }

		eta <- tau;
		
		for (child in v$id[v$parent == vi$id]) {
			child.weight <- v$leaves[v$id == child] / v$leaves[v$parent == -1];
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

	v$len <- sapply(
	    v$y,
	    FUN = function(x) {
	        max(v$y) + extra.len - x;
	        }
	    );

	return(list(v = v, tree = tree));
    }
