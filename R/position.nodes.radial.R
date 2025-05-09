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
	node.weight <- (v$leaves[v$id == node] - 1) / v$leaves[v$parent == -1];
	return(node.weight);
    }

position.nodes <- function(v, tree, extra.len, start.angle) {
	xpos <- ypos <- 0;
	vi <- v[v$parent == -1, ];

	preorder.traversal <- function(node, tree) {
		vi <- v[v$id == node, ];
		distance <- tree$length[tree$tip == vi$id & tree$parent == vi$parent];
		angle <- tree$angle[tree$tip == vi$id & tree$parent == vi$parent];

		if (vi$mode == 'radial') {
		    dx <- distance * sin(angle);
		    dy <- distance * cos(angle);
		} else {
		    # Dendrogram
		    x.length <- vi$x.length;
		    dx <- if (is.na(x.length)) distance * tan(angle) else x.length;
		    dy <- distance;
			new.d <- rotate.coords(dx, dy, rotate.by = start.angle);
			dx <- new.d$x;
			dy <- new.d$y;
	        }

		if (vi$parent != -1) {
			v$x[v$id == vi$id] <<- v$x[v$id == vi$parent] + dx;
			v$y[v$id == vi$id] <<- v$y[v$id == vi$parent] + dy;
		} else {
		    v$x[v$id == vi$id] <<- dx;
			v$y[v$id == vi$id] <<- dy;
		    }
		for (child in v$id[v$parent == vi$id]) {
			preorder.traversal(node = child, tree = tree);
		    }
	     }

	preorder.traversal(node = 1, tree = tree);

	v$len <- sapply(
	    v$y,
	    FUN = function(x) {
	        max(v$y) + extra.len - x;
	        }
	    );

	return(list(v = v, tree = tree));
    }
