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

position.nodes.node.radiusial <- function(v, tree, extra.len) {
	xpos <- ypos <- 0;
	tau <- -(pi / 2.5);
	vi <- v[v$parent == -1, ];

	preorder.traversal <- function(
	    node,
	    tree,
	    tau,
	    angle = 0,
	    eta = NULL
	    ) {

		vi <- v[v$id == node, ];
		d <- tree$length[tree$tip == vi$id & tree$parent == vi$parent];

		parent.index <- if (vi$parent != -1) {
		    i <- which(v$id == vi$parent);

		    if (length(i) < 1) {
		        stop(paste('No parent found for node', '"v$id"'));
		        }
            i;
        } else {
	            NULL;
	        }

		child.index <- which(v$id == vi$id);
		parent <- if (!is.null(parent.index)) v[parent.index, ] else list(x = 0, y = 0);

        # Angle in radians
	    tree[tree$tip == vi$id & tree$parent == vi$parent, 'angle'] <<- angle;

		v$x[child.index] <<- parent$x + d * sin(angle);
		v$y[child.index] <<- parent$y + d * cos(angle);

		eta <- tau;
		children <- v$id[v$parent == vi$id]

		for (child in children) {
		    child.node <- v[v$id == child, ];
			child.weight <- child.node$leaves / v$leaves[v$parent == -1];
			w <- child.weight * child.node$spread * pi;
			tau <- eta;
			eta <- eta + w;
			
			if (length(children) > 1) {
			    angle <- tau + w / 2;
			    }

			preorder.traversal(
			    node = child,
			    tree = tree,
			    tau = tau,
			    eta = eta,
			    angle = angle
			    );
		    }
	     }

	preorder.traversal(
	    node = 1,
	    tree = tree,
	    tau = tau
	    );

	v$len <- sapply(
	    v$y,
	    FUN = function(x) {
	        max(v$y) + extra.len - x;
	        }
	    );

	return(list(v = v, tree = tree));
    }
