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
			v$x[v$id == vi$id] <<- v$x[v$id == vi$parent] + d * sin(tau + w / 2);
			v$y[v$id == vi$id] <<- v$y[v$id == vi$parent] + d * cos(tau + w / 2);
			tree$angle[tree$tip == vi$id & tree$parent == vi$parent] <<- tau + w / 2;
		} else {
			v$x[v$id == vi$id] <<- 0;
			v$y[v$id == vi$id] <<- d;
			tree$angle[tree$tip == vi$id & tree$parent == vi$parent] <<- 0;
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

	tree$angle[!is.na(v$angle)] <- degrees.to.radians(v$angle[!is.na(v$angle)]);

	for (i in seq_along(v$id)) {
	    vi <- v[i, ];
	    angle <- tree$angle[tree$tip == vi$id];

	    if (!is.na(vi$parent) && vi$parent == -1) {
	        x0 <- 0;
	        y0 <- tree$length[tree$parent == -1];
	        len0 <- 0;
	    } else {
	        par <- v[v$id == vi$parent, ];
	        
	        r <- tree$length[tree$tip == vi$id];
	        x.shift <- r * sin(angle);
	        x0 <- par$x + x.shift;
	        y.shift <- r * cos(angle);
	        y0 <- par$y + y.shift;
	        len0 <- par$len + y.shift;
	        }

	    v[i,]$len <- len0;
	    v[i,]$y <- y0;
	    v[i,]$x <- x0;
	    }

	v$len <- sapply(
	    v$y,
	    FUN = function(x) {
	        max(v$y) + extra.len - x;
	        }
	    );

	return(list(v = v, tree = tree));
    }
