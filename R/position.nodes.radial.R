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
	eta <- tau <- -(pi / 2.5);
	vi <- v[v$parent == -1, ];

	nodes <- c(1);

	while (length(nodes) > 0) {
	    node <- nodes[1];
	    nodes <- nodes[-1];

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
		angle <- tree$angle[child.index];

		v$x[child.index] <- parent$x + d * sin(angle);
		v$y[child.index] <- parent$y + d * cos(angle);

		eta <- tau;
		children <- v$id[v$parent == vi$id]
        num.siblings <- length(children);

        leaves.weight <- 1 / (1 + exp(-(vi$leaves - 6) ** 3));
        siblings.weight <- 1 / (1 + exp(-(num.siblings - 4)) ** 3);
        total.angle <- if (num.siblings > 1) {
            pi / 12 + pi / 6 * siblings.weight + pi / 3 * leaves.weight;
        } else {
            0;
            }
        num.sections <- num.siblings - 1;
		section.size <- total.angle / max(num.sections, 1);
		child.angles <- 0:num.sections * section.size + angle - (total.angle / 2);

		for (i in 1:length(children)) {
    		tree$angle[tree$tip == children[i]] <- child.angles[i];
	        }
		nodes <- c(children, nodes);
	}

	v$len <- sapply(
	    v$y,
	    FUN = function(x) {
	        max(v$y) + extra.len - x;
	        }
	    );

	return(list(v = v, tree = tree));
    }
