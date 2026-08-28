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

calculate.node.angles <- function(
    v,
    tree,
    spread,
    start.angle,
    fixed.angle,
    damping = 0
    ) {

    use.radial <- (is.null(fixed.angle) && nrow(v) > 6) ||
        any(table(v$parent) > 2) ||
        any(v$mode == 'dendrogram');

    if (use.radial) {
        tau <- -(pi / 2.5);

        return(calculate.angles.radial(
            v,
            tree,
            spread,
            abs(tau),
            start.angle = start.angle,
            damping = damping
            ));
        }

    return(calculate.angles.fixed(
        v,
        tree,
        fixed.angle,
        start.angle = start.angle,
        damping = damping
        ));
    }

get.node.radii <- function(v, node.radius) {
    # Matches adjust.branch.lengths(): the drawn node scales with "node.size",
    # and nodes that are not drawn take up no room at all.
    radii <- rep(node.radius, nrow(v));

    if ('node.size' %in% colnames(v)) {
        sized <- node.radius * as.numeric(v$node.size);
        radii[!is.na(sized)] <- sized[!is.na(sized)];
        }

    if ('draw.node' %in% colnames(v)) {
        radii[!v$draw.node] <- 0;
        }

    radii[v$id == -1] <- 0;

    return(radii);
    }

measure.node.overlap <- function(v, radii) {
    # Total linear encroachment over every node pair. Zero means no two node
    # circles touch, which is the property issue #127 is about.
    if (nrow(v) < 2) {
        return(0);
        }

    distance <- sqrt(
        outer(v$x, v$x, '-') ** 2 +
        outer(v$y, v$y, '-') ** 2
        );
    encroachment <- outer(radii, radii, '+') - distance;

    # A node that is not drawn takes up no room, so it cannot collide.
    encroachment[radii == 0, ] <- 0;
    encroachment[, radii == 0] <- 0;

    # Count each pair once.
    encroachment[!upper.tri(encroachment)] <- 0;

    return(sum(pmax(encroachment, 0)));
    }

position.nodes.without.overlap <- function(
    v,
    tree,
    extra.len,
    spread,
    start.angle,
    fixed.angle,
    node.radius = NULL
    ) {

    # Damping levels tried, in order, when nodes collide. The first entry must
    # be 0 so a layout that is already clean is returned untouched.
    damping.levels <- c(0, 0.25, 0.5, 0.75, 1);

    # Above this many nodes the pairwise overlap scan is not worth the time, and
    # a tree that large is unreadable whatever the angles.
    max.searchable.nodes <- 200;

    layout.for <- function(damping) {
        candidate.tree <- tree;
        candidate.tree$angle <- calculate.node.angles(
            v,
            tree,
            spread = spread,
            start.angle = start.angle,
            fixed.angle = fixed.angle,
            damping = damping
            );

        return(position.nodes(v, candidate.tree, extra.len, start.angle));
        }

    undamped <- layout.for(damping.levels[1]);

    search.disabled <- is.null(node.radius) ||
        !is.finite(node.radius) ||
        node.radius <= 0 ||
        nrow(v) > max.searchable.nodes;

    if (search.disabled) {
        return(undamped);
        }

    radii <- get.node.radii(v, node.radius);

    best <- list(
        layout = undamped,
        overlap = measure.node.overlap(undamped$v, radii)
        );

    # Child angles are measured from the parent branch, so without damping they
    # accumulate and mirrored paths can put two nodes on the exact same spot
    # (issue #127). Damping is only worth applying when it actually buys space,
    # so keep the least-damped layout that improves on the default.
    for (damping in damping.levels[-1]) {
        if (best$overlap == 0) {
            break;
            }

        candidate <- layout.for(damping);
        overlap <- measure.node.overlap(candidate$v, radii);

        if (overlap < best$overlap) {
            best <- list(layout = candidate, overlap = overlap);
            }
        }

    return(best$layout);
    }
