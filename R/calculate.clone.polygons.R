f0 <- function() {
	in.env <- new.env(parent = emptyenv());
	in.env$x <- 0;

	f1 <- function(env) {
		y <- env$x;
		y <- y + 1;
		assign('x' , y, envir = env);
	    }

	f1(in.env);
    }

sigmoid <- function(params, g) {
    exp(params[1] - params[2] * g) / (1 + exp(params[1] - (g * params[2])));
    }

sigmoid.up <- function(params, g) {
    exp(params[1] + params[2] * g) / (1 + exp(params[1] + (g * params[2])));
    }

make.polygon <- function(
    x0,
    y0,
    x1,
    x2,
    sig.shape,
    wid = 1,
    len = 1,
    col = 'gray',
    beta.in = 3
    ) {

    vaf <- wid;

	beta <- len / beta.in;
	y1 <- max(y0 + beta, y0 + 1);
	yy <- seq(y0, y1, length.out = 100);

    params.d  <- c(-0.7310133, sig.shape);
	y.ex <- seq(-1, 1.5, length.out = length(yy));
	xu.2 <- sigmoid(params.d, y.ex);

	#scale and shift each sigmoid
	x.rt.d <- (xu.2 - min(xu.2)) / (max(xu.2 - min(xu.2))) * (x0 - x2) + x2;
	x.rt.u <- (-xu.2 - max(-xu.2)) / (max(xu.2 + max(-xu.2))) * (x1 - x0) + x1;

	yy.plot <- c(y0, yy, y0 + len, y0 + len, rev(yy));
	xx <- c(x0, x.rt.u, x1, x2, rev(x.rt.d));

	return(list(
	    x = xx,
	    y = yy.plot,
	    col = col
	    ));
    }

position.polygons <- function(
    clone.env,
    i,
    wid,
    x,
    y,
    len,
    beta.in = 3,
    branching = TRUE,
    fixed.angle = NULL,
    no.ccf = FALSE
    ) {

    v <- clone.env$v;
	tree <- clone.env$tree;
	clones <- clone.env$clones;

	# get the row of v that corresponds to the clone
	vi <- v[i,];
	sig.shape <- vi[['polygon.shape']];

	if (!is.na(vi$parent) && vi$parent == -1 && nrow(v[v$parent == -1, ]) == 1) {
	    # If root the clone extends the full width of the plot
		x0 <- x;
		y0 <- y;
		len0 <- len;
		x1 <- vi$x1;
		x2 <- vi$x2;
	} else {
	    # Parent not root -- not trunk clone
		if (vi$parent == -1) {
			par <- data.frame(
			    lab = -1,
			    x = 0,
			    y = 0,
			    len = len,
			    x.mid = 0,
			    x1 = min(v$x1),
			    x2 = max(v$x2)
			    );
		} else {
			par <- v[v$id == vi$parent,]; # Parent clone
		    }

	    x.mid <- vi$x.mid;
		x1 <- vi$x1;
		x2 <- vi$x2;

		siblings <- v[which(v$parent == par$id), ];

		if (nrow(siblings) == 1) {
			dist <- par$x.mid - par$x;
			parent.angle <- ifelse(
			    is.null(fixed.angle) && !no.ccf,
			    yes = atan(dist / par$len),
			    no = 0
			    );
		} else if (nrow(siblings) == 2) {
			sibling.coords <- c(siblings$x1, siblings$x2);
			x1.max <- sibling.coords[which.max(abs(sibling.coords))];
			x2.max <- sibling.coords[which.max(abs(sibling.coords - x1.max))];

			dist <- abs(x1.max - x2.max) / 2;

			if (x.mid > par$x.mid) {
				parent.angle <- ifelse(
				    is.null(fixed.angle),
				    yes = atan(dist / par$len),
				    no = fixed.angle
				    );

				parent.angle <- min(parent.angle, 40 / 180 * pi);
			} else if (x.mid < par$x.mid) {
				parent.angle <- ifelse(
				    is.null(fixed.angle),
				    yes = atan(-(dist / par$len)),
				    no = -(fixed.angle)
				    );

				parent.angle <- max(parent.angle, -40 / 180 * pi)
			} else {
				dist <- par$x.mid - par$x;
				parent.angle <- atan(dist / par$len);
			    }
		} else {
			if (vi$id == siblings$id[which.min(siblings$x.mid)]) {
			    # Align leftmost child with left outer clone border
				parent.angle <- ifelse(
				    is.null(fixed.angle),
				    yes = atan(-(abs(par$x1) / par$len)),
				    no = -(fixed.angle)
				    );
			} else if (vi$id == siblings$id[which.max(siblings$x.mid)]) {
			    # Align rightmost child with right outer clone border
				parent.angle <- ifelse(
				    is.null(fixed.angle),
				    yes = atan(abs(par$x1) / par$len),
				    no = fixed.angle
				    );
			} else{
				parent.angle <- if (par$len > 0) {
				    atan((vi$x.mid - par$x.mid) / par$len)
			    } else {
			        0
			        };
			    }
		    }

		r <- tree$length[which(tree$parent == par$id & tree$tip == vi$id)];
		x.shift <- r * sin(parent.angle);
		x0 <- par$x + x.shift;
		y.shift <- r * cos(parent.angle);
		y0 <- par$y + y.shift;
		len0 <- par$len - y.shift;

		if (par$id != -1 & len0 >= 0) {
			#make sure the node isn't outside of the parent clone
			par.coords <- data.frame(
			    x = clones[[as.integer(which(v$id == par$id))]][['x']],
			    y = clones[[as.integer(which(v$id == par$id))]][['y']]
			    );

			par$x1 <- clones[[as.integer(which(v$id == par$id))]][['x1']];
			par$x2 <- clones[[as.integer(which(v$id == par$id))]][['x2']];
			par.coords.pos <- par.coords[1:match(par$x1,par.coords$x), ];
			par.coords.neg <- par.coords[match(par$x2,par.coords$x):length(par.coords$x), ];

			match.x.pos  <- par.coords.pos$x[which.min(abs(par.coords.pos$y - y0))];
			match.x.neg  <- par.coords.neg$x[which.min(abs(par.coords.neg$y - y0))];

			while ((match.x.pos > x0 & match.x.neg > x0) | (match.x.pos < x0 & match.x.neg < x0)) {
				closer <- ifelse(match.x.pos > x0, min(match.x.pos,match.x.neg), max(match.x.pos,match.x.neg));
				further <- ifelse(match.x.pos > x0, max(match.x.pos,match.x.neg), min(match.x.pos,match.x.neg));
				x0 <- closer + 0.15 * (further - closer);
				x.shift <- x0 - par$x;
				x0 <- par$x + x.shift;
				parent.angle <- asin(x.shift / r);
				angle.x <- par$x + r * sin(parent.angle);
				y.shift <- r * cos(parent.angle);
				y0 <- par$y + y.shift
				match.x.pos  <- par.coords.pos$x[which.min(abs(par.coords.pos$y - y0))];
				match.x.neg  <- par.coords.neg$x[which.min(abs(par.coords.neg$y - y0))];
			    }
		    }

		len0 <- par$len - y.shift;
		tree$angle[which(tree$parent == par$id & tree$tip == vi$id)] <- parent.angle;
	    }

	v[i,]$len <- len0;
	v[i,]$y <- y0;
	v[i,]$x <- x0;
	clone.env$v <- v;
	clone.env$tree <- tree;

	clone.points <- make.polygon(
	    x0 = x0,
	    y0 = y0,
	    x1 = x1,
	    x2 = x2,
	    wid = wid * vi$vaf,
	    len = len0,
	    col = vi$color,
	    sig.shape = sig.shape,
	    beta.in = beta.in
	    );

 	return(c(
 	    clone.points,
 	    x0 = x0,
 	    y0 = y0,
 	    len = len0,
 	    x1 = x1,
 	    x2 = x2,
 	    alpha = vi$alpha
 	    ));
    }

get.clones <- function(
    x = 0,
    y = 0,
    wid = 1.2,
    len = len,
    beta.in = 3,
    branching = FALSE,
    no.ccf = FALSE,
    fixed.angle = NULL,
    spread = 1,
    clone.env = NULL,
    adjust.beta = FALSE
    ) {

    clone.env$clones <- list();

	clone.env$coords.df <- data.frame(
	    x0 = numeric(length = nrow(clone.env$v)),
	    y0 = numeric(length = nrow(clone.env$v)),
	    len = numeric(length = nrow(clone.env$v)),
	    x1 = numeric(length = (nrow(clone.env$v))),
	    x2 = numeric(length = nrow(clone.env$v))
	    );

	for (j in 1:(nrow(clone.env$v))) {
        clone.env$clones[[j]] <- position.polygons(
            clone.env,
            j,
            wid = wid,
            x = x,
            y = y,
            len = len,
            beta.in = beta.in,
            branching = branching,
            no.ccf = no.ccf,
            fixed.angle = fixed.angle
            );

		beta.add <- 0.5;

		if (adjust.beta & !no.ccf) {
			#if the polygon gets cut off before it can occupy the full width adjust the beta value to make it curve more sharply
			while (all(
			    clone.env$clones[[j]]$y[
			        which(abs(clone.env$clones[[j]]$x) == max(abs(clone.env$clones[[j]]$x)))
			        ] > (clone.env$coords.df$len[1] + y)
		        )) {

	            clone.env$clones[[j]] <- position.polygons(
	                clone.env,
	                j,
	                wid = wid,
	                x = x,
	                y = y,
	                len = len,
	                beta.in = beta.in + beta.add,
	                branching = branching,
	                no.ccf = no.ccf,
	                fixed.angle = fixed.angle
	                );

	            for (var in colnames(clone.env$coords.df)) {
		  		    clone.env$coords.df[j,var] <- clone.env$clones[[j]][var];
			        }

	            beta.add <- beta.add + 0.5;
			    }
		    }

		for (var in colnames(clone.env$coords.df)) {
	   		clone.env$coords.df[j, var] <- clone.env$clones[[j]][var];
    	    }
    	}
    }

compute.clones <- function(
    v,
    x = 1,
    y = 0,
    wid = 1.2,
    extra.len = 1,
    tree = NULL,
    fixed.angle = NULL,
    beta.in = 3,
    branching = TRUE,
    no.ccf = FALSE,
    spread = 1
    ) {

	# Ensure that the root is properly defined
	root <- v[!is.na(v$parent) & v$parent == -1, ];
	v <- v[is.na(v$parent) | v$parent != -1, ];
	v <- rbind(root, v);
	v <- count.leaves.per.node(v);
	if (no.ccf) {
	    tree$angle <- if ((is.null(fixed.angle) && nrow(v) > 6) || any(table(v$parent) > 2) || any(v$mode == 'dendrogram')) {
			tau <- -(pi / 2.5);
    		vi <- v[v$parent == -1, ];
    		calculate.angles.radial(v, tree, spread, abs(tau));
	    } else {
	        calculate.angles.fixed(v, tree, fixed.angle);
	        }
	    tmp <- position.nodes(v, tree, extra.len);

	    clone.env <-  new.env(parent = emptyenv());
	    clone.env$v <- tmp$v;
	    clone.env$tree <- tmp$tree;

	    return(clone.env);
	    }

	v <- position.clones(v, tree, wid);

	v$x <- v$y <- v$len <- 0;
	len <- extra.len;

	clone.env <- new.env(parent = emptyenv());
	clone.env$v <- v;
	clone.env$tree <- tree;

	get.clones(
	    x = x,
	    y = y,
	    len = len,
	    beta.in = beta.in,
	    branching = branching,
	    no.ccf = no.ccf,
	    fixed.angle = fixed.angle,
	    spread = spread,
	    clone.env = clone.env
	    );

	#if the end of the polygon is shorter than the last clone polygon or the desired length make the polygon longer and recompute
	while (max(clone.env$coords.df$y0) > (clone.env$coords.df$len[1] + y) | (min(clone.env$coords.df$len) < extra.len )) {
        len <- len + (extra.len - min(clone.env$coords.df$len)) + 0.0001;

        get.clones(
             x = x,
             y = y,
             wid = wid,
             len = len,
             beta.in = beta.in,
             branching = branching,
             no.ccf = no.ccf,
             fixed.angle = fixed.angle,
             spread = spread,
             clone.env = clone.env
             );
        }

	#if the polygon gets cut off before it can occupy the full width adjust the beta value to make it curve more sharply
	get.clones(
	    x = x,
	    y = y,
	    len = len,
	    beta.in = beta.in,
	    branching = branching,
	    no.ccf = no.ccf,
	    fixed.angle = fixed.angle,
	    spread = spread,
	    clone.env = clone.env,
	    adjust.beta = TRUE
	    );

	return(clone.env);
    }
